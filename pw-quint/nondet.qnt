// Simulate:    quint run --max-steps=20 --invariant=totalPreserved nondet.qnt
// Model check: quint verify --max-steps=5 --invariant=totalPreserved nondet.qnt
//
// Controlling exploration:
//   --max-steps=N    how many transitions deep (default 10)
//   --verbosity=3    show detailed progress per step
//   State space is controlled by the spec itself: the sets used in nondet.
//   e.g. 1.to(100) = 100 choices; shrink to 1.to(10) for faster verification.
//
// quint run:    fast, probabilistic (random sampling)
// quint verify: exhaustive but bounded (proves no violation within --max-steps)
//
// Performance finding: quint verify with maps + nondet doesn't scale past
// ~6-7 steps even with a tiny state space (3 addresses, amounts 1-3).
// The SMT encoding of chained map get/set operations grows exponentially.
// Using sum types instead of str for map keys helps somewhat but doesn't
// solve the fundamental scaling issue. Keep --max-steps<=5 for verify.
module bank {
  // Use a sum type instead of str for addresses. With str, the SMT solver
  // reasons about arbitrary string-keyed maps (unbounded domain), which
  // makes verification intractable. A sum type makes the domain finite and
  // explicit to the solver.
  type Address = Alice | Bob | Carol
  type Balance = int

  val addresses = Set(Alice, Bob, Carol)

  var balances: Address -> Balance

  action init = {
    balances' = Map(Alice -> 100, Bob -> 50, Carol -> 0)
  }

  action transfer(sender: Address, receiver: Address, amount: Balance): bool = all {
    balances.get(sender) >= amount,
    amount > 0,
    sender != receiver,
    balances' = balances.set(sender, balances.get(sender) - amount)
                        .set(receiver, balances.get(receiver) + amount),
  }

  // nondet: pick an arbitrary value from a set
  // The model checker explores ALL possible choices.
  // The simulator picks one at random.
  action step = {
    nondet sender = addresses.oneOf() //nondet x = someSet.oneOf() — picks an arbitrary element
    nondet receiver = addresses.oneOf()
    nondet amount = 1.to(3).oneOf() // 1.to(100) — the integer range {1, 2, ..., 100} as a set
    transfer(sender, receiver, amount)
  }
  // Compare this with:
  //   action step = {
  //   // any { ... } means one of the actions fires (nondeterministic choice)
  //   any {
  //     transfer("alice", "bob", 10),
  //     transfer("bob", "carol", 5),
  //     transfer("alice", "carol", 20),
  //     transfer("carol", "alice", 15),
  //   }
  // }

  val totalPreserved = balances.get(Alice) + balances.get(Bob) + balances.get(Carol) == 150
}
