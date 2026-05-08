// REPL:        quint -r statemachine.qnt::statemachine
// Simulate:    quint run --max-steps=20 --invariant=totalPreserved statemachine.qnt
// Model check: quint verify --invariant=totalPreserved statemachine.qnt
module statemachine {
  type Address = str
  type Balance = int

  // var declares mutable state
  var balances: Address -> Balance
  var totalTransfers: int

  // init must assign every var via primes
  action init = all {
    // x' (prime) means "the value of x in the next state"
    balances' = Map("alice" -> 100, "bob" -> 50, "carol" -> 0),
    totalTransfers' = 0,
  }

  // An action that changes state
  //
  // Actions in Quint are atomic — all the primed assignments in a
  // single action happen simultaneously as one state transition. So,
  // for instance, there's no way to observe an intermediate state
  // where the sender is debited but the receiver isn't yet credited.
  action transfer(sender: Address, receiver: Address, amount: Balance): bool = all {
    // all { ... } means all conditions/updates must hold (conjunction)
    // Guards: conditions that must hold for the action to fire
    balances.get(sender) >= amount,
    amount > 0,
    sender != receiver,
    // State updates
    balances' = balances.set(sender, balances.get(sender) - amount)
                        .set(receiver, balances.get(receiver) + amount),

    // Every action must assign every var — if unchanged, write x' = x
    totalTransfers' = totalTransfers + 1,
  }

  // step: the transition relation — one of these happens each step
  action step = {
    // any { ... } means one of the actions fires (nondeterministic choice)
    any {
      transfer("alice", "bob", 10),
      transfer("bob", "carol", 5),
      transfer("alice", "carol", 20),
      transfer("carol", "alice", 15),
    }
  }

  // Invariants: properties that must hold in every reachable state
  val noNegativeBalances = balances.keys().forall(a => balances.get(a) >= 0)
  val totalPreserved = balances.get("alice") + balances.get("bob") + balances.get("carol") == 150
}
