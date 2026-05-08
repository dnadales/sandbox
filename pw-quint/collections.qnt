module collections {
  // Sets: unordered, no duplicates
  val primes = Set(2, 3, 5, 7, 11)
  val empty: Set[int] = Set()

  // Set operations
  val has7 = primes.contains(7)
  val has4 = primes.contains(4)
  val moreprimes = primes.union(Set(13, 17))
  val small = primes.filter(p => p < 6)
  val doubled = primes.map(p => p * 2)

  // Set cardinality and membership
  val count = primes.size()
  val allPositive = primes.forall(p => p > 0)
  val hasEven = primes.exists(p => p % 2 == 0)

  // Maps: key-value pairs (like Haskell's Map)
  type Address = str
  type Balance = int

  val balances: Address -> Balance = Map(
    "alice" -> 100,
    "bob" -> 50,
    "carol" -> 0,
  )

  // Map access
  val aliceBal = balances.get("alice")

  // Map update (returns new map)
  val afterDeposit = balances.set("bob", 75)

  // Map keys
  val accounts = balances.keys()

  // Combining: a set of all accounts with positive balance
  val funded = accounts.filter(a => balances.get(a) > 0)
}
