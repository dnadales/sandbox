// REPL: quint -r instance.qnt::twoCounters
// Simulate: quint run --max-steps=10 --invariant=bothInRange instance.qnt
module boundedCounter {
  const Max: int
  const Name: str

  assume validMax = Max > 0

  var count: int

  action init = { count' = 0 }

  action increment = all {
    count < Max,
    count' = count + 1,
  }

  action reset = { count' = 0 }

  action step = any { increment, reset }

  val inRange = count >= 0 and count <= Max
}

// instance creates an independent copy of the module with its own state.
// Each instance has separate state variables — changing one doesn't affect
// the other. Compare with import, which shares definitions but not state.
module twoCounters {
  instance slow = boundedCounter(Max = 3, Name = "slow")
  instance fast = boundedCounter(Max = 10, Name = "fast")

  // Access instance state via the instance name
  action init = all {
    slow::init,
    fast::init,
  }

  action step = any {
    // Every action must assign every state variable in the system.
    // slow::step only assigns slow::count', so we must explicitly say
    // what happens to fast::count — otherwise Quint errors on the
    // unassigned variable. x' = x means "unchanged this step."
    all { slow::step, fast::count' = fast::count },
    all { fast::step, slow::count' = slow::count },
  }

  val bothInRange = slow::inRange and fast::inRange
}
