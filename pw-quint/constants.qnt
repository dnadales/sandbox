// REPL: quint -r constants.qnt::kes
// Simulate: quint run --max-steps=10 --invariant=periodInRange constants.qnt
module kes {
  // const: a module parameter, fixed per instantiation
  //
  // Example instatiation (see below)
  //
  //  kes(MaxKESEvolutions = 62, SlotsPerKESPeriod = 129600).*
  const MaxKESEvolutions: int
  const SlotsPerKESPeriod: int

  // assume: precondition on constants — checked at instantiation time
  //
  // You can't use assume on vars or other state; for those, you use invariants (val) or guards in
  // actions.
  assume validConfig = and {
    MaxKESEvolutions > 0,
    SlotsPerKESPeriod > 0,
  }

  pure def kesPeriodOf(slot: int): int = slot / SlotsPerKESPeriod

  pure def validKesPeriod(slot: int, startPeriod: int): bool = and {
    startPeriod <= kesPeriodOf(slot),
    kesPeriodOf(slot) < startPeriod + MaxKESEvolutions,
  }

  var currentSlot: int

  action init = { currentSlot' = 0 }

  action advanceSlot = {
    nondet delta = 1.to(SlotsPerKESPeriod).oneOf()
    currentSlot' = currentSlot + delta
  }

  action step = advanceSlot

  val periodInRange = kesPeriodOf(currentSlot) >= 0
}

// Instantiate with concrete values
//
// The simulator/verifier needs a concrete module — use kesTest not kes
module kesTest {
  import kes(MaxKESEvolutions = 62, SlotsPerKESPeriod = 129600).*
}
