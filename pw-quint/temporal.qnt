// REPL:        quint -r temporal.qnt::temporal
// Simulate:    quint run --max-steps=20 --invariant=alwaysNonNeg temporal.qnt
// Model check: quint verify --temporal=alwaysNonNeg temporal.qnt
module _temporal {
  var count: int
  var done: bool

  action init = all {
    count' = 0,
    done' = false,
  }

  action increment = all {
    not(done),
    count' = count + 1,
    done' = count + 1 >= 5,
  }

  action stutter = all {
    // Do nothing — models the system idling
    count' = count,
    done' = done,
  }

  action step = any {
    increment,
    stutter,
  }

  // "temporal" declares a trace property (vs "val" for single-state properties).
  // Only quint verify checks these, not the simulator.

  // Safety: always(P) — P must hold in every state of every trace
  temporal alwaysNonNeg = always(count >= 0)

  // Liveness: eventually(P) — P must become true at some point in every trace.
  // This will FAIL because stutter can loop forever without reaching done.
  temporal eventuallyDone = eventually(done)

  // weakFair(A, vars) corresponds to WF_vars(A) in TLA+.
  //   "if action A is CONTINUOUSLY enabled, a step that changes vars via A
  //   must eventually happen."
  // The second argument (count, done) is the tuple of state variables that
  // the action modifies. It tells the model checker how to distinguish an
  // increment step from a stutter step — did (count, done) actually change?
  // With this assumption, the system can't idle forever while increment is
  // enabled, so eventually(done) holds.
  //
  // Refs:
  //   https://quint-lang.org/docs/lang
  //   https://github.com/informalsystems/quint/blob/main/docs/content/docs/builtin.md
  temporal eventuallyDoneWF =
    weakFair(increment, (count, done)) implies eventually(done)

  // strongFair(A, vars) corresponds to SF_vars(A) in TLA+.
  //   "if action A is enabled INFINITELY OFTEN, a step that changes vars
  //   via A must happen infinitely often."
  // Stronger assumption than weak fairness: even if the action's enabledness
  // flickers on and off, it still fires. In this spec both have the same
  // effect because increment stays continuously enabled until done becomes true.
  temporal eventuallyDoneSF =
    strongFair(increment, (count, done)) implies eventually(done)

  // Quint supports Lamport's Temporal Logic of Actions (TLA).
  // Available temporal operators:
  //   always(P)          — □P, P holds in every state
  //   eventually(P)      — ◇P, P holds in some future state
  //   weakFair(A, v)     — WF_v(A)
  //   strongFair(A, v)   — SF_v(A)
  //   implies, and, or, not — standard boolean connectives
  //
  // These compose freely:
  //   always(P implies eventually(Q))  — "P leads to Q"
  //   always(eventually(P))            — "P holds infinitely often"
  //   eventually(always(P))            — "P eventually holds forever"
  //
  // TLA deliberately omits "next" and "until" (unlike CTL/LTL) to preserve
  // stuttering invariance, which is what makes refinement between abstraction
  // levels work.

  // Composed example: under weak fairness, count > 0 leads to done.
  // Whenever count > 0, the system must eventually reach done.
  temporal countLeadsToDone =
    weakFair(increment, (count, done)) implies
      always(count > 0 implies eventually(done))
}
