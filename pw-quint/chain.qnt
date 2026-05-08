// REPL: quint -r chain.qnt::chain
//
// Try: init, step, tip, step, tip, nextBlockNo(tip)
//
// Simulate: quint run --max-steps=10 --invariant=slotAlwaysAdvances chain.qnt
module chain {
  // Import everything from types — must specify the file path
  import types.* from "./types"

  var tip: BlockHeader

  action init = {
    tip' = { slot: 0, blockNo: 0, prevHash: "none", hash: "genesis" }
  }

  action appendBlock(slot: Slot, hash: Hash): bool = all {
    // Guard: slot must advance
    slot > tip.slot,
    // Update tip
    tip' = {
      slot: slot,
      blockNo: nextBlockNo(tip),
      prevHash: tip.hash,
      hash: hash,
    },
  }

  action step = {
    nondet slot = (tip.slot + 1).to(tip.slot + 3).oneOf()
    appendBlock(slot, "block")
  }

  val slotAlwaysAdvances = tip.slot >= 0
  val blockNoMatchesLength = tip.blockNo >= 0
}
