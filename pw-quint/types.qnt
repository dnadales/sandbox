// Shared type definitions — no state, just types and pure functions
module types {
  type Slot = int
  type BlockNo = int
  type Hash = str

  type BlockHeader = {
    slot: Slot,
    blockNo: BlockNo,
    prevHash: Hash,
    hash: Hash,
  }

  pure def nextBlockNo(b: BlockHeader): BlockNo = b.blockNo + 1
}
