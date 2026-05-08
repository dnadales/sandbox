module datatypes {
  // Type aliases give names to types
  type Slot = int
  type BlockNo = int
  type Hash = str

  // Records: named fields (like Haskell records or structs)
  type Block = {
    slot: Slot,
    blockNo: BlockNo,
    prevHash: Hash,
    hash: Hash,
  }

  val genesisBlock: Block = {
    slot: 0,
    blockNo: 0,
    prevHash: "none",
    hash: "genesis",
  }

  // Accessing fields: dot notation
  pure def slotOf(b: Block): Slot = b.slot

  // Updating a field: the `with` operator (returns a new record)
  pure def advanceSlot(b: Block, newSlot: Slot): Block = {
    b.with("slot", newSlot)
  }

  // Tuples: positional, accessed by index (_1, _2, ...)
  type SlotAndHash = (Slot, Hash)

  val example: SlotAndHash = (42, "abc123")

  pure def getSlot(sh: SlotAndHash): Slot = sh._1
  pure def getHash(sh: SlotAndHash): Hash = sh._2
}
