// REPL: quint -r variants.qnt::variants
module variants {
  // Sum type: each variant has a label and a payload
  type ChainTip =
    | Genesis
    | BlockTip({ slot: int, blockNo: int, hash: str })

  // Construct variants
  val tip1: ChainTip = Genesis
  val tip2: ChainTip = BlockTip({ slot: 42, blockNo: 5, hash: "abc" })

  // Pattern match with match ... { ... }
  pure def slotOf(tip: ChainTip): int = match tip {
    | Genesis => -1
    | BlockTip(b) => b.slot
  }

  // Variant with no payload uses a unit value
  type Result =
    | Ok(int)
    | Err(str)

  pure def describe(r: Result): str = match r {
    | Ok(n) => "success"
    | Err(msg) => msg
  }

  // Option is not built-in but easy to define
  type Option = Some(int) | None

  pure def withDefault(o: Option, d: int): int = match o {
    | Some(v) => v
    | None => d
  }
}
