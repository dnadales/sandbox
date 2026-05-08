module basics {
  // val: a named constant
  val greeting = "hello"
  val maxSlot = 100

  // pure def: a function — takes arguments, returns a value
  pure def double(x: int): int = x * 2
  pure def isPositive(x: int): bool = x > 0

  // Quint has the usual types: int, str, bool
  // Type annotations are optional but good practice
  pure def clamp(x: int, lo: int, hi: int): int = {
    if (x < lo) lo
    else if (x > hi) hi
    else x
  }
}
