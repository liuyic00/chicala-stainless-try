package chicala

object max {
  def apply(a: BigInt, b: BigInt): BigInt = {
    if (a > b) a else b
  }
}

object bitLength {
  def apply(x: BigInt): BigInt = {
    def f(base: BigInt, res: BigInt): BigInt = {
      if (res > 0) {
        f(base + 1, res / 2)
      } else {
        base
      }
    }
    f(0, x)
  }
}
object log2Ceil {
  def apply(x: BigInt): BigInt = {
    require(x > 0)
    bitLength(x - 1)
  }
}

object Mux {
  // Cannot instantiate a non-mutable function type parameter T <: Data
  // so apply all the dataType
  def apply(cond: Bool, con: UInt, alt: UInt): UInt = {
    if (cond.value) con else alt
  }
}
object Cat {
  def apply(left: UInt, right: UInt): UInt = {
    // `<<` need int param, use `Pow2` hear
    UInt(
      (left.value * Pow2(right.width)) + right.value,
      left.width + right.width
    )
  }
  def apply(left: UInt, right: Bool): UInt = {
    // `<<` need int param, use `Pow2` hear
    UInt((left.value * 2) + (if (right.value) 1 else 0), left.width + 1)
  }
}

object Pow2 {
  def apply(p: Int): BigInt = {
    // Only literal arguments are allowed for BigInt.
    // can't cast Int to BigInt
    def f(base: BigInt, p: Int): BigInt = {
      if (p > 0) {
        f(base * 2, p - 1)
      } else {
        base
      }
    }
    f(BigInt(1), p)
  }
  def apply(p: BigInt): BigInt = {
    def f(base: BigInt, p: BigInt): BigInt = {
      if (p > 0) {
        f(base * 2, p - 1)
      } else {
        base
      }
    }
    f(BigInt(1), p)
  }
}

object Log2 {
  def apply(x: UInt): UInt = {
    val log2 = bitLength(x.value) - 1
    UInt(log2, bitLength(log2))
  }
}

object when {
  def apply(x: Bool): Boolean = {
    x.value
  }
}
