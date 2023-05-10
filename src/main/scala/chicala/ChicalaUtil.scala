package chicala

import stainless.collection._

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
  def apply[T <: Bits](cond: Bool, con: T, alt: T): T = {
    if (cond.value) con else alt
  }
}
object Cat {
  def apply(left: Bits, right: Bits): UInt = {
    val l = left.asUInt
    val r = right.asUInt
    UInt(
      (l.value * Pow2(r.width)) + r.value,
      l.width + r.width
    )
  }
  // `[T <: Bits]` then `List[T]` is not supported
  def apply(ls: List[Bits]): UInt = {
    ls.tail.foldLeft(ls.head.asUInt) { case (res, r) => Cat(res, r) }
  }
}
object Fill {
  def apply(times: BigInt, bool: Bool): UInt = {
    require(times > 0)
    def f(result: UInt, times: BigInt): UInt = {
      if (times > 0)
        f(Cat(result, bool), times - 1)
      else
        result
    }
    f(bool.asUInt, times - 1)
  }
}

object MuxLookup {
  def apply[T <: Bits](key: UInt, default: T, mapping: List[(UInt, T)]): T = {
    mapping.foldLeft(default) { case (res, (k, v)) => Mux(k === key, v, res) }
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
