package chicala

sealed abstract class Data

case class UInt(val value: BigInt, val width: BigInt) extends Data {
  require(0 <= width)
  require(0 <= value && value < Pow2(width))

  def apply(idx: BigInt): Bool = {
    require(0 <= idx && idx < width)
    Bool((value / Pow2(idx)) % 2 == 1)
  }
  def apply(left: BigInt, right: BigInt): UInt = {
    require(left >= right)
    UInt((value / Pow2(right)) % Pow2(left), left - right + 1)
  }

  // Unary

  def unary_- : UInt = {
    UInt(Pow2(width) - value, width)
  }

  // Binary

  def +(that: UInt): UInt = {
    val carryed  = this.value + that.value
    val newWidth = if (this.width > that.width) this.width else that.width
    val limt     = Pow2(newWidth)

    UInt(
      if (carryed > limt) carryed - limt else carryed,
      newWidth
    )
  }
  def -(that: UInt): UInt = {
    UInt(
      this.value - that.value,
      (if (this.width > that.width) this.width else that.width) + 1
    )
  }
  def <<(that: UInt): UInt = {
    UInt(this.value * Pow2(that.value), this.width + Pow2(that.width) - 1)
  }

  // Binary compire
  def ===(that: UInt): Bool = {
    Bool(this.value == that.value)
  }
  def >=(that: UInt): Bool = {
    Bool(this.value >= that.value)
  }
}
object UInt {
  def empty(width: BigInt): UInt = {
    UInt(BigInt(0), width)
  }
}

case class Bool(val value: Boolean) extends Data {
  def unary_! : Bool = {
    Bool(!value)
  }

  def ^(that: Bool): Bool = {
    Bool(this.value ^ that.value)
  }
  def &&(that: Bool): Bool = {
    Bool(this.value && that.value)
  }
}
object Bool {
  def empty(): Bool = {
    Bool(false)
  }
}

case class Lit(value: BigInt, width: BigInt) {
  def U: UInt = UInt(value, width)
}
object Lit {
  def apply(value: BigInt): Lit = {
    Lit(value, log2Ceil(value + 1))
  }
}
