package chicala

object ChicalaData {
  import ChicalaUtil._

  sealed abstract class Direction
  case object Input  extends Direction
  case object Output extends Direction

  sealed abstract class PhysicalType
  case class Io(direction: Direction) extends PhysicalType
  case object Wire                    extends PhysicalType
  case object Reg                     extends PhysicalType
  case object Node                    extends PhysicalType

  sealed abstract class Data

  case class UInt(var value: BigInt, val width: BigInt, val ptype: PhysicalType = Node, var next: BigInt = BigInt(0))
      extends Data {

    def clock(): Unit = {
      value = next
    }

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
      // TODO
      this
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

    def :=(that: UInt): Unit = {
      ptype match {
        case Reg => this.next = that.value
        case Io(dir) =>
          dir match {
            case Input  => assert(false)
            case Output => this.value = that.value
          }
        case Wire => this.value = that.value
        case Node => assert(false)
      }
    }
  }

  case class Bool(var value: Boolean, val ptype: PhysicalType = Node, var next: Boolean = false) extends Data {
    def clock(): Unit = {
      value = next
    }

    def unary_! : Bool = {
      Bool(!value)
    }

    def ^(that: Bool): Bool = {
      Bool(this.value ^ that.value)
    }
    def &&(that: Bool): Bool = {
      Bool(this.value && that.value)
    }

    def :=(that: Bool): Unit = {
      this.value = that.value
    }
  }

  case class Vec[T <: Data](val array: Array[T], val length: Int) extends Data {
    // TODO: array need a Int index, this may couse problem using UInt to index
    def apply(idx: Int): T = {
      array(idx)
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
}
