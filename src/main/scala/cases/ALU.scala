package cases

import chicala._

object Adder {
  import ChicalaModule._
  import ChicalaData._
  import ChicalaUtil._

  case class AdderIo(
      val valid: Bool,
      val in1: UInt,
      val in2: UInt,
      val out: UInt
  )

  def adderTrans(width: Int, io: AdderIo): Unit = {
    if (io.valid.value) {
      io.out := io.in1 + io.in2
    } else {
      io.out := UInt(BigInt(0), 1) // 1.2.1
    }
  } ensuring (if (io.valid.value) io.out.value == io.in1.value + io.in2.value else io.out.value == 0)
}
