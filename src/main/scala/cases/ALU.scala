package cases

import chicala._

object Adder {
  import ChicalaData._
  import ChicalaUtil._

  case class AdderIo(
      val valid: Bool,
      val in1: UInt,
      val in2: UInt,
      val out: UInt
  )

  def adderTrans(width: BigInt)(io_valid: Bool, io_in1: UInt, io_in2: UInt) = {
    var io_out = UInt.empty(width)
    if (when(io_valid)) {
      io_out = io_in1 + io_in2
    } else {
      io_out = Lit(BigInt(0)).U
    }
    io_out
  } ensuring (io_out => if (io_valid.value) io_out.value == io_in1.value + io_in2.value else io_out.value == BigInt(0))
}
