package cases

import stainless.collection._

import chicala._

object SignExt {
  def apply(a: UInt, len: BigInt): UInt = {
    val aLen    = a.getWidth
    val signBit = a(aLen - 1)
    if (aLen >= len) a(len - 1, 0) else Cat(Fill(len - aLen, signBit), a)
  }
}

case class ArrayMulDataModuleInputs(
    val io_a: UInt,
    val io_b: UInt,
    val io_regEnables: Array[Bool]
)
case class ArrayMulDataModuleOutputs(
    val result: UInt
)

case class ArrayMulDataModule(len: BigInt) {
  def inputsRequire(inputs: ArrayMulDataModuleInputs): Boolean = inputs match {
    case ArrayMulDataModuleInputs(io_a, io_b, io_regEnables) =>
      io_a.width == len &&
      io_b.width == len &&
      io_regEnables.length == 2
  }
  def outputsRequire(outputs: ArrayMulDataModuleOutputs): Boolean = outputs match {
    case ArrayMulDataModuleOutputs(result) =>
      result.width == 2 * len
  }
  def trans(
      inputs: ArrayMulDataModuleInputs
  ): ArrayMulDataModuleOutputs = {
    require(inputsRequire(inputs))

    inputs match {
      case ArrayMulDataModuleInputs(io_a, io_b, io_regEnables) =>
        // output
        var io_result: UInt = UInt.empty(len * 2)
        // reg next

        // begin

        val (a, b) = (io_a, io_b)

        var b_sext, bx2, neg_b, neg_bx2 = UInt.empty(len + 1)
        b_sext = SignExt(b, len + 1)
        bx2 = b_sext << 1
        neg_b = ~b_sext
        neg_bx2 = neg_b << 1

        var columns: List[List[Bool]] = List.fill(2 * len)(List())

        var last_x = Lit(0, 3).U

        var i = BigInt(0)
        while (i < len) {
          val x =
            if (i == 0) Cat(a(1, 0), Lit(0, 1).U)
            else if (i + 1 == len) SignExt(a(i, i - 1), 3)
            else a(i + 1, i - 1)
          val pp_temp = MuxLookup(
            x,
            Lit(0).U,
            List(
              Lit(1).U -> b_sext,
              Lit(2).U -> b_sext,
              Lit(3).U -> bx2,
              Lit(4).U -> neg_bx2,
              Lit(5).U -> neg_b,
              Lit(6).U -> neg_b
            )
          )
          val s = pp_temp(len)
          val t = MuxLookup(
            last_x,
            Lit(0, 2).U,
            List(
              Lit(4).U -> Lit(2, 2).U,
              Lit(5).U -> Lit(1, 2).U,
              Lit(6).U -> Lit(1, 2).U
            )
          )

          last_x = x
          val (pp, weight) = {
            if (i == 0)
              (Cat(List(~s, s, s, pp_temp).map(_.asUInt)), BigInt(0))
            else if (i == len - 1 || i == len - 2)
              (Cat(List(~s, pp_temp, t).map(_.asUInt)), i - 2)
            else
              (Cat(List(Lit(1, 1).U, ~s, pp_temp, t).map(_.asUInt)), i - 2)
          }

          var newColumns: List[List[Bool]] = List.empty
          var j: BigInt                    = 0
          while (columns.nonEmpty) {
            if (j >= weight && j < (weight + pp.getWidth))
              newColumns = newColumns :+ (columns.head :+ pp(j - weight))
            else
              newColumns = newColumns :+ columns.head
            j = j + 1
            columns = columns.tail
          }
          columns = newColumns

          i = i + 2
        }

        def addOneColumn(col: List[Bool], cin: List[Bool]): (List[Bool], List[Bool], List[Bool]) = {
          var sum   = List[Bool]()
          var cout1 = List[Bool]()
          var cout2 = List[Bool]()

          if (col.size == 1) {
            sum = col ++ cin
          } else if (col.size == 2) {
            val c22 = C22()

            var c22_io_in   = col.map(_.asUInt)
            val c22_outputs = c22.trans(CarrySaveAdderMToN_Inputs(c22_io_in))
            val c22_io_out  = c22_outputs.io_out

            sum = c22_io_out(0).asBool :: cin
            cout2 = List(c22_io_out(1).asBool)
          } else if (col.size == 3) {
            val c32 = C32()

            var c32_io_in   = col.map(_.asUInt)
            val c32_outputs = c32.trans(CarrySaveAdderMToN_Inputs(c32_io_in))
            val c32_io_out  = c32_outputs.io_out

            sum = c32_io_out(0).asBool :: cin
            cout2 = List(c32_io_out(1).asBool)
          } else if (col.size == 4) {
            val c53 = C53()

            var c53_io_in   = col.take(4).map(_.asUInt) :+ (if (cin.nonEmpty) cin.head.asUInt else Lit(0).U)
            val c53_outputs = c53.trans(CarrySaveAdderMToN_Inputs(c53_io_in))
            val c53_io_out  = c53_outputs.io_out

            sum = List(c53_io_out(0).asBool) ++ (if (cin.nonEmpty) cin.drop(1) else Nil[Bool]())
            cout1 = List(c53_io_out(1).asBool)
            cout2 = List(c53_io_out(2).asBool)
          } else {
            val cin_1               = if (cin.nonEmpty) List(cin.head) else Nil[Bool]()
            val cin_2               = if (cin.nonEmpty) cin.drop(1) else Nil[Bool]()
            val (s_1, c_1_1, c_1_2) = addOneColumn(col take 4, cin_1)
            val (s_2, c_2_1, c_2_2) = addOneColumn(col drop 4, cin_2)
            sum = s_1 ++ s_2
            cout1 = c_1_1 ++ c_2_1
            cout2 = c_1_2 ++ c_2_2
          }

          (sum, cout1, cout2)
        }

        def max(in: List[BigInt]): BigInt = chicala.max(in)

        def addAll(cols: List[List[Bool]], depth: BigInt): (UInt, UInt) = {
          if (max(cols.map(_.size)) <= 2) {
            val sum = Cat(
              cols.map(_(0)).reverse.map(_.asUInt)
            )
            var k = BigInt(0)
            while (cols(k).size == 1) k = k + 1
            val carry = Cat(cols.drop(k).map(_(1)).reverse.map(_.asUInt))
            (sum, Cat(carry, Lit(0, k).U))
          } else {
            var columns_next = List.fill(2 * len)(List[Bool]())
            var cout1, cout2 = List[Bool]()

            columns_next = cols
              .foldLeft((cout1, cout2, List.empty[List[Bool]])) { case ((cout1, cout2, res), cols_i) =>
                val (s, c1, c2)    = addOneColumn(cols_i, cout1)
                val columns_next_i = s ++ cout2
                (c1, c2, res :+ columns_next_i)
              }
              ._3

            val toNextLayer = columns_next

            addAll(toNextLayer, depth + 1)
          }
        }

        val (sum, carry) = addAll(cols = columns, depth = 0)

        io_result = sum + carry

        // end
        ArrayMulDataModuleOutputs(io_result)
    }
  } ensuring (outputs => outputsRequire(outputs))
}
