package cases

import chicala._

object Divider {
  import ChicalaModule._
  import ChicalaData._
  import ChicalaUtil._

  case class DividerIo_in(
      val ready: Bool,
      val valid: Bool,
      val bits: Array[UInt]
  ) {
    def isIos: Boolean = {
      (ready.ptype match {
        case Io(Output) => true
        case _          => false
      }) &&
      (valid.ptype match {
        case Io(Input) => true
        case _         => false
      }) && ({
        (bits(0) match {
          case x: UInt =>
            x.ptype match {
              case Io(Input) => true
              case _         => false
            }
        }) && (bits(1) match {
          case x: UInt =>
            x.ptype match {
              case Io(Input) => true
              case _         => false
            }
        })
      })
    }
  }
  case class DividerIo_out(
      val ready: Bool,
      val valid: Bool,
      val bits: UInt
  ) {
    def isIos: Boolean = {
      (ready.ptype match {
        case Io(Input) => true
        case _         => false
      }) &&
      (valid.ptype match {
        case Io(Output) => true
        case _          => false
      }) &&
      (bits.ptype match {
        case Io(Output) => true
        case _          => false
      })
    }
  }
  case class DividerIo(
      val in: DividerIo_in,
      val sign: Bool,
      val out: DividerIo_out
  ) {
    def isIos: Boolean = {
      in.isIos &&
      (sign.ptype match {
        case Io(Input) => true
        case _         => false
      }) &&
      out.isIos
    }
  }

  case class DividerReg(
      val state: UInt,     // Enum(5)
      val shiftReg: UInt,  // UInt((1 + len * 2).W)
      val aSignReg: Bool,  // RegEnable(aSign, newReq)
      val qSignReg: Bool,  // RegEnable((aSign ^ bSign) && !divBy0, newReq)
      val bReg: UInt,      // RegEnable(bVal, newReq)
      val aValx2Reg: UInt, // RegEnable(Cat(aVal, "b0".U), newReq)
      val cnt: UInt        // RegInit(0.U(BigInt(len).bitLength.W))   bitLength?
  ) {
    def clock(): Unit = {
      state.clock()
      shiftReg.clock()
      aSignReg.clock()
      qSignReg.clock()
      bReg.clock()
      aValx2Reg.clock()
      cnt.clock()
    }
    def isRegs: Boolean = {
      state.ptype == Reg &&
      shiftReg.ptype == Reg &&
      aSignReg.ptype == Reg &&
      qSignReg.ptype == Reg &&
      bReg.ptype == Reg &&
      aValx2Reg.ptype == Reg &&
      cnt.ptype == Reg
    }
    def setNext(regs: DividerReg): Unit = {
      state     := regs.state
      shiftReg  := regs.shiftReg
      aSignReg  := regs.aSignReg
      qSignReg  := regs.qSignReg
      bReg      := regs.bReg
      aValx2Reg := regs.aValx2Reg
      cnt       := regs.cnt
    }
  }

  case class Divaider(len: BigInt = 64) {

    def abs(a: UInt, sign: Bool): (Bool, UInt) = {
      val s = a(len - 1) && sign
      (s, Mux(s, -a, a))
    }
    def absSign(a: UInt, sign: Bool): Bool = {
      a(len - 1) && sign
    }
    def absVal(a: UInt, sign: Bool): UInt = {
      val s = a(len - 1) && sign
      Mux(s, -a, a)
    }

    def dividerTrans(io: DividerIo, regs: DividerReg): (DividerIo, DividerReg) = regs match {
      case DividerReg(state, shiftReg, aSignReg, qSignReg, bReg, aValx2Reg, cnt) =>
        // Enum(5)
        val s_idle    = Lit(0, 3).U
        val s_log2    = Lit(1, 3).U
        val s_shift   = Lit(2, 3).U
        val s_compute = Lit(3, 3).U
        val s_finish  = Lit(4, 3).U

        val newReq = (state === s_idle) && (io.in.ready && io.in.valid)

        val a      = io.in.bits(0)
        val b      = io.in.bits(1)
        val divBy0 = b === Lit(0, len).U

        val hi = shiftReg(len * 2, len)
        val lo = shiftReg(len - 1, 0)

        val aSign = absSign(a, io.sign)
        val aVal  = absVal(a, io.sign)
        val bSign = absSign(b, io.sign)
        val bVal  = absVal(b, io.sign)

        if (when(newReq)) {
          aSignReg := aSign
        } // val aSignReg = RegEnable(aSign, newReq)
        if (when(newReq)) {
          qSignReg := (aSign ^ bSign) && !divBy0
        } // val qSignReg = RegEnable((aSign ^ bSign) && !divBy0, newReq)
        if (when(newReq)) {
          bReg := bVal
        } // val aSignReg = RegEnable(aSign, newReq)
        if (when(newReq)) {
          aValx2Reg := Cat(aVal, Lit(0).U)
        } // val aValx2Reg = RegEnable(Cat(aVal, "b0".U), newReq)

        if (when(newReq)) {
          state := s_log2
        } else if (when(state === s_log2)) {
          val canSkipShift = (Lit(len).U + Log2(bReg)) - Log2(aValx2Reg)
          cnt := Mux(
            divBy0,
            Lit(0).U,
            Mux(
              canSkipShift >= Lit(len - 1).U,
              Lit(len - 1).U,
              canSkipShift
            )
          )
          state := s_shift

        } else if (when(state === s_shift)) {
          shiftReg := aValx2Reg << cnt
          state    := s_compute
        } else if (when(state === s_compute)) {
          val enough = hi >= bReg
          shiftReg := Cat(
            Mux(enough, hi - bReg, hi)(len - 1, 0),
            Cat(lo, enough)
          )
          cnt := cnt + Lit(1).U
          if (when(cnt === Lit(len - 1).U)) { state := s_finish }
        } else if (when(state === s_finish)) {
          if (when(io.out.ready)) {
            state := s_idle
          }
        }

        val r    = hi(len, 1)
        val resQ = Mux(qSignReg, -lo, lo)
        val resR = Mux(aSignReg, -r, r)
        io.out.bits := Cat(resR, resQ)

        io.out.valid := (state === s_finish)
        io.in.ready  := (state === s_idle)

        (io, DividerReg(state, shiftReg, aSignReg, qSignReg, bReg, aValx2Reg, cnt))
    }
    def dividerRun(timeout: Int, io: DividerIo, regInit: DividerReg): (DividerIo, DividerReg) = {
      require(timeout >= 0)

      if (timeout > 0) {
        val newState = dividerTrans(io, regInit)
        val newIo    = newState._1
        val newReg   = newState._2
        newReg.clock()
        dividerRun(timeout - 1, newIo, regInit)
      } else {
        (io, regInit)
      }
    }

    def run(io: DividerIo, randomInitValue: DividerReg) = {
      require(io.isIos)

      val regInit: DividerReg = DividerReg(
        UInt.emptyReg(BigInt(3)),
        UInt.emptyReg(1 + len * 2),
        Bool.emptyReg(),
        Bool.emptyReg(),
        UInt.emptyReg(len),
        UInt.emptyReg(len + 1),
        UInt.emptyReg(bitLength(len))
      )
      regInit.setNext(randomInitValue)
      regInit.state := Lit(0, 3).U      // RegInit
      regInit.cnt   := Lit(BigInt(0)).U // RegInit
      regInit.clock()

      dividerRun(100, io, regInit)
    }
  }
}
