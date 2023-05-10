package chicala

import stainless.collection._

object until {
  def apply(l: BigInt, r: BigInt): List[BigInt] = {
    require(l <= r)
    def f(res: List[BigInt], i: BigInt): List[BigInt] = {
      if (l <= i) f(i :: res, i - 1)
      else res
    }
    f(Nil[BigInt](), r - 1)
  }
}

object max {
  def apply(a: BigInt, b: BigInt): BigInt = {
    if (a > b) a else b
  }
  def apply(ns: List[BigInt]): BigInt = {
    require(ns.size > 0)
    def f(n: BigInt, ns: List[BigInt]): BigInt = {
      ns match {
        case Cons(head, tail) => f(max(n, head), tail)
        case Nil()            => n
      }
    }
    f(ns.head, ns.tail)
  }
}
