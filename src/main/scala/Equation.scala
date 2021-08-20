import scala.io.StdIn
import scala.util.Random

trait Equation

object Equation {
  def lim(md: Boolean): Int = if (md) 20 else 20

  def make(implicit useMulDiv: Boolean = false): Equation = Random.between(0, lim(useMulDiv)) match {
    case 0 => Minus(Plus.make(), Plus(Group.make, Minus.make()))
    case 1 => Minus(Plus.make(), Plus(Group.make, Plus.make()))
    case 2 => Plus(Plus.make(), Plus(Group.make, Minus.make()))
    case 3 => Plus(Plus.make(), Plus(Group.make, Plus.make()))

    case 4 => Minus(Plus.make(), Minus(Group.make, Minus.make()))
    case 5 => Minus(Plus.make(), Minus(Group.make, Plus.make()))
    case 6 => Plus(Plus.make(), Minus(Group.make, Minus.make()))
    case 7 => Plus(Plus.make(), Minus(Group.make, Plus.make()))

    case 8 => Minus(Minus.make(), Plus(Group.make, Minus.make()))
    case 9 => Minus(Minus.make(), Plus(Group.make, Plus.make()))
    case 10 => Plus(Minus.make(), Plus(Group.make, Minus.make()))
    case 11 => Plus(Minus.make(), Plus(Group.make, Plus.make()))

    case 12 => Minus(Minus.make(), Minus(Group.make, Minus.make()))
    case 13 => Minus(Minus.make(), Minus(Group.make, Plus.make()))
    case 14 => Plus(Minus.make(), Minus(Group.make, Minus.make()))
    case 15 => Plus(Minus.make(), Minus(Group.make, Plus.make()))

    case 16 => Minus(Plus.make(), Plus(Group.make, EquationNum.make()))
    case 17 => Plus(Plus.make(), Plus(Group.make, EquationNum.make()))
    case 18 => Minus(Minus.make(), Plus(Group.make, EquationNum.make()))
    case 19 => Plus(Minus.make(), Plus(Group.make, EquationNum.make()))
  }
}

case class EquationNum(n: Rational) extends Equation

object EquationNum {
  def make(): Equation = EquationNum(Rational(Random.between(0, 100)))
}

case class Group(e: Equation) extends Equation

object Group {
  def lim(md: Boolean): Int = if (md) 16 else 8

  def make(implicit useMulDiv: Boolean = false): Equation = Random.between(0, lim(useMulDiv)) match {
    case 0 => Group(Plus(Minus.make, Minus.make))
    case 1 => Group(Plus(Minus.make, Plus.make))
    case 2 => Group(Plus(Plus.make, Minus.make))
    case 3 => Group(Plus(Plus.make, Plus.make))

    case 4 => Group(Plus(EquationNum.make, Plus.make))
    case 5 => Group(Plus(EquationNum.make, Minus.make))
    case 6 => Group(Minus(EquationNum.make, Plus.make))
    case 7 => Group(Minus(EquationNum.make, Minus.make))

    case 8 => Group(Plus(Multiply.make, Minus.make))
    case 9 => Group(Plus(Minus.make, Divide.make))
    case 10 => Group(Plus(Plus.make, Multiply.make))
    case 11 => Group(Plus(Divide.make, Plus.make))

    case 12 => Group(Plus(EquationNum.make, Multiply.make))
    case 13 => Group(Plus(EquationNum.make, Divide.make))
    case 14 => Group(Minus(EquationNum.make, Multiply.make))
    case 15 => Group(Minus(EquationNum.make, Divide.make))
  }
}

case class Plus(left: Equation, right: Equation) extends Equation

object Plus {
  def make(): Equation = Plus(EquationNum.make(), EquationNum.make())
}

case class Minus(left: Equation, right: Equation) extends Equation

object Minus {
  def make(): Equation = Minus(EquationNum.make(), EquationNum.make())
}

case class Multiply(left: Equation, right: Equation) extends Equation

object Multiply {
  def make(): Equation = Multiply(EquationNum.make(), EquationNum.make())
}

case class Divide(left: Equation, right: Equation) extends Equation

object Divide {
  def make(): Equation = Divide(EquationNum.make(), EquationNum.make())
}

//case class ResultAndRemainder(n: Int, r: Int) {
//  def +(that: ResultAndRemainder): ResultAndRemainder = ResultAndRemainder(this.n + that.n, this.r + that.r)
//
//  def -(that: ResultAndRemainder): ResultAndRemainder = ResultAndRemainder(this.n - that.n, this.r - that.r)
//
//  def *(that: ResultAndRemainder): ResultAndRemainder = ResultAndRemainder(this.n * that.n, this.r * that.r)
//
//  def /(that: ResultAndRemainder): ResultAndRemainder = ResultAndRemainder(this.n / that.n, this.r / that.r)
//
//  override def toString(): String = n.toString //s"$n ($r)"
//}

//object ResultAndRemainder {
//  def zero = ResultAndRemainder(0, 0)
//}

object Equator {
  def make(useMulDiv: Boolean = false, lim: Int = 100): Equation = {
    var e = Equation.make(useMulDiv)
    var buf = solveSafe(collect(e))
    while (buf.isEmpty || buf.get.n.denom != 1 || buf.get.n < 0 || buf.get.n > lim) {
      //while (buf.get.n < 0 || buf.get.n > lim) {
      e = Equation.make(useMulDiv)
      buf = solveSafe(collect(e))
    }
    e
  }

  def show(e: Equation): String = e match {
    case Plus(left, right) => show(left) + " + " + show(right)
    case Minus(left, right) => show(left) + " - " + show(right)
    case Multiply(left, right) => show(left) + " * " + show(right)
    case Divide(left, right) => show(left) + " / " + show(right)
    case EquationNum(n) => n.toString
    case Group(inner) => "(" + show(inner) + ")"
  }

  trait Calc

  case class Number(n: Rational) extends Calc

  case class GroupCalc(seq: Seq[Calc]) extends Calc

  class Operation extends Calc

  case object OperationPlus extends Operation

  case object OperationMinus extends Operation

  case object OperationMultiply extends Operation

  case object OperationDivide extends Operation

  def collect(e: Equation): Seq[Calc] = {
    def collectInner(e: Equation)(accumulator: Seq[Calc]): Seq[Calc] = e match {
      case EquationNum(n) => accumulator :+ Number(n)
      case Minus(left, right) => collectInner(right)(collectInner(left)(accumulator) :+ OperationMinus)
      case Plus(left, right) => collectInner(right)(collectInner(left)(accumulator) :+ OperationPlus)
      case Divide(left, right) => collectInner(right)(collectInner(left)(accumulator) :+ OperationDivide)
      case Multiply(left, right) => collectInner(right)(collectInner(left)(accumulator) :+ OperationMultiply)
      case Group(groupEquation) => accumulator :+ GroupCalc(collectInner(groupEquation)(Seq.empty[Calc]))
    }

    collectInner(e)(Seq.empty[Calc])
  }

  def solveSafe(seq: Seq[Calc]): Option[Number] = {
    try {
      Some(solve(seq))
    } catch {
      case _: IllegalArgumentException => None
    }
  }

  def solve(seq: Seq[Calc]): Number = {
    def resolveInner(c: Calc): Number = c match {
      case n: Number => n
      case GroupCalc(seq) => solve(seq)
    }

    def convertOpMulDiv(l: Calc, o: Operation, r: Calc): Seq[Calc] = {
      val resolvedLeft = resolveInner(l).n
      val resolvedRight = resolveInner(r).n
      o match {
        case OperationMultiply => Seq(Number(resolvedLeft * resolvedRight))
        case OperationDivide => Seq(Number(resolvedLeft / resolvedRight))
      }
    }

    def convertOpPlusMinus(l: Calc, o: Operation, r: Calc): Seq[Calc] = {
      val resolvedLeft = resolveInner(l).n
      val resolvedRight = resolveInner(r).n
      o match {
        case OperationPlus => Seq(Number(resolvedLeft + resolvedRight))
        case OperationMinus => Seq(Number(resolvedLeft - resolvedRight))
      }
    }

    def solveInnerMulDiv(s: Seq[Calc], buf: Seq[Calc], acc: Seq[Calc]): Seq[Calc] = {
      s match {
        case Nil => buf match {
          case l :: (o@(OperationMultiply | OperationDivide)) :: r :: Nil =>
            acc ++ convertOpMulDiv(l, o.asInstanceOf[Operation], r)
          case _ =>
            acc ++ buf
        }
        case _ => buf match {
          case l :: (o@(OperationMultiply | OperationDivide)) :: r :: Nil =>
            solveInnerMulDiv(s.tail, convertOpMulDiv(l, o.asInstanceOf[Operation], r) :+ s.head, acc)
          case _ :: (OperationPlus | OperationMinus) :: Nil =>
            solveInnerMulDiv(s.tail, Seq(s.head), acc ++ buf)
          case _ =>
            solveInnerMulDiv(s.tail, buf :+ s.head, acc)
        }
      }
    }

    def solveInnerPlusMinus(s: Seq[Calc], buf: Seq[Calc], acc: Seq[Calc]): Seq[Calc] = {
      s match {
        case Nil => buf match {
          case l :: (o@(OperationPlus | OperationMinus)) :: r :: Nil =>
            acc ++ convertOpPlusMinus(l, o.asInstanceOf[Operation], r)
          case _ =>
            acc ++ buf
        }
        case _ => buf match {
          case l :: (o@(OperationPlus | OperationMinus)) :: r :: Nil =>
            solveInnerPlusMinus(s.tail, convertOpPlusMinus(l, o.asInstanceOf[Operation], r) :+ s.head, acc)
          case _ =>
            solveInnerPlusMinus(s.tail, buf :+ s.head, acc)
        }
      }
    }

    val mulDiv = solveInnerMulDiv(seq, Seq.empty[Calc], Seq.empty[Calc])
    val res = solveInnerPlusMinus(mulDiv, Seq.empty[Calc], Seq.empty[Calc])
    res.head match {
      case n: Number => n
    }
  }

  def calculate(sequence: Seq[Calc]): Rational = {
    var acc = sequence.head match {
      case Number(n) => n
      case GroupCalc(seq) => calculate(seq)
    }

    var sign = 1

    for (item <- sequence.tail) {
      item match {
        case Number(n) => acc = acc + sign * n
        case OperationMinus => sign = -1
        case OperationPlus => sign = 1
        case GroupCalc(seq) => acc = acc + sign * calculate(seq)
      }
    }

    acc
  }

  def demo(): Unit = {

    //    val test1: Seq[Calc] = Seq(
    //      GroupCalc(Seq(Number(4), OperationMinus, Number(2))),
    //      OperationMultiply,
    //      GroupCalc(Seq(Number(6), OperationMinus, Number(4))),
    //      OperationDivide,
    //      GroupCalc(Seq(Number(1), OperationPlus, Number(1))),
    //    )
    //    val test1converted = solve(test1)
    //    println(test1converted)

    //    val test1: Seq[Calc] = Seq(Number(3), OperationPlus, GroupCalc(Seq(Number(60), OperationDivide, Number(10))), OperationDivide, Number(3))
    //    val test1converted = solve(test1)
    //    println(test1converted)

    //        val e1 = Minus(Num(7), Num(3))
    //    val e1 = Minus(Plus(Num(1), Num(1)), Plus(Group(Num(1)), Plus(Num(1), Num(1))))
    //    val e1 = Minus(
    //      Plus(
    //        Num(5),
    //        Num(4)
    //      ),
    //      Plus(
    //        Num(3), //Group(Num(3)),
    //        Plus(Num(2), Num(1))
    //      )
    //    )
    //    val e1collected = collect(e1)(Seq.empty[Calc])
    //    println(s"show: ${show(e1)} = ${e1collected.mkString(",")} = ${calculate(e1collected)}")
    //
    //    val m0 = Minus(Num(1), Num(1))
    //    val m0collected = collect(m0)(Seq.empty[Calc])
    //    println(s"show: ${show(m0)} = ${m0collected.mkString(",")} = ${calculate(m0collected)}")
    //
    //    val m1 = Minus(Minus(Num(1), Num(1)), Num(1))
    //    val m1collected = collect(m1)(Seq.empty[Calc])
    //    println(s"show: ${show(m1)} = ${m1collected.mkString(",")} = ${calculate(m1collected)}")
    //
    //    val m11 = Minus(Num(1), Minus(Num(1), Num(1)))
    //    val m11collected = collect(m11)(Seq.empty[Calc])
    //    println(s"show: ${show(m11)} = ${m11collected.mkString(",")} = ${calculate(m11collected)}")
    //
    //    val m111 = Minus(
    //      Minus(Num(1), Num(1)),
    //      Minus(Num(1), Num(1))
    //    )
    //    val m111collected = collect(m111)(Seq.empty[Calc])
    //    println(s"show: ${show(m111)} = ${m111collected.mkString(",")} = ${calculate(m111collected)}")
    //
    //    val mg = Minus(
    //      Group(Minus(
    //        Minus(Num(1), Num(1)),
    //        Minus(Num(1), Num(1))
    //      )),
    //      Minus(Num(1), Num(1))
    //    )
    //
    //    val mgcollected = collect(mg)(Seq.empty[Calc])
    //    println(s"show: ${show(mg)} = ${mgcollected.mkString(",")}")
    //    println(s"show: ${show(mg)} = ${mgcollected.mkString(",")} = ${calculate(mgcollected)}")

    val count = 1000

    for (i <- 1 to count) {
      val e1 = make(false, 1000)
      val e1collected = collect(e1)
      //println(s"$i:  show: ${show(e1)} = ${e1collected.mkString(",")} = ${solve(e1collected).n}")
      println(s"$i\':  ${show(e1)} = ${solveSafe(e1collected).get.n}")
    }

    for (i <- 1 to count) {
      val e1 = make(true, 1000)
      val e1collected = collect(e1)
      //println(s"$i:  show: ${show(e1)} = ${e1collected.mkString(",")} = ${solve(e1collected).n}")
      println(s"""$i":  ${show(e1)} = ${solveSafe(e1collected).get.n}""")
    }

  }
}