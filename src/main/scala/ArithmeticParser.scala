import Op.Op

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

// Entities definition
object Op extends Enumeration {
  type Op = Value

  val Add, Sub, Mult, Div, Val = Value
}

abstract class OpNode(var op: Op = Op.Val, var value: Int = 0) {
  def getValue: Int
}

case class Operator(arg1: Number, arg2: Number) extends OpNode {
  println("inside operator")

  override def getValue: Int = {
    2
  }
}

case class Number(num: Int) extends OpNode {
  println("inside operand: " + num)

  override def getValue: Int = {
    5
  }
}

object ArithmeticParser extends RegexParsers with PackratParsers {
  // define lazy vals for what you want to parse
  lazy val op: Regex = "[*+-/]".r

  lazy val expr: PackratParser[OpNode] = (number <~ op) ~ number ^^ {case a ~ b => Operator(a, b)}

  lazy val integer: PackratParser[OpNode] = number

  lazy val number: PackratParser[Number] =
    "[0-9]+".r ^^ { n => Number(Integer.valueOf(n)) }

  def main(args: Array[String]): Unit = {
    // Execution
    println(ArithmeticParser.parseAll(expr, "10+12"))
  }
}