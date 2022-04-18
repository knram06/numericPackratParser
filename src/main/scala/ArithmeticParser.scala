import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

// Entities definition
object Op extends Enumeration {
  type Op = Value

  val Add, Sub, Mult, Div, Val = Value
}

abstract class OpNode {
  def getValue: Int
}

case class Operator(oper: String, left: OpNode, right: OpNode) extends OpNode {
  // println("inside operator")

  override def getValue: Int = {
    val lv = left.getValue
    val rv = right.getValue

    oper match {
      case "+" => lv + rv
      case "-" => lv - rv
      case "*" => lv * rv
      case "/" => lv / rv
      case _ =>
        println("Unrecognized operator")
        throw new Exception
    }
  }
}

case class Number(num: Int) extends OpNode {
  // println("inside operand: " + num)

  override def getValue: Int = {
    num
  }
}

object ArithmeticParser extends RegexParsers with PackratParsers {
  // define lazy vals for what you want to parse
  lazy val op: PackratParser[String] = "[*+-/]".r ^^ {s => s}

  lazy val expr: PackratParser[OpNode] = (
    "(" ~ expr ~ op ~ expr ~ ")" ^^ {case s ~ a ~ b ~ c ~ t => Operator(b, a, c)} |
    (expr ~ op ~ expr) ^^ {case a ~ b ~ c => Operator(b, a, c)} |
    number
    )

  lazy val number: PackratParser[Number] =
    "[0-9]+".r ^^ { n => Number(Integer.valueOf(n)) }

  def main(args: Array[String]): Unit = {
    // Execution
    val parsedObj = ArithmeticParser.parseAll(expr, "10+12/15")
    println(parsedObj)
    val opNode = parsedObj.get
    println("opTree value: " + opNode.getValue)
  }
}