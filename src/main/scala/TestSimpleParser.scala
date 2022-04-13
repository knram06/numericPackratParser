import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

object TestSimpleParser extends RegexParsers with PackratParsers {

  // Entities definition
  sealed trait LogicalUnit

  case class Variable(name: String) extends LogicalUnit

  case class Not(arg: LogicalUnit) extends LogicalUnit

  case class And(arg1: LogicalUnit, arg2: LogicalUnit) extends LogicalUnit

  // In order of descending priority
  lazy val pattern: PackratParser[LogicalUnit] =
    ((and) | (not) | (variable))

  lazy val variable: PackratParser[Variable] =
    "[a-zA-Z]".r ^^ { n => Variable(n) }

  lazy val term: PackratParser[LogicalUnit] =
    variable | "(" ~> and <~ ")"

  lazy val not: PackratParser[Not] =
    ("!" ~> term) ^^ { x => Not(x) }

  lazy val and: PackratParser[And] =
    ((pattern <~ "&") ~ pattern) ^^ { case a ~ b => And(a, b) }

  def main(args: Array[String]): Unit = {
    // Execution
    println(TestSimpleParser.parseAll(pattern, "!a & !b"))
  }
}