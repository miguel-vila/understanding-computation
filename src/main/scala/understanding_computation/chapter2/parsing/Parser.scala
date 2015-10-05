package understanding_computation.chapter2.parsing

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import understanding_computation.chapter2.ast._
import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers with RegexParsers {

  val ws = rep(" ")

  def withOptionalWhitespace[A](p: Parser[A]): Parser[A] = (ws ~> p <~ ws)

  val number: Parser[Number] = withOptionalWhitespace( floatingPointNumber ).map(s => Number(s.toFloat) )

  val _ifString = "if"
  val trueString = "true"
  val falseString = "false"

  val reservedWords = Set(trueString, falseString, _ifString)

  val varName = regex("[a-zA-Z]+".r).filter(!reservedWords.contains(_))

  val booleanLiteral = (trueString ^^^ Boolean.True) | (falseString ^^^ Boolean.False)

  val factor = number | ( varName map { name => Variable[NumberValue](name) } )

  val addOp = "+" ^^^ (Add.apply(_,_))

  val multiplyOp = "*" ^^^ (Multiply.apply(_,_))

  val term: Parser[NumberExpression] = chainl1(factor, multiplyOp)

  val arithmeticExpr: Parser[NumberExpression] = chainl1(term, addOp)

  val lessThan = ((arithmeticExpr <~ "<") ~ arithmeticExpr) map { case l ~ r => LessThan(l,r) }

  val booleanExpr = booleanLiteral | lessThan

  val assign: Parser[VoidExpression] = (varName ~ "=" ~ arithmeticExpr) map { case name ~ _ ~ arithmeticExpr => Assign(name, arithmeticExpr) }

  val expression = assign

  val _if: Parser[VoidExpression] =
    (_ifString ~> "(" ~> booleanExpr <~ ")") ~
      ("{" ~> sequence <~ "}") ~
    ("else" ~>
      ("{" ~> sequence <~ "}")) map {
      case condition ~ conseq ~ altern => If(condition, conseq, altern)
    }

  val sequence: Parser[VoidExpression] = rep1( (assign <~ ";") | _if ) map { sts => Sequence(sts:_*) }

}
