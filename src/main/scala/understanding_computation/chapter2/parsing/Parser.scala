package understanding_computation.chapter2.parsing

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import understanding_computation.chapter2.ast._
import scala.util.parsing.combinator._

object Parser extends JavaTokenParsers with RegexParsers {

  def parseToAST(content: String): ParseResult[Expression[_<:Value]] = parseAll(expression, content)

  val ws = rep(" ")

  def withOptionalWhitespace[A](p: Parser[A]): Parser[A] = (ws ~> p <~ ws)

  val number: Parser[Number] = withOptionalWhitespace( floatingPointNumber ).map(s => Number(s.toFloat) )

  val _ifString = "if"
  val _whileString = "while"
  val trueString = "true"
  val falseString = "false"

  val reservedWords = Set(trueString, falseString, _ifString, _whileString)

  val varName = regex("[a-zA-Z]+".r).filter(!reservedWords.contains(_))

  val booleanLiteral = (trueString ^^^ Boolean.True) | (falseString ^^^ Boolean.False)

  val factor: Parser[NumberExpression] = number | ( varName map { name => NumberVariable(name) } )

  val addOp = "+" ^^^ (Add.apply(_,_))

  val multiplyOp = "*" ^^^ (Multiply.apply(_,_))

  val term: Parser[NumberExpression] = chainl1(factor, multiplyOp)

  val arithmeticExpr: Parser[NumberExpression] = chainl1(term, addOp)

  val lessThan = ((arithmeticExpr <~ "<") ~ arithmeticExpr) map { case l ~ r => LessThan(l,r) }

  val booleanExpr = booleanLiteral | lessThan

  val assign: Parser[Assign] = (varName ~ "=" ~ arithmeticExpr <~ ";") map { case name ~ _ ~ arithmeticExpr => Assign(name, arithmeticExpr) }

  val _if: Parser[If] =
    (_ifString ~> "(" ~> booleanExpr <~ ")") ~
      ("{" ~> voidExpression <~ "}") ~
    ("else" ~>
      ("{" ~> voidExpression <~ "}")) map {
      case condition ~ conseq ~ altern => If(condition, conseq, altern)
    }

  val _while: Parser[While] =
    (_whileString ~> "(" ~> booleanExpr <~ ")") ~
      ("{" ~> voidExpression <~ "}") map { case condition ~ body => While(condition, body) }

  val statement: Parser[VoidExpression] = _if | _while | assign

  val voidExpression: Parser[VoidExpression] = rep1( statement ) map { sts =>
    if(sts.size == 1){
      sts.head
    } else {
      Sequence(sts:_*)
    }
  }

  val expression: Parser[Expression[_<:Value]] = voidExpression

}
