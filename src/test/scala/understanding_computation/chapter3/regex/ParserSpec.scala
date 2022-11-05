package understanding_computation.chapter3.regex

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "Parser" should "have a test!" in {
    new Parser("''").parseRegex() shouldBe (Right(Empty))
    new Parser("c").parseRegex() shouldBe (Right(Literal('c')))
    new Parser("(x)*").parseRegex() shouldBe (Right(Repeat(Literal('x'))))
    new Parser("x|y").parseRegex() shouldBe (Right(Choose(Literal('x'), Literal('y'))))
  }
}
