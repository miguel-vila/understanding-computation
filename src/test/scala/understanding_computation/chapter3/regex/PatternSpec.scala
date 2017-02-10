package understanding_computation.chapter3.regex

import org.scalatest._

class PatternSpec extends FlatSpec with Matchers {
  "Pattern" should "match a simple regex" in {
    val regex = Repeat(Concatenate(Literal('a'), Choose(Empty, Literal('b'))))
    println(regex)

    println(regex.toNFADesign)

    regex.matches("") should equal (true)
    regex.matches("a") should equal (true)
    regex.matches("ab") should equal (true)
    regex.matches("aaaaa") should equal (true)
    regex.matches("aaaaababa") should equal (true)
  }
}
