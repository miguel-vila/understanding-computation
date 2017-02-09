package understanding_computation.chapter3

import org.scalatest._

class DFADesignSpec extends FlatSpec with Matchers {

  val dfaRules = DFARulebook(List(FARule(1, 'a', 2), FARule(1, 'b', 1),
                                  FARule(2, 'a', 2), FARule(2, 'b', 3),
                                  FARule(3, 'a', 3), FARule(3, 'b', 3)))

  "DFADesign" should "create and execute a DFA" in {
    val dfaDesign = DFADesign(1, Set(3), dfaRules)

    dfaDesign.acceptsString("a") should equal (false)
    dfaDesign.acceptsString("baa") should equal (false)
    dfaDesign.acceptsString("baba") should equal (true)
  }
}
