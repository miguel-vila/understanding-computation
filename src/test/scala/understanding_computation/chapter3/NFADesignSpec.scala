package understanding_computation.chapter3

import org.scalatest._

class NFADesignSpec extends FlatSpec with Matchers {
  val rulebook1 = NFARulebook(List(NFARule(1, 'a', 1), NFARule(1, 'b',1), NFARule(1,'b',2),
                                   NFARule(2, 'a', 3), NFARule(2, 'b',3),
                                   NFARule(3, 'a', 4), NFARule(3, 'b', 4)
                              ))

  val rulebook2 = NFARulebook(List(NFARule.freeMove(1, 2), NFARule.freeMove(1, 4),
                                   NFARule(2, 'a', 3),
                                   NFARule(3, 'a', 2),
                                   NFARule(4, 'a', 5),
                                   NFARule(5, 'a', 6),
                                   NFARule(6, 'a', 4)
                              ))

  "NFADesign" should "recognize correctly strings" in {
    val nfaDesign = NFADesign(1, Set(4), rulebook1)

    nfaDesign.accepts("bab") should equal (true)
    nfaDesign.accepts("bbbbb") should equal (true)
    nfaDesign.accepts("bbabb") should equal (false)
  }

  it should "support free moves" in {
    val nfaDesign = NFADesign(1, Set(2,4), rulebook2)

    nfaDesign.accepts("aa") should equal (true)
    nfaDesign.accepts("aaa") should equal (true)
    nfaDesign.accepts("aaaaa") should equal (false)
    nfaDesign.accepts("aaaaaa") should equal (true)
  }
}
