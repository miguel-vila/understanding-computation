package understanding_computation.chapter3

import org.scalatest._

class DFARulebookSpec extends FlatSpec with Matchers {
  "DFARulebook" should "compute the correct next state for a simple DFA" in {
    val dfa = DFARulebook(List(FARule(1, 'a', 2), FARule(1, 'b', 1),
                               FARule(2, 'a', 2), FARule(2, 'b', 3),
                               FARule(3, 'a', 3), FARule(3, 'b', 3)))

    dfa.nextState(1, 'a') should equal (2)
    dfa.nextState(2, 'b') should equal (3)
    dfa.nextState(3, 'b') should equal (3)
  }
}
