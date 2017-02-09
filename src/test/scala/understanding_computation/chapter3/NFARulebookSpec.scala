package understanding_computation.chapter3

import org.scalatest._

class NFARulebookSpec extends FlatSpec with Matchers {
  "NFARulebook" should "compute correctly the next states" in {
    val rulebook = NFARulebook(List(NFARule(1, 'a', 1), NFARule(1,'b',1), NFARule(1,'b',2),
                                    NFARule(2, 'a', 3), NFARule(2,'b',3),
                                    NFARule(3, 'a', 4), NFARule(3, 'b', 4)
                               ))

    rulebook.nextStates(Set(1), Some('b')) should equal (Set(1,2))
    rulebook.nextStates(Set(1,2), Some('b')) should equal (Set(1,2,3))
    rulebook.nextStates(Set(3,2), Some('a')) should equal (Set(3,4))

  }
}
