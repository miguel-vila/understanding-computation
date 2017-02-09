package understanding_computation.chapter3

import org.scalatest._

class DFASpec extends FlatSpec with Matchers {

  val dfaRules = DFARulebook(List(FARule(1, 'a', 2), FARule(1, 'b', 1),
                             FARule(2, 'a', 2), FARule(2, 'b', 3),
                             FARule(3, 'a', 3), FARule(3, 'b', 3)))

  "DFA" should "identify correctly if its in an accepting state" in {
    val dfa1 = DFA(1, Set(3), dfaRules)
    dfa1.isInAcceptingState() should equal (false)

    val dfa2 = DFA(3, Set(3), dfaRules)
    dfa2.isInAcceptingState() should equal (true)

    val dfa3 = DFA(3, Set(2,3), dfaRules)
    dfa3.isInAcceptingState() should equal (true)

    val dfa4 = DFA(2, Set(2,3), dfaRules)
    dfa4.isInAcceptingState() should equal (true)
  }

  it should "transition correctly when feed chars" in {
    val dfa = DFA(1, Set(3), dfaRules)

    dfa.readCharacter('a')

    dfa.isInAcceptingState() should equal (false)

    dfa.readCharacter('a')

    dfa.isInAcceptingState() should equal (false)

    dfa.readCharacter('b')

    dfa.isInAcceptingState() should equal (true)

    val dfa2 = DFA(1, Set(3), dfaRules)

    dfa2.readString("aab")

    dfa2.isInAcceptingState() should equal (true)
  }
}
