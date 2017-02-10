package understanding_computation.chapter3.regex

import understanding_computation.chapter3._

sealed trait Pattern {

  def precedence: Int

  def bracket(outerPrecedence: Int): String =
    if( outerPrecedence > precedence )
      s"(${toString()})"
    else
      toString()

  def toNFADesign: NFADesign

  def matches(str: String): Boolean = toNFADesign.accepts(str)

}

case object Empty extends Pattern {

  def precedence = 3

  def toNFADesign: NFADesign = {
    val startState = StateGenerator.generate()
    val acceptStates = Set(startState)
    val rules = NFARulebook(List())

    NFADesign(startState, acceptStates, rules)
  }

  override def toString = "''"

}

case class Literal(char: Char) extends Pattern {

  def precedence = 3

  override def toString(): String = char.toString()

  def toNFADesign: NFADesign = {
    val startState = StateGenerator.generate()
    val endState = StateGenerator.generate()

    val rule = NFARule(startState, char, endState)
    val rules = NFARulebook(List(rule))

    NFADesign(startState, Set(endState), rules)
  }

}

case class Concatenate(first: Pattern, second: Pattern) extends Pattern {

  def precedence = 1

  override def toString(): String = s"${first.bracket(precedence)}${second.bracket(precedence)}"

  def toNFADesign: NFADesign = {
    val startState = StateGenerator.generate()
    val nfa1 = first.toNFADesign
    val nfa2 = second.toNFADesign

    val rules = nfa1.rules.values ++ nfa2.rules.values
    val extraRules = nfa1.acceptingStates.map { state =>
      NFARule.freeMove(state, nfa2.initialState)
    } + NFARule.freeMove(startState, nfa1.initialState)

    NFADesign(startState, nfa2.acceptingStates, NFARulebook(rules ++ extraRules))
  }
}

case class Choose(first: Pattern, second: Pattern) extends Pattern {

  def precedence = 0

  override def toString(): String = s"${first.bracket(precedence)}|${second.bracket(precedence)}"

  def toNFADesign: NFADesign = {
    val startState = StateGenerator.generate()
    val nfa1 = first.toNFADesign
    val nfa2 = second.toNFADesign

    val rules = nfa1.rules.values ++ nfa2.rules.values
    val extraRules: List[NFARule] = List(nfa1, nfa2).map { nfa =>
      NFARule.freeMove(startState, nfa.initialState)
    }

    NFADesign(startState, nfa1.acceptingStates union nfa2.acceptingStates, NFARulebook(rules ++ extraRules))
  }
}

case class Repeat(pattern: Pattern) extends AnyRef with Pattern {

  def precedence = 2

  override def toString(): String = s"${pattern.bracket(precedence)}*"

  def toNFADesign: NFADesign = {
    val startState = StateGenerator.generate()
    val nfa = pattern.toNFADesign
    val acceptingStates = nfa.acceptingStates + startState

    val extraRules = nfa.acceptingStates.map { st =>
      NFARule.freeMove(st, nfa.initialState)
    } + NFARule.freeMove(startState, nfa.initialState)

    NFADesign(startState, acceptingStates, NFARulebook(nfa.rules.values ++ extraRules))
  }
}
