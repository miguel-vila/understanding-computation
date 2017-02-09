package understanding_computation.chapter3

case class DFADesign(initialState: State, acceptingStates: Set[State], rules: DFARulebook) {

  def getDFA(): DFA = DFA(initialState, acceptingStates, rules)

  def acceptsString(str: String): Boolean = {
    val dfa = getDFA()
    dfa.readString(str)
    dfa.isInAcceptingState()
  }

}
