package understanding_computation.chapter3

case class NFADesign(initialState: State, acceptingStates: Set[State], rules: NFARulebook) {

  def buildNFA(): NFA = NFA(initialState, acceptingStates, rules)

  def accepts(str: String): Boolean = {
    val nfa = buildNFA()
    nfa.readString(str)
    nfa.isInAcceptingState()
  }

}
