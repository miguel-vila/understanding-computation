package understanding_computation.chapter3

case class DFA(initialState: State, acceptingStates: Set[State], rules: DFARulebook) {

  private var currentState: State = initialState

  def isInAcceptingState(): Boolean = acceptingStates contains currentState

  def readCharacter(char: Char): Unit = {
    currentState = rules.nextState(currentState, char)
  }

  def readString(str: String): Unit = {
    str foreach readCharacter
  }

}
