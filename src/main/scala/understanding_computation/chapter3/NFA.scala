package understanding_computation.chapter3

case class NFA(initialState: State, acceptingStates: Set[State], rules: NFARulebook) {

  private var _currentStates: Set[State] = Set(initialState)

  def setCurrentStates(cs: Set[State]): Unit = {
    _currentStates = cs
  }

  def currentStates: Set[State] = rules.followFreeMoves(_currentStates)

  def isInAcceptingState(): Boolean = currentStates exists acceptingStates.contains

  def readCharacter(char: Char): Unit = {
    setCurrentStates( rules.nextStates(currentStates, Some(char)) )
  }

  def readString(str: String): Unit = {
    str foreach readCharacter
  }
}
