package understanding_computation.chapter3

case class FARule(state: State, char: Char, nextState: State) {

  def follow(): State = nextState

}
