package understanding_computation.chapter3

case class NFARule(state: State, char: Option[Char], nextState: State) {

  def follow(): State = nextState

}

object NFARule {

  def apply(state: State, char: Char, nextState: State): NFARule = NFARule(state, Some(char), nextState)

  def freeMove(state: State, nextState: State): NFARule = NFARule(state, None, nextState)

}
