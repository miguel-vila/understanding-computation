package understanding_computation.chapter3

case class NFARulebook(rules: Map[(State, Option[Char]), Iterable[NFARule]]) {

  def nextStates(states: Set[State], char: Option[Char]): Set[State] = {
    states.flatMap { st => followRulesFor(st, char) }
  }

  def followRulesFor(state: State, char: Option[Char]): Iterable[State] = {
    rules((state, char)).map(_.follow())
  }

  def followFreeMoves(states: Set[State]): Set[State] = {
    val moreStates = nextStates(states, None)

    if( moreStates.forall(st => states contains st)) {
      states
    } else {
      followFreeMoves(states union moreStates)
    }
  }

  def values: Iterable[NFARule] = rules.values.flatten

}

object NFARulebook {

  def apply(rules: Iterable[NFARule]): NFARulebook = {
    val _rules = rules.groupBy { rule => (rule.state, rule.char) }
    new NFARulebook( _rules withDefault { case _ => List.empty } )
  }

}
