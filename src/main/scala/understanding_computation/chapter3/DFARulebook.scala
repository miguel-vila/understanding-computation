package understanding_computation.chapter3

case class DFARulebook(rules: Map[(State, Char), FARule]) extends AnyRef {

  def nextState(state: State, char: Char): State = rules((state, char)).follow

}

object DFARulebook {

  def apply(rules: Iterable[FARule]): DFARulebook = {
    val _rules = rules.view.map { rule => (rule.state, rule.char) -> rule }.toMap
    DFARulebook(_rules)
  }

}
