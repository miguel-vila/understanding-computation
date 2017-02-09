package understanding_computation.chapter3

object StateGenerator {

  private var current = 0

  def generate(): State = {
    current+=1
    current
  }

}
