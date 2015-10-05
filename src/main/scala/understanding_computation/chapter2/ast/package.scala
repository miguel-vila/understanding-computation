package understanding_computation.chapter2

import scalaz._

package object ast {

  type Environment = Map[String, Value]

  type Evaluator[T<:Value] = State[Environment,T]

}
