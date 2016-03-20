package understanding_computation.chapter2

import understanding_computation.chapter2.ast.Value

trait IOEffect
case class PrintEffect[ V <: Value ](value: V) extends IOEffect