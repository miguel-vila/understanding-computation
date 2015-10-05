package understanding_computation.chapter2.execution.operational_semantics

import understanding_computation.chapter2.ast._
import scalaz.{ State , Traverse }
import scalaz.std.list._

object BigStepSemantics {

  type EvaluationState[T<:Value] = State[Environment,T]

  def evaluate[T<:Value](expression: Expression[T]): T = {
    val (_,value) = evaluateWithEnvironment(expression)(Map.empty)
    value
  }

  def evaluateWithEnvironment[T<:Value](expression: Expression[T])(environment: Environment): (Environment,T) ={
    toState(expression)(environment)
  }

  def toState[T<:Value](expression: Expression[T]): EvaluationState[T] = expression match {
    case add: Add => addState(add)
    case boolean: Boolean => booleanState(boolean)
    case DoNothing => nothingState
    case assign: Assign[_] => assignState(assign)
    case _if: If => ifState(_if)
    case lessThan: LessThan => lessThanState(lessThan)
    case multiply: Multiply => multiplyState(multiply)
    case number: Number => numberState(number)
    case variable: Variable[_] => variableState(variable)
    case sequence: Sequence => sequenceState(sequence)
  }

  def assignState[T<:Value](assign: Assign[T]): EvaluationState[Void.type] =
    toState(assign.expression).flatMap { v =>
      State[Environment, Void.type](s => (s + ((assign.name, v)), Void))
    }

  val nothingState = State.state[Environment,Void.type](Void)

  def booleanState(boolean: Boolean): EvaluationState[BooleanValue] = State.state[Environment, BooleanValue]( boolean.value )

  def addState(add: Add): EvaluationState[NumberValue] =
    for {
      l <- toState(add.left)
      r <- toState(add.right)
    } yield l + r

  def ifState(_if: If): EvaluationState[Void.type] =
    for {
      condition <- toState(_if.condition)
      _         <- toState(if(condition.value) _if.consequence else _if.alternative)
    } yield Void

  def lessThanState(lessThan: LessThan): EvaluationState[BooleanValue] =
    for {
      l <- toState( lessThan.left )
      r <- toState( lessThan.right )
    } yield l < r

  def multiplyState(multiply: Multiply): EvaluationState[NumberValue] =
    for {
      l <- toState(multiply.left)
      r <- toState(multiply.right)
    } yield l * r

  def numberState(number: Number): EvaluationState[NumberValue] = State.state[Environment, NumberValue](number.value)

  def sequenceState(sequence: Sequence): EvaluationState[Void.type] = {
    Traverse[List].sequenceU( sequence.exps.map(toState).toList ) map { _ => Void }
  }

  def variableState[T<:Value](variable: Variable[T]): EvaluationState[T] = State.gets[Environment, T] { environment =>
    environment(variable.name).asInstanceOf[T]
  }

}
