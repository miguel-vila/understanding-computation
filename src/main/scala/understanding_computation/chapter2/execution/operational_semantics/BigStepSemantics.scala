package understanding_computation.chapter2.execution.operational_semantics

import understanding_computation.chapter2.ast._
import understanding_computation.chapter2.execution.Interpreter

import scalaz.State

object BigStepSemantics extends Interpreter {

  def evaluateWithEnvironment[T<:Value](environment: Environment, expression: Expression[T]): (Environment,T) ={
    expression.evaluator(environment)
  }

}
