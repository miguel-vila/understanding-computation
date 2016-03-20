package understanding_computation.chapter2.ast

import scalaz.{ State , Traverse }
import scalaz.std.list._
import scalaz.State

sealed trait Expression[T<:Value] {
  def evaluator: Evaluator[T]
}
trait Literal[T<:Value] extends Expression[T] {
  def value: T
  val evaluator: Evaluator[T] = State.state(value)
}
sealed trait VoidExpression extends Expression[Void.type]
case object DoNothing extends VoidExpression with Literal[Void.type] {
  def value = Void
}
case class Assign(name: String, expression: Expression[_<:Value]) extends VoidExpression {
  val evaluator: Evaluator[Void.type] =
    for {
      value <- expression.evaluator
      _     <- State.modify { s: Environment => s + (name -> value) }
    } yield Void
}

sealed trait BooleanExpression extends Expression[BooleanValue]
case class Boolean(value: BooleanValue) extends BooleanExpression with Literal[BooleanValue] {
}
object Boolean {
  def apply(boolean: scala.Boolean): Boolean = Boolean(BooleanValue(boolean))
  def True: Boolean = apply(true)
  def False: Boolean = apply(false)
}

sealed trait NumberExpression extends Expression[NumberValue]
sealed trait Comparison extends BooleanExpression {
  def left: NumberExpression
  def right: NumberExpression
  def combinedWith(f: (NumberValue, NumberValue) => BooleanValue) = {
    for {
      l <- left.evaluator
      r <- right.evaluator
    } yield f(l,r)
  }
}
case class LessThan(left: NumberExpression, right: NumberExpression) extends Comparison {
  val evaluator = combinedWith(_ < _)
}
case class Equal(left: NumberExpression, right: NumberExpression) extends Comparison {
  val evaluator = combinedWith(_ eq _)
}
case class GreaterThan(left: NumberExpression, right: NumberExpression) extends Comparison {
  val evaluator = combinedWith(_ > _)
}

case class If(
               condition:    BooleanExpression,
               consequence:  VoidExpression,
               alternative:  VoidExpression) extends VoidExpression {
  val evaluator: Evaluator[Void.type] =
    for {
      condition <- condition.evaluator
      _         <- (if(condition.value) consequence else alternative).evaluator
    } yield Void
}

case class While(condition: BooleanExpression, body: VoidExpression) extends VoidExpression {
  val evaluator: Evaluator[Void.type] = {
    lazy val bodyEvaluator = body.evaluator
    val conditionEvaluator = condition.evaluator
    lazy val loop: Evaluator[Void.type] = {
      conditionEvaluator.flatMap { condition =>
        if(condition.value) {
          bodyEvaluator.flatMap { _=>
            loop
          }
        } else {
          State.state( Void )
        }
      }
    }
    loop
  }
}
case class Sequence(exps: VoidExpression*) extends VoidExpression {

  val evaluator: Evaluator[Void.type] =
    for {
      _ <- Traverse[List].traverseS_( exps.toList ) (_.evaluator)
    } yield Void

}

sealed trait BinaryOp extends NumberExpression {
  def left: NumberExpression
  def right: NumberExpression
  def combinedWith(f: (NumberValue, NumberValue) => NumberValue) = {
    for {
      l <- left.evaluator
      r <- right.evaluator
    } yield f(l,r)
  }
}
case class Add(left: NumberExpression, right: NumberExpression) extends BinaryOp {
  val evaluator = combinedWith(_ + _)
}

case class Multiply(
                     left: NumberExpression,
                     right: NumberExpression) extends BinaryOp {
  val evaluator = combinedWith(_ * _)
}

case class Number(value: NumberValue) extends NumberExpression with Literal[NumberValue] {
}

object Number {

  def apply(float: Float): Number = new Number(NumberValue(float))

}

trait Variable[T<:Value] extends Expression[T] {
  def name: String
  val evaluator: Evaluator[T] = State.gets[Environment, T] { environment =>
    environment(name).asInstanceOf[T]
  }
}
case class NumberVariable(name: String) extends Variable[NumberValue] with NumberExpression
case class BooleanVariable(name: String) extends Variable[BooleanValue]

object Variable {

  def Number(name: String) = NumberVariable(name)
  def Boolean(name: String) = BooleanVariable(name)

}