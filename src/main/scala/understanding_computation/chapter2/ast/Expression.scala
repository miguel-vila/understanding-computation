package understanding_computation.chapter2.ast

import scala.reflect.ClassTag

sealed trait Expression[T<:Value]
case class Add(left: NumberExpression, right: NumberExpression) extends NumberExpression
case class Boolean(value: BooleanValue) extends BooleanExpression
object Boolean {
  def apply(boolean: scala.Boolean): Boolean = Boolean(BooleanValue(boolean))
  def True: Boolean = apply(true)
  def False: Boolean = apply(false)
}

case object DoNothing extends VoidExpression

case class Assign[T<:Value](name: String, expression: Expression[T]) extends Expression[Void.type]

case class If(
               condition:    BooleanExpression,
               consequence:  Expression[_<:Value],
               alternative:  Expression[_<:Value]) extends VoidExpression

case class LessThan(left: NumberExpression, right: NumberExpression) extends BooleanExpression

case class Multiply(
                     left: NumberExpression,
                     right: NumberExpression) extends NumberExpression

case class Number(value: NumberValue) extends Expression[NumberValue]

object Number {

  def apply(float: Float): Number = new Number(NumberValue(float))

}

case class Sequence(exps: VoidExpression*) extends VoidExpression

case class Variable[T<:Value](name: String)(implicit ct: ClassTag[T]) extends Expression[T]

object Variable {

  def Number(name: String) = Variable[NumberValue](name)
  def Boolean(name: String) = Variable[BooleanValue](name)

}