package understanding_computation.chapter2.ast

trait Value
object Void extends Value
case class NumberValue(value: Float) extends Value {

  def +(other: NumberValue): NumberValue = NumberValue(value + other.value)
  def *(other: NumberValue): NumberValue = NumberValue(value * other.value)
  def <(other: NumberValue): BooleanValue = BooleanValue(value < other.value)

}

case class BooleanValue(value: scala.Boolean) extends Value