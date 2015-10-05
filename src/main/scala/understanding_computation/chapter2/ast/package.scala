package understanding_computation.chapter2

package object ast {

  type Environment = Map[String, Value]

  type NumberExpression = Expression[NumberValue]

  type BooleanExpression = Expression[BooleanValue]

  type VoidExpression = Expression[Void.type]

}
