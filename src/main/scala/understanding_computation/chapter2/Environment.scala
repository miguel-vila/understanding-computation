package understanding_computation.chapter2

import understanding_computation.chapter2.ast.Value

case class Environment(
                      variables: Map[String, Value],
                      ioEffects: List[IOEffect]
                      ) {

  def putVariable(name: String, value: Value): Environment = {
    copy(variables = variables + (name -> value))
  }

  def addEffect(ioEffect: IOEffect): Environment = {
    copy( ioEffects = ioEffects ++ List(ioEffect) )
  }

  def getVariableValue[V <: Value](name: String): V = {
    variables(name).asInstanceOf[V]
  }

}

object Environment {

  def apply(): Environment = Environment(Map.empty, List.empty)

}