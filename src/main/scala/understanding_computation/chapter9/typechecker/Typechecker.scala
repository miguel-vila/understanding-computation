package understanding_computation.chapter9.typechecker

import scalaz.ValidationNel
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.Validation._
import understanding_computation.chapter2.ast._

trait SimpleType
object VoidType extends SimpleType
object NumberType extends SimpleType
object BooleanType extends SimpleType

object Typechecker {

  type TypecheckResult = ValidationNel[String, SimpleType]
  type DefinedVarsTypes = Map[String, SimpleType]

  def typecheckNumberExp(numberExp: NumberExpression, definedVarsTypes: DefinedVarsTypes): TypecheckResult = {
    numberExp match {
      case binaryOp: BinaryOp => typecheckBinaryOp(binaryOp, definedVarsTypes)
      case number: Number => typecheckNumber(number)
      case numberVariable: NumberVariable => typecheckNumberVariable(numberVariable, definedVarsTypes)
    }
  }

  def typecheckBinaryOp(binaryOp: BinaryOp, definedVarsTypes: DefinedVarsTypes): TypecheckResult =
    (typecheckNumberExp(binaryOp.left, definedVarsTypes) |@| typecheckNumberExp(binaryOp.right, definedVarsTypes)).tupled.map(_ => NumberType)

  def typecheckNumber(number: Number): TypecheckResult =
    NumberType.success

  def typecheckNumberVariable(numberVariable: NumberVariable, definedVarsTypes: DefinedVarsTypes): TypecheckResult = definedVarsTypes.get(numberVariable.name) match {
    case Some(NumberType) => NumberType.success
    case Some(otherType)  => s"Expected the var ${numberVariable.name} to be a number but instead it's a $otherType".failureNel
    case None => s"The variable ${numberVariable.name} is not defined.".failureNel
  }

}
