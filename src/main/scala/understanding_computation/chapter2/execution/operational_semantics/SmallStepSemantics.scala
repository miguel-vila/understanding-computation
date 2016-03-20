package understanding_computation.chapter2.execution.operational_semantics

import understanding_computation.chapter2.ast._
import understanding_computation.chapter2.execution.Interpreter

/**
 * Created by mglvl on 5/10/15.
 */
object SmallStepSemantics extends Interpreter {

  def evaluateWithEnvironment[T<:Value](environment: Environment, expression: Expression[T]): (Environment,T) = {
    var result: Option[Literal[T]] = None
    var exp = expression
    var env = environment
    while(result.isEmpty) {
      val (_env, _exp) = reduceExp(exp, env)
      env = _env
      exp = _exp
      exp match {
        case literal: Literal[T] => result = Some(literal)
        case _ =>
      }
    }
    (env, result.get.value)
  }

  def isReducible[T<:Value](exp: Expression[T]): scala.Boolean = exp match {
    case _: Literal[T]  => false
    case _              => true
  }

  def reduceExp[T<:Value](exp: Expression[T], environment: Environment): (Environment, Expression[T]) = exp match {
    case variable: Variable[T] => (environment, new Literal[T]{
      def value = environment(variable.name).asInstanceOf[T]
    })
    case literal: Literal[T] => (environment, literal)
    case voidExpression: VoidExpression => reduceVoidExpression(voidExpression, environment)
    case numberExpression: NumberExpression => (environment, reduceNumberExpression(numberExpression, environment))
    case booleanExpression: BooleanExpression => (environment, reduceBooleanExpression(booleanExpression, environment))
  }

  def reduceVoidExpression(voidExpression: VoidExpression, environment: Environment): (Environment, VoidExpression) = voidExpression match {
    case assign: Assign => reduceAssign(assign, environment)
    case _if: If => reduceIf(_if, environment)
    case _while: While => reduceWhile(_while, environment)
    case sequence: Sequence => reduceSequence(sequence, environment)
    case DoNothing => (environment, DoNothing)
  }

  def reduceWhile(_while: While, environment: Environment): (Environment, VoidExpression) = {
    (environment, If(_while.condition, Sequence(_while.body, _while), DoNothing))
  }

  def reduceAssign(assign: Assign, environment: Environment): (Environment, VoidExpression) = assign match {
    case Assign(name, literal: Literal[_])  =>
      val newEnv = environment + (name -> literal.value)
      (newEnv, DoNothing)
    case Assign(name, exp) =>
      val (newEnv, reduced) = reduceExp(exp, environment)
      (newEnv, Assign(name, reduced))
  }

  def reduceSequence(sequence: Sequence, environment: Environment): (Environment, VoidExpression) = {
    if(sequence.exps.isEmpty) {
      (environment, DoNothing)
    } else {
      val first = sequence.exps.head
      val rest = sequence.exps.tail
      first match {
        case DoNothing => (environment, Sequence(rest:_*))
        case voidExpression =>
          val (env, exp) = reduceVoidExpression(voidExpression, environment)
          (env, Sequence((Seq(exp) ++ rest).toSeq:_*))
      }
    }
  }

  def reduceIf(_if: If, environment: Environment): (Environment, VoidExpression) = _if match {
    case If(Boolean(BooleanValue(true)), cons, _) => (environment, cons)
    case If(Boolean(BooleanValue(false)), _, alt) => (environment, alt)
    case If(cond,cons,alt) => (environment, If(reduceBooleanExpression(cond, environment), cons, alt))
  }

  def reduceBooleanExpression(booleanExpression: BooleanExpression, environment: Environment): BooleanExpression = booleanExpression match {
    case lessThan: LessThan => reduceLessThan(lessThan, environment)
    case greaterThan: GreaterThan => reduceGreaterThan(greaterThan, environment)
    case equal: Equal => reduceEqual(equal, environment)
    case boolean: Boolean => boolean
  }

  def reduceLessThan(lessThan: LessThan, environment: Environment): BooleanExpression = lessThan match {
    case LessThan(Number(left), Number(right)) => Boolean(left < right)
    case LessThan(left, right) if isReducible(left) => LessThan(reduceNumberExpression(left, environment), right)
    case LessThan(left, right) if isReducible(right) => LessThan(left, reduceNumberExpression(right, environment))
    case LessThan(left, right) => LessThan(reduceNumberExpression(left, environment), reduceNumberExpression(right, environment))
  }

  def reduceEqual(equal: Equal, environment: Environment): BooleanExpression = equal match {
    case Equal(Number(left), Number(right))        => Boolean(left eq right)
    case Equal(left, right) if isReducible(left)   => Equal(reduceNumberExpression(left, environment), right)
    case Equal(left, right) if isReducible(right)  => Equal(left, reduceNumberExpression(right, environment))
  }

  def reduceGreaterThan(greaterThan: GreaterThan, environment: Environment): BooleanExpression = greaterThan match {
    case GreaterThan(Number(left), Number(right))        => Boolean(left > right)
    case GreaterThan(left, right) if isReducible(left)   => GreaterThan(reduceNumberExpression(left, environment), right)
    case GreaterThan(left, right) if isReducible(right)  => GreaterThan(left, reduceNumberExpression(right, environment))
    case GreaterThan(left, right)                        => GreaterThan(reduceNumberExpression(left, environment), reduceNumberExpression(right, environment))
  }

  def reduceNumberExpression(numberExpression: NumberExpression, environment: Environment): NumberExpression = numberExpression match {
    case NumberVariable(name)             => Number(environment(name).asInstanceOf[NumberValue])
    case binaryOp: BinaryOp               => reduceBinaryOp(binaryOp, environment)
    case number: Number                   => number
  }

  def reduceBinaryOp(binaryOp: BinaryOp, environment: Environment): NumberExpression = binaryOp match {
    case Add(Number(left), Number(right))             => Number(left+right)
    case Multiply(Number(left), Number(right))        => Number(left*right)
    case Add(left,right)  if(isReducible(left))       => Add(reduceNumberExpression(left, environment), right)
    case Add(left,right)  if(isReducible(right))      => Add(left,reduceNumberExpression(right, environment))
    case Add(left,right)                              => Add(reduceNumberExpression(left, environment),reduceNumberExpression(right, environment))
    case Multiply(left,right)  if(isReducible(left))  => Multiply(reduceNumberExpression(left, environment), right)
    case Multiply(left,right)  if(isReducible(right)) => Multiply(left,reduceNumberExpression(right, environment))
    case Multiply(left,right)                         => Multiply(reduceNumberExpression(left, environment),reduceNumberExpression(right, environment))
  }

}
