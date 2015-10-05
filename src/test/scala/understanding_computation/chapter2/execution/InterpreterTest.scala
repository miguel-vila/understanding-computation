package understanding_computation.chapter2.execution

import org.scalatest.{FlatSpec, Matchers}
import understanding_computation.chapter2.ast._

class InterpreterTest(interpreter: Interpreter) extends FlatSpec with Matchers {
  import interpreter._

  s"${interpreter.getClass.getSimpleName}" should "evaluate boolean literal expressions" in {
    evaluate(Boolean.True) should be (BooleanValue(true))
    evaluate(Boolean.False) should be (BooleanValue(false))
  }

  it should "evaluate number literal expressions" in {
    evaluate(Number(123.456f)) should be (NumberValue(123.456f))
  }

  it should "modify the environment when making assignments" in {
    var (newEnv,_) = evaluateWithEnvironment(Map.empty, Assign("foo",Number(3)))
    newEnv.get("foo") should be (Some(NumberValue(3)))
  }

  it should "override existing values when making assignments for existing variables" in {
    var (newEnv,_) = evaluateWithEnvironment(Map("foo"->NumberValue(1)), Assign("foo",Number(2)))
    newEnv.get("foo") should be (Some(NumberValue(2)))
  }

  it should "add numbers" in {
    evaluate(Add(Number(12),Number(45))) should be (NumberValue(57))
  }

  it should "multiply numbers" in {
    evaluate(Multiply(Number(12),Number(45))) should be (NumberValue(540))
  }

  it should "add numbers where the numbers are variables" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (_,result) = evaluateWithEnvironment(env, Add(Variable.Number("foo"),Variable.Number("bar")))
    result should be (NumberValue(57))
  }

  it should "multiply numbers where the numbers are variables" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (_,result) = evaluateWithEnvironment(env, Multiply(Variable.Number("foo"),Variable.Number("bar")))
    result should be (NumberValue(540))
  }

  it should "evaluate 'less than' expressions" in {
    evaluate(LessThan(Number(1),Number(2))) should be (BooleanValue(true))
    evaluate(LessThan(Number(1),Number(1))) should be (BooleanValue(false))
    evaluate(LessThan(Number(10),Number(5))) should be (BooleanValue(false))
  }

  it should "evaluate 'less than' expressions with variables" in {
    val env = Map("foo"->NumberValue(0))
    val (_,result) = evaluateWithEnvironment(env, LessThan(Variable.Number("foo"), Number(2)))
    result should be (BooleanValue(true))
  }

  it should "evaluate if expressions (1)" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (newEnv,_) = evaluateWithEnvironment(env,
      If(
        LessThan(Number(1),Number(2)),
        Assign("foo", Number(34)),
        Assign("bar", Number(56))
      ))
    newEnv should be (Map("foo"->NumberValue(34),"bar"->NumberValue(45)))
  }

  it should "evaluate if expressions (2)" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (newEnv,_) = evaluateWithEnvironment(env,
      If(
        LessThan(Number(3),Number(2)),
        Assign("foo", Number(34)),
        Assign("bar", Number(56))
      ))
    newEnv should be (Map("foo"->NumberValue(12),"bar"->NumberValue(56)))
  }

  it should "evaluate while expressions" in {
    val env = Map("foo"->NumberValue(0))
    val (newEnv,_) = evaluateWithEnvironment(env,
      While(
        LessThan(Variable.Number("foo"), Number(2)),
        Assign("foo", Add(Variable.Number("foo"), Number(1)))
      ))

  }

  it should "evaluate sequences of expressions (1)" in {
    val (newEnv,_) = evaluateWithEnvironment(Map.empty,
      Sequence(
        Assign("foo", Number(12)),
        Assign("bar", Number(34)),
        Assign("baz", Number(45)),
        Assign("foo", Number(56)),
        Assign("baz", Number(67))
      )
    )
    newEnv should be (Map(
      "foo"->NumberValue(56),
      "bar"->NumberValue(34),
      "baz"->NumberValue(67)
    ))
  }

}
