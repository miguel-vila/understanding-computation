package understanding_computation.chapter2.execution.operational_semantics

import org.scalatest.{Matchers, FlatSpec}
import understanding_computation.chapter2.ast._

class BigStepSemanticsTest extends FlatSpec with Matchers {
  import BigStepSemantics._

  "BigStepSemantics" should "evaluate boolean literal expressions" in {
    evaluate(Boolean.True) should be (BooleanValue(true))
    evaluate(Boolean.False) should be (BooleanValue(false))
  }

  it should "evaluate number literal expressions" in {
    evaluate(Number(123.456f)) should be (NumberValue(123.456f))
  }

  it should "modify the environment when making assignments" in {
    var (newEnv,_) = evaluateWithEnvironment(Assign("foo",Number(3)))(Map.empty)
    newEnv.get("foo") should be (Some(NumberValue(3)))
  }

  it should "override existing values when making assignments for existing variables" in {
    var (newEnv,_) = evaluateWithEnvironment(Assign("foo",Number(2)))(Map("foo"->NumberValue(1)))
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
    val (_,result) = evaluateWithEnvironment(Add(Variable.Number("foo"),Variable.Number("bar")))(env)
    result should be (NumberValue(57))
  }

  it should "multiply numbers where the numbers are variables" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (_,result) = evaluateWithEnvironment(Multiply(Variable.Number("foo"),Variable.Number("bar")))(env)
    result should be (NumberValue(540))
  }

  it should "evaluate 'less than' expressions" in {
    evaluate(LessThan(Number(1),Number(2))) should be (BooleanValue(true))
    evaluate(LessThan(Number(1),Number(1))) should be (BooleanValue(false))
    evaluate(LessThan(Number(10),Number(5))) should be (BooleanValue(false))
  }

  it should "evaluate if expressions (1)" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (newEnv,_) = evaluateWithEnvironment(
      If(
        LessThan(Number(1),Number(2)),
        Assign("foo", Number(34)),
        Assign("bar", Number(56))
      ))(env)
    newEnv should be (Map("foo"->NumberValue(34),"bar"->NumberValue(45)))
  }

  it should "evaluate if expressions (2)" in {
    val env = Map("foo"->NumberValue(12),"bar"->NumberValue(45))
    val (newEnv,_) = evaluateWithEnvironment(
      If(
        LessThan(Number(3),Number(2)),
        Assign("foo", Number(34)),
        Assign("bar", Number(56))
      ))(env)
    newEnv should be (Map("foo"->NumberValue(12),"bar"->NumberValue(56)))
  }

}
