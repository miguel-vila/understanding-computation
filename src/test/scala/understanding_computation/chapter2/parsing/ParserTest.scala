package understanding_computation.chapter2.parsing

import org.scalatest.{Matchers, FlatSpec}
import understanding_computation.chapter2.ast._

class ParserTest extends FlatSpec with Matchers {
  import Parser._

  "A Parser" should "identify var names" in {
    parseAll(varName, "asdf").get should be ("asdf")
    parseAll(varName, "  asdf  ").get should be ("asdf")
  }

  it should "not identyify reserved words" in {
    parseAll(varName, "if").successful should be (false)
    parseAll(varName, "true").successful should be (false)
    parseAll(varName, "false").successful should be (false)
  }

  it should "identify boolean literals" in {
    parseAll(booleanLiteral, "true").get should be (Boolean.True)
    parseAll(booleanLiteral, "false").get should be (Boolean.False)
    parseAll(booleanLiteral, "asdfaf").successful should be (false)
  }

  it should "identify numbers" in {
    parseAll(number, "123").get should be (Number(123))
    parseAll(number, "-123").get should be (Number(-123))
    parseAll(number, "123.456").get should be (Number(123.456f))
    parseAll(number, "-123.456").get should be (Number(-123.456f))
    parseAll(number, " 123").get should be (Number(123))
    parseAll(number, " 12  ").get should be (Number(12))
    parseAll(number, " 12  23").successful should be (false)
    parseAll(number, "asdf").successful should be (false)
  }

  it should "identify term expressions" in {
    parseAll(term, "1*3").get should be (Multiply(Number(1),Number(3)))
    parseAll(term, "1*foo").get should be (Multiply(Number(1),Variable.Number("foo")))
    parseAll(term, "foo*bar").get should be (Multiply(Variable.Number("foo"),Variable.Number("bar")))
    parseAll(term, "1*2*3").get should be (Multiply(Multiply(Number(1),Number(2)), Number(3)))
    parseAll(term, "1*bar*3").get should be (Multiply(Multiply(Number(1),Variable.Number("bar")), Number(3)))
    parseAll(term, "  1*2*3").get should be (Multiply(Multiply(Number(1),Number(2)), Number(3)))
    parseAll(term, "1 * 2 * 3").get should be (Multiply(Multiply(Number(1),Number(2)), Number(3)))
  }

  it should "identify arithmetic expressions" in {
    parseAll(arithmeticExpr, "1+2").get should be (Add(Number(1),Number(2)))
    parseAll(arithmeticExpr, "1+foo").get should be (Add(Number(1),Variable.Number("foo")))
    parseAll(arithmeticExpr, "1+2*3").get should be (Add(Number(1),Multiply(Number(2),Number(3))))
    parseAll(arithmeticExpr, "foo+2*3").get should be (Add(Variable.Number("foo"),Multiply(Number(2),Number(3))))
    parseAll(arithmeticExpr, "3*4+5*6").get should be (Add(Multiply(Number(3),Number(4)),Multiply(Number(5),Number(6))))
    parseAll(arithmeticExpr, "  3 * 4 + 5*6").get should be (Add(Multiply(Number(3),Number(4)),Multiply(Number(5),Number(6))))
    parseAll(arithmeticExpr, "  3 * bar + 5*6").get should be (Add(Multiply(Number(3),Variable.Number("bar")),Multiply(Number(5),Number(6))))
    parseAll(arithmeticExpr, "3*4+5*6+7*8").get should be (Add(
      Add(
        Multiply(Number(3),Number(4)),
        Multiply(Number(5),Number(6))),
        Multiply(Number(7),Number(8))))
    parseAll(arithmeticExpr, "3*foo+5*bar+baz*7").get should be (Add(
      Add(
        Multiply(Number(3),Variable.Number("foo")),
        Multiply(Number(5),Variable.Number("bar"))),
        Multiply(Variable.Number("baz"),Number(7))))
  }

  it should "identify less than comparisons" in {
    parseAll(lessThan, "1<2").get should be(LessThan(Number(1),Number(2)))
    parseAll(lessThan, "1<foo").get should be(LessThan(Number(1),Variable.Number("foo")))
    parseAll(lessThan, "1<2<3").successful should be (false)
    parseAll(lessThan, "1*2").successful should be (false)
    parseAll(lessThan, " 1 < 2 * 3").get should be (LessThan(Number(1),Multiply(Number(2),Number(3))))
    parseAll(lessThan, "3*4<5*6").get should be (LessThan(Multiply(Number(3),Number(4)),Multiply(Number(5),Number(6))))
  }

  it should "identify assignments" in {
    parseAll(assign, "foo=1;").get should be(Assign("foo", Number(1)))
    parseAll(assign, "foo=bar+1;").get should be(Assign("foo", Add(Variable.Number("bar"), Number(1))))
    //parseAll(assign, "foo=bar;").get
    parseAll(assign, " foo = 1 ;").get should be(Assign("foo", Number(1)))
  }

  it should "identify if statements" in {
    parseAll(_if,
      "if( 1 < 3){foo = 1;}else{bar = 2;}"
    ).get should be (If(
      LessThan(Number(1),Number(3)),
      Assign("foo", Number(1)),
      Assign("bar", Number(2))
    ))
    parseAll(_if,
      """if( 1 < 3){
        |foo = 1;
        |}else{
        |bar = 2;
        |}""".stripMargin
    ).get should be (If(
      LessThan(Number(1),Number(3)),
      Assign("foo", Number(1)),
      Assign("bar", Number(2))
    ))
    val r = parseAll(_if,
      """if( 1 < 3) {
        | foo = 1;
        |} else {
        | bar = 2;
        | baz = 3;
        |}
        |""".stripMargin
    )
    println(s" r = $r")
    r.get should be (If(
      LessThan(Number(1),Number(3)),
      Assign("foo", Number(1)),
      Sequence(
        Assign("bar", Number(2)),
        Assign("baz", Number(3)))
    ))
  }

  it should "identify 'while' statements" in {
    parseAll(_while,
      """
        |while(foo < 2){
        | foo = foo + 1;
        |}
      """.stripMargin
    ).get should be (
      While(
        LessThan(Variable.Number("foo"), Number(2)),
        Assign("foo", Add(Variable.Number("foo"), Number(1)))))
    parseAll(_while,
      """
        |while(foo < 2){
        | foo = foo + 1;
        | baz = foo + 7;
        |}
      """.stripMargin
    ).get should be (
      While(
        LessThan(Variable.Number("foo"), Number(2)),
          Sequence(
            Assign("foo", Add(Variable.Number("foo"), Number(1))),
            Assign("baz", Add(Variable.Number("foo"), Number(7))))))
  }

  it should "identify sequences" in {
    parseAll(voidExpression,"foo = 1; bar = 3;").get should be (Sequence(Assign("foo",Number(1)),Assign("bar",Number(3))))
    parseAll(voidExpression,
      """foo = 1;
        |bar = 3;""".stripMargin
    ).get should be (Sequence(Assign("foo",Number(1)),Assign("bar",Number(3))))
    parseAll(voidExpression,
      """foo = 1;
        |bar = 3;
        |fuz = 4;
      """.stripMargin
    ).get should be (Sequence(Assign("foo",Number(1)),Assign("bar",Number(3)),Assign("fuz",Number(4))))
    parseAll(voidExpression,
      """foo = 1;
        |if( foo < 3 ) {
        | foo = foo +5;
        |} else {
        | bar = 4;
        |}
        |fuz = 4;
      """.stripMargin
    ).get should be (Sequence(
      Assign("foo",Number(1)),
      If(
        LessThan(Variable.Number("foo"),Number(3)),
        Assign("foo", Add(Variable.Number("foo"), Number(5))),
        Assign("bar", Number(4))
      ),
      Assign("fuz",Number(4))))
  }

}
