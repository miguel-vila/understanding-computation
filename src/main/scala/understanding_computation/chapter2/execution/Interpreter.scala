package understanding_computation.chapter2.execution

import understanding_computation.chapter2.ast._
import understanding_computation.chapter2.execution.operational_semantics.BigStepSemantics
import understanding_computation.chapter2.parsing.Parser
import Console.{ GREEN, RED, RESET }

import scala.io.Source.fromFile
import Parser.{ Success, NoSuccess }

trait Interpreter {
  def evaluate[T<:Value](expression: Expression[T]): T = {
    val (_, result) = evaluateWithEnvironment(Map.empty, expression)
    result
  }
  def evaluateWithEnvironment[T<:Value](environment: Environment, expression: Expression[T]): (Environment,T)
}

object InterpreterApp extends App {

  val interpreter: Interpreter = BigStepSemantics

  val source = fromFile("test1.simple")
  val content = try source.mkString finally source.close()
  Parser.parseToAST(content) match {
    case Success(ast,_)   =>
      val (newEnv, _) = interpreter.evaluateWithEnvironment(Map.empty, ast)
      print(GREEN)
      println(s"SIMPLE program succesfully run.")
      print(RESET)
      println("Final environment:")
      println(newEnv.toList.map { case (k,v) => s"~ $k => $v" }.mkString("\n"))
      print(RESET)
    case NoSuccess(msg,_) =>
      print(RED)
      println(s"ParseError: $msg")
      print(RESET)
  }

}
