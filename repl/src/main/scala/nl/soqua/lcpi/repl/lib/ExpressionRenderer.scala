package nl.soqua.lcpi.repl.lib

import nl.soqua.lcpi.ast.lambda.Expression
import nl.soqua.lcpi.interpreter.show.Show
import nl.soqua.lcpi.interpreter.transformation.Stringify
import nl.soqua.lcpi.interpreter.{InterpreterResult, SingleExpressionInterpreterResult, TraceInterpreterResult}

trait ExpressionRenderer {

  import Show._

  protected val lb: String = System.lineSeparator()

  def mightReplaceLambda(toggle: AsciiModeToggle): String => String = toggle match {
    case Disabled => identity
    case Enabled => _ replaceAll("λ", "\\\\\\\\")
  }

  private def renderInterpreterResult(interpreterResult: InterpreterResult): String =
    renderTrace(interpreterResult) compose Stringify(interpreterResult.expression)

  private def traceFold(acc: ShowS, tuple: (String, Expression)) =
    acc compose tuple._1 compose " => " compose Stringify(tuple._2) compose lb

  private def renderTrace(interpreterResult: InterpreterResult): ShowS = interpreterResult match {
    case SingleExpressionInterpreterResult(_, _) => empty
    case TraceInterpreterResult(_, _, trace) => trace.foldLeft(empty)(traceFold)
  }

  def renderEvaluationResult(asciiModeToggle: AsciiModeToggle)(interpreterResult: InterpreterResult): String =
    mightReplaceLambda(asciiModeToggle)(renderInterpreterResult(interpreterResult))

}
