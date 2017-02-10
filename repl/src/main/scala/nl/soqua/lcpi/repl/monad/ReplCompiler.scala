package nl.soqua.lcpi.repl.monad

import cats.data.State
import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.{Expression, Variable}
import nl.soqua.lcpi.interpreter._
import nl.soqua.lcpi.interpreter.transformation.Stringify
import nl.soqua.lcpi.parser.repl.ReplParser
import nl.soqua.lcpi.repl.Messages
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO}
import nl.soqua.lcpi.repl.monad.ReplCompilerDefinition.PureReplState

import scala.collection.mutable
import scala.util.{Failure, Success}

trait ReplCompiler extends ReplCompilerDefinition with DiskIO {

  private val lb: String = System.lineSeparator()

  private def renderEvaluationResult(interpreterResult: InterpreterResult): String = interpreterResult match {
    case SingleExpressionInterpreterResult(_, resultExpression) => Stringify(resultExpression)
    case TraceInterpreterResult(_, resultExpression, trace) =>
      val builder = trace.foldLeft(mutable.StringBuilder.newBuilder)((acc, t) => acc.append(s"${t._1} => ${Stringify(t._2)}$lb"))
      builder.append(Stringify(resultExpression)).toString()
  }

  private def renderContext(acc: mutable.StringBuilder, v: Variable, e: Expression): mutable.StringBuilder =
    acc.append(f"${Stringify(v)}%10s := ${Stringify(e)}%s$lb%s")

  private def loadContextFromFile(stream: Stream[String], ctx: Context): Context = stream
    .filterNot(l => l.startsWith("#")) // Skip 'comments'
    .map(l => ReplParser(l))
    .collect {
      case Right(Assignment(v, e)) => (v, e)
    }
    .foldLeft(ctx)((acc, t) => acc.assign(t._1, t._2))

  override protected def help(): PureReplState[String] =
    State.pure(Messages.help)

  override protected def quit(): PureReplState[Unit] =
    State.modify(s => s.copy(terminated = true))

  override protected def show(): PureReplState[String] =
    State.inspect(s => s.context.foldLeft(mutable.StringBuilder.newBuilder)(renderContext).toString().dropRight(lb.length))

  override protected def reset(): PureReplState[Unit] =
    State.modify(s => s.copy(context = CombinatorLibrary.loadIn(Context())))

  override protected def trace(): PureReplState[String] = State(s => s.traceMode match {
    case Enabled => (s.copy(traceMode = Disabled), Messages.traceModeDisabled)
    case Disabled => (s.copy(traceMode = Enabled), Messages.traceModeEnabled)
  })

  override protected def load(file: String): PureReplState[String] = readFile(file) match {
    case Failure(ex) => State.pure(s"Failed to load `$file`: ${ex.getMessage}")
    case Success(stream) => State(s => {
      (s.copy(context = loadContextFromFile(stream, s.context), reloadableFile = Some(file)), s"Successfully loaded file `$file`")
    })
  }

  override protected def reload(): PureReplState[String] = State(s => {
    s.reloadableFile match {
      case None => (s, "Failed to reload: no file has been loaded yet")
      case Some(path) => readFile(path) match {
        case Failure(ex) => (s, s"Failed to reload `$path`: ${ex.getMessage}")
        case Success(stream) => (s.copy(context = loadContextFromFile(stream, s.context)), s"Successfully reloaded file `$path`")
      }
    }
  })

  override protected def command(expression: ReplExpression): PureReplState[String] = State(s => {
    val fn: (Context, ReplExpression) => Either[InterpreterError, InterpreterResult] = s.traceMode match {
      case Disabled => Interpreter.apply
      case Enabled => Interpreter.trace
    }
    fn(s.context, expression) match {
      case Left(error) => (s, error.message)
      case Right(result) => (s.copy(context = result.context), renderEvaluationResult(result))
    }
  })
}
