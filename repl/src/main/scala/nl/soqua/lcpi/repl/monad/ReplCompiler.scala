package nl.soqua.lcpi.repl.monad

import cats.~>
import nl.soqua.lcpi.ast.interpreter.{Assignment, ReplExpression}
import nl.soqua.lcpi.ast.lambda.{Expression, Variable}
import nl.soqua.lcpi.interpreter._
import nl.soqua.lcpi.interpreter.transformation.Stringify
import nl.soqua.lcpi.parser.repl.ReplParser
import nl.soqua.lcpi.repl.Messages
import nl.soqua.lcpi.repl.lib.{CombinatorLibrary, DiskIO}

import scala.collection.mutable
import scala.util.{Failure, Success}

object ReplCompiler {

  import cats.data.State

  type PureReplState[A] = State[ReplState, A]

  type alias = ReplMonadA ~> PureReplState

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

  def compiler(disk: DiskIO): alias = new (alias) {
    def apply[A](fa: ReplMonadA[A]): PureReplState[A] = {
      fa match {
        case Help => State.pure(Messages.help);
        case Quit => State.modify(s => s.copy(terminated = true))
        case Reset => State.modify(s => s.copy(context = CombinatorLibrary.loadIn(Context())))
        case Show => State.inspect(s => s.context.foldLeft(mutable.StringBuilder.newBuilder)(renderContext).toString().dropRight(lb.length))
        case TraceMode => State(s => s.traceMode match {
          case Enabled => (s.copy(traceMode = Disabled), Messages.traceModeDisabled)
          case Disabled => (s.copy(traceMode = Enabled), Messages.traceModeEnabled)
        })
        case Command(e) =>
          State(s => {
            val fn: (Context, ReplExpression) => Either[InterpreterError, InterpreterResult] = s.traceMode match {
              case Disabled => Interpreter.apply
              case Enabled => Interpreter.trace
            }
            fn(s.context, e) match {
              case Left(error) => (s, error.message)
              case Right(result) => (s.copy(context = result.context), renderEvaluationResult(result))
            }
          })
        case LoadFile(path) => disk.load(path) match {
          case Failure(ex) => State.pure(s"Failed to load `$path`: ${ex.getMessage}")
          case Success(stream) => State(s => {
            (s.copy(context = loadContextFromFile(stream, s.context), reloadableFile = Some(path)), s"Successfully loaded file `$path`")
          })
        }
        case Reload => State(s => {
          s.reloadableFile match {
            case None => (s, "Failed to reload: no file has been loaded yet")
            case Some(path) => disk.load(path) match {
              case Failure(ex) => (s, s"Failed to reload `$path`: ${ex.getMessage}")
              case Success(stream) => (s.copy(context = loadContextFromFile(stream, s.context)), s"Successfully reloaded file `$path`")
            }
          }
        })
      }
    }
  }
}
