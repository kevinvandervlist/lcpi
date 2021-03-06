package nl.soqua.lcpi.repl.monad

import cats.data.State
import nl.soqua.lcpi.ast.interpreter.ReplExpression
import nl.soqua.lcpi.ast.lambda.{Expression, Variable}
import nl.soqua.lcpi.interpreter._
import nl.soqua.lcpi.interpreter.transformation.{DeBruijn, Stringify}
import nl.soqua.lcpi.repl.Messages
import nl.soqua.lcpi.repl.lib._
import nl.soqua.lcpi.repl.monad.ReplCompilerDefinition.PureReplState
import nl.soqua.lcpi.repl.monad.ReplMonad.Repl
import nl.soqua.lcpi.repl.parser.StdInParser

import scala.util.{Failure, Success}

trait ReplCompiler extends ReplCompilerDefinition with DiskIO with ExpressionRenderer {

  override protected def help(): PureReplState[String] =
    State.pure(Messages.help)

  override protected def quit(): PureReplState[Unit] =
    State.modify(s => s.copy(terminated = true))

  override protected def show(): PureReplState[String] =
    State.inspect(s => mightReplaceLambda(s.asciiMode)(s.context.map(renderContext).mkString(lb)))

  override protected def reset(): PureReplState[Unit] =
    State.modify(_ => ReplState.empty)

  override protected def trace(): PureReplState[String] = State(s => s.traceMode match {
    case Enabled => (s.copy(traceMode = Disabled), Messages.traceModeDisabled)
    case Disabled => (s.copy(traceMode = Enabled), Messages.traceModeEnabled)
  })

  override protected def ascii(): PureReplState[String] = State(s => s.asciiMode match {
    case Enabled => (s.copy(asciiMode = Disabled), Messages.asciiModeDisabled)
    case Disabled => (s.copy(asciiMode = Enabled), Messages.asciiModeEnabled)
  })

  override protected def load(file: String): PureReplState[String] = readFile(file) match {
    case Failure(ex) => State.pure(s"Failed to load `$file`: ${ex.getMessage}")
    case Success(stream) => State(s => {
      if (s.reloadableFiles.contains(file)) {
        (s, s"File `$file` is already loaded.")
      } else {
        streamCommands(stream).foldMap(compile).run(s).value match {
          case (newS, _) => (newS.copy(reloadableFiles = newS.reloadableFiles :+ file), s"Successfully loaded file `$file`")
        }
      }
    })
  }

  override protected def reload(): PureReplState[String] = State(state => {
    if (state.reloadableFiles.isEmpty) {
      (state, "Failed to reload: no file has been loaded yet")
    } else {
      val s = state.copy(contextIsMutable = true)
      val newContext = s.reloadableFiles.foldLeft((s.context, List.empty[String]))((acc, file) => acc match {
        case (ctx, out) => readFile(file) match {
          case Failure(ex) => (ctx, out :+ s"Failed to reload `$file`: ${ex.getMessage}")
          case Success(stream) =>
            val program = streamCommands(stream)
            program.foldMap(compile).run(s).value match {
              case (newS, _) => (newS.context, out ++ List(s"Successfully reloaded file `$file`"))
            }
        }
      })
      (state.copy(context = newContext._1), newContext._2.mkString(lb))
    }
  })

  override protected def replExpression(expression: ReplExpression): PureReplState[String] = State(s => {
    interpreterFunction(s)(s.context, expression) match {
      case Left(error) => (s, error.message)
      case Right(result) => (s.copy(context = result.context), renderEvaluationResult(s.asciiMode)(result))
    }
  })

  override protected def deBruijnIndex(expression: ReplExpression): PureReplState[String] = State(s => {
    interpreterFunction(s)(s.context, expression) match {
      case Left(error) => (s, error.message)
      case Right(result) => (s, renderEvaluationResult(s.asciiMode)(result.map(DeBruijn.index)))
    }
  })

  private def renderContext(v: Variable, e: Expression): String =
    f"${Stringify(v)}%10s := ${Stringify(e)}%s"

  private def streamCommands(stream: Stream[String]): Repl[_] = stream
    .filterNot(l => l.startsWith("#")) // Skip 'comments'
    .map(l => StdInParser(l))
    .collect {
      case Right(cmd) => cmd
    }
    .foldRight(ReplMonad.nothing())((v, acc) => v match {
      case cmd => cmd.flatMap(_ => acc)
    })

  private def interpreterFunction(s: ReplState): (Context, ReplExpression) => Either[InterpreterError, InterpreterResult] =
    s.traceMode match {
      case _ if s.contextIsMutable => Interpreter.mutableApply
      case Disabled => Interpreter.apply
      case Enabled => Interpreter.trace
    }
}
