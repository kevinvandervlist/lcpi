package nl.soqua.lcpi.repl.monad

import cats.data.State
import cats.~>
import nl.soqua.lcpi.ast.interpreter.ReplExpression
import nl.soqua.lcpi.repl.lib.ReplState
import nl.soqua.lcpi.repl.monad.ReplCompilerDefinition.{PureReplState, alias}

object ReplCompilerDefinition {

  import cats.data.State

  type PureReplState[A] = State[ReplState, A]

  type alias = ReplMonadA ~> PureReplState
}

trait ReplCompilerDefinition {

  val compile: alias = new (alias) {
    def apply[A](fa: ReplMonadA[A]): PureReplState[A] = fa match {
      case Nothing => nothing()
      case Help => help()
      case Quit => quit()
      case Reset => reset()
      case ShowContext => show()
      case TraceMode => trace()
      case AsciiMode => ascii()
      case EvaluateReplExpression(e) => replExpression(e)
      case LoadFile(file) => load(file)
      case Reload => reload()
      case DeBruijnIndex(e) => deBruijnIndex(e)
    }
  }

  private def nothing(): PureReplState[Unit] = State.modify(s => s)

  protected def help(): PureReplState[String]

  protected def quit(): PureReplState[Unit]

  protected def show(): PureReplState[String]

  protected def reset(): PureReplState[Unit]

  protected def trace(): PureReplState[String]

  protected def ascii(): PureReplState[String]

  protected def load(file: String): PureReplState[String]

  protected def reload(): PureReplState[String]

  protected def replExpression(expression: ReplExpression): PureReplState[String]

  protected def deBruijnIndex(expression: ReplExpression): PureReplState[String]
}
