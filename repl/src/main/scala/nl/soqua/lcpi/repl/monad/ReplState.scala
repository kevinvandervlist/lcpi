package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.lib.CombinatorLibrary

private[monad] sealed trait TraceModePosition

private[monad] case object Enabled extends TraceModePosition

private[monad] case object Disabled extends TraceModePosition

object ReplState {
  val empty: ReplState = ReplState(CombinatorLibrary.loadIn(Context()), traceMode = Disabled, terminated = false, None)
}

case class ReplState(context: Context, traceMode: TraceModePosition, terminated: Boolean, reloadableFile: Option[String])
