package nl.soqua.lcpi.repl.monad

import nl.soqua.lcpi.interpreter.Context
import nl.soqua.lcpi.repl.lib.CombinatorLibrary

private[monad] sealed trait TraceModePosition

private[monad] sealed trait AsciiModeToggle

private[monad] case object Enabled extends TraceModePosition with AsciiModeToggle

private[monad] case object Disabled extends TraceModePosition with AsciiModeToggle

object ReplState {
  val empty: ReplState = ReplState(CombinatorLibrary.loadIn(Context()), traceMode = Disabled, asciiMode = Disabled,
    terminated = false, contextIsMutable = false, List.empty)
}

case class ReplState(context: Context, traceMode: TraceModePosition, asciiMode: AsciiModeToggle, terminated: Boolean,
                     contextIsMutable: Boolean, reloadableFiles: List[String])
