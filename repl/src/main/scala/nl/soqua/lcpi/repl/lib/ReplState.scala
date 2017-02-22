package nl.soqua.lcpi.repl.lib

import nl.soqua.lcpi.interpreter.Context

sealed trait TraceModePosition

sealed trait AsciiModeToggle

case object Enabled extends TraceModePosition with AsciiModeToggle

case object Disabled extends TraceModePosition with AsciiModeToggle

object ReplState {
  val empty: ReplState = ReplState(CombinatorLibrary.loadIn(Context()), traceMode = Disabled, asciiMode = Disabled,
    terminated = false, contextIsMutable = false, List.empty)
}

case class ReplState(context: Context, traceMode: TraceModePosition, asciiMode: AsciiModeToggle, terminated: Boolean,
                     contextIsMutable: Boolean, reloadableFiles: List[String])
