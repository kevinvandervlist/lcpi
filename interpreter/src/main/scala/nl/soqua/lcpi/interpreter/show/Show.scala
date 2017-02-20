package nl.soqua.lcpi.interpreter.show

import scala.language.implicitConversions

// Modeled after GHC's Show
// https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Show.html#showString
sealed trait Show

object Show {
  type ShowS = StringBuilder => StringBuilder

  implicit def showS2string(f: ShowS): String = f(StringBuilder.newBuilder).toString().reverse

  implicit def str2s(s: String): ShowS = string(s)

  def empty: ShowS = identity

  def string(s: String): ShowS = _.append(s.reverse)

  def char(s: Char): ShowS = _.append(s)

  def parenthesize: ShowS => ShowS = char('(') compose _ compose char(')')
}
