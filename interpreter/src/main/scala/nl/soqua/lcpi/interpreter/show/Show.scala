package nl.soqua.lcpi.interpreter.show

import scala.language.{implicitConversions, postfixOps}

// Modeled after GHC's Show
// https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Show.html#showString
sealed trait Show {
  protected def append(c: Char): Show

  protected def append(s: String): Show

  protected def build: String
}

object Show {
  type ShowS = Show => Show

  implicit def showS2string(f: ShowS): String = f(new ShowImpl) build

  implicit def str2s(s: String): ShowS = string(s)

  def empty: ShowS = identity

  def string(s: String): ShowS = _ append s

  def char(s: Char): ShowS = _ append s

  def parenthesize: ShowS => ShowS = char('(') compose _ compose char(')')
}

/**
  * An efficient implementation of `Show` realized by internally leveraging a mutable string builder
  */
private class ShowImpl extends Show {
  private val builder = StringBuilder.newBuilder

  override protected def append(c: Char): Show = {
    builder.append(c)
    this
  }

  override protected def append(s: String): Show = {
    builder.append(s.reverse)
    this
  }

  override protected def build: String = builder.toString().reverse
}