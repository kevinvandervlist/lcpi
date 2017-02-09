package nl.soqua.lcpi.interpreter

import nl.soqua.lcpi.ast.interpreter.ReplExpression
import nl.soqua.lcpi.ast.interpreter.ReplExpression._
import nl.soqua.lcpi.ast.lambda.{Expression, Variable}
import nl.soqua.lcpi.interpreter.transformation.{DeBruijnIndex, Stringify}
import nl.soqua.lcpi.parser.repl.ReplParser
import org.scalatest.Matchers

import scala.language.implicitConversions

trait InterpreterTester {

  implicit def parseExpression(line: String): ReplExpression = {
    ReplParser(line) match {
      case Left(_) => Variable("x")
      case Right(e) => e
    }
  }

  /**
    * Create a mutable context for testing purposes
    *
    * @param ctx
    */
  protected implicit class MutableContext(private var ctx: Context) extends Context {
    override def foldLeft[T](seed: T)(op: (T, Variable, Expression) => T): T =
      ctx.foldLeft(seed)(op)

    override def foreach(fn: (Variable, Expression) => Unit): Unit =
      ctx.foreach(fn)

    override def assign(v: Variable, e: Expression): Context = {
      ctx = ctx.assign(v, e)
      ctx
    }

    override def contains(v: Variable): Boolean = ctx.contains(v)
  }

  /**
    * This class allows a convenient notation for test declarations
    *
    * @param expr The left hand side expression to parse. Yields the 'actual' result of the assertion.
    */
  protected implicit class InterpreterParserAndTester(val expr: String) extends Matchers {
    /**
      * Assert equality given an expression expr that will be compared to the 'actual' result
      * (e.g. the left hand side). Equality is determined by writing the expressions in De Bruijn Index form
      * for α-equivalence
      *
      * @param expectedExpression
      * @param ctx
      */
    def >>(expectedExpression: Expression)(implicit ctx: Context): Unit = {
      Interpreter(ctx, expr).fold(ex => {
        fail(s"Expression $expr failed: $ex")
      }, actualExpression => {
        withClue(
          s"""
             |expressions are not equal. l >> r:
             |got:      ${Stringify(actualExpression.expression)}
             |expected: ${Stringify(expectedExpression)}
             |---
          """.stripMargin) {
          DeBruijnIndex.index(actualExpression.expression) shouldBe DeBruijnIndex.index(expectedExpression)
        }
      })
    }

    def parse(implicit ctx: Context): (Context, ReplExpression) = {
      Interpreter(ctx, expr).fold(ex => {
        fail(s"Parse of expression $expr failed: $ex")
      }, e => (e.context, e.expression))
    }

    /**
      * Assert equality given an expression str that will be parsed and compared to the 'actual' result
      * (e.g. the left hand side). Equality is determined by writing the expressions in De Bruijn Index form
      * for α-equivalence
      *
      * @param expectedExpression
      * @param ctx
      */
    def >>(expectedExpression: String)(implicit ctx: Context): Unit = Interpreter(ctx, expectedExpression)
      .fold(ex => {
        fail(s"Parsing of expected expression $expectedExpression failed: $ex")
      }, result => >>(result.expression))
  }

  protected implicit class ListParserAndInterpreter(val expr: List[String]) extends Matchers {
    def parse(implicit ctx: Context): (Context, List[ReplExpression]) = expr.foldLeft((ctx, List.empty[ReplExpression]))((t, str) => t match {
      case (c, elems) => Interpreter(c, str) match {
        case Left(_) => c -> elems
        case Right(r) => r.context -> (elems :+ e2re(r.expression))
      }
    })
  }

}