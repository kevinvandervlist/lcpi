package nl.soqua.lcpi.interpreter.transformation

import nl.soqua.lcpi.ast.lambda.Expression.{A, V, λ}
import org.scalatest.{Matchers, WordSpec}

class DeBruijnIndexSpec extends WordSpec with Matchers {

  import DeBruijnIndex.deBruijn
  val x = V("x")

  "Transforming to De Bruijn indexes" should {
    "work on the identity function" in {
      deBruijn(λ(x, x)) >> λ(1)
    }
  }
}
