package nl.soqua.lcpi.interpreter

trait Context {

}

object Context {
  def apply(): Context = new ContextImpl()
}

private class ContextImpl() extends Context {

}
