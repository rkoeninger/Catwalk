package catwalk

object Primitives {
  val swap: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: stack) => y :: x :: stack
    case _ => throw new StackUnderflowException()
  }
  val drop: (Environment, List[Value]) => List[Value] = {
    case (_, _ :: stack) => stack
    case _ => throw new StackUnderflowException()
  }
  val call: (Environment, List[Value]) => List[Value] = {
    case (env, Quote(quote) :: stack) => Evaluator.eval(env, quote, stack)
    case _ => throw new StackUnderflowException()
  }
}
