package catwalk

object Primitives {
  val swap: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: rest) => y :: x :: rest
    case _ => throw new StackUnderflowException()
  }
  val drop: (Environment, List[Value]) => List[Value] = {
    case (_, _ :: rest) => rest
    case _ => throw new StackUnderflowException()
  }
  val call: (Environment, List[Value]) => List[Value] = {
    case (env, Quote(quote) :: rest) => Evaluator.app(env, Definition("anon", quote), rest)
    case _ => throw new StackUnderflowException()
  }
}
