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
  val duplicate: (Environment, List[Value]) => List[Value] = {
    case (_, x :: stack) => x :: x :: stack
    case _ => throw new StackUnderflowException()
  }
  val add: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x + y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Num, Num] required")
    case _ => throw new StackUnderflowException()
  }
  val subtract: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x - y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Num, Num] required")
    case _ => throw new StackUnderflowException()
  }
  val multiply: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x * y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Num, Num] required")
    case _ => throw new StackUnderflowException()
  }
  val divide: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x / y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Num, Num] required")
    case _ => throw new StackUnderflowException()
  }
  val power: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(Math.pow(x, y)) :: stack
    case _ => throw new StackUnderflowException()
  }
  val equal: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: stack) => Bool(x == y) :: stack
    case _ => throw new StackUnderflowException()
  }
  val greater: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Bool(x > y) :: stack
    case _ => throw new StackUnderflowException()
  }
  val pair: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: stack) => Pair(x, y) :: stack
    case _ => throw new StackUnderflowException()
  }
  val left: (Environment, List[Value]) => List[Value] = {
    case (_, Pair(x, _) :: stack) => x :: stack
    case _ => throw new StackUnderflowException()
  }
  val right: (Environment, List[Value]) => List[Value] = {
    case (_, Pair(_, y) :: stack) => y :: stack
    case _ => throw new StackUnderflowException()
  }
  val isEmpty: (Environment, List[Value]) => List[Value] = {
    case (_, Empty :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val isBoolean: (Environment, List[Value]) => List[Value] = {
    case (_, Bool(_) :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val isNumber: (Environment, List[Value]) => List[Value] = {
    case (_, Num(_) :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val isString: (Environment, List[Value]) => List[Value] = {
    case (_, Str(_) :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val isWord: (Environment, List[Value]) => List[Value] = {
    case (_, Word(_) :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val isPair: (Environment, List[Value]) => List[Value] = {
    case (_, Pair(_, _) :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val isQuote: (Environment, List[Value]) => List[Value] = {
    case (_, Quote(_) :: stack) => Bool(true) :: stack
    case (_, _ :: stack) => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  }
  val call: (Environment, List[Value]) => List[Value] = {
    case (env, Quote(quote) :: stack) => Evaluator.eval(env, quote, stack)
    case (_, _ :: _) => throw new IllegalStateException("[Quote] required")
    case _ => throw new StackUnderflowException()
  }
  val `if`: (Environment, List[Value]) => List[Value] = {
    case (_, alternative :: consequent :: Bool(b) :: stack) => (if (b) { consequent } else { alternative }) :: stack
    case (_, _ :: _ :: _ :: _) => throw new IllegalStateException("[_, _, Bool] required")
    case _ => throw new StackUnderflowException()
  }
}
