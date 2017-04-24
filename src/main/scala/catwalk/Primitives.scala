package catwalk

import java.lang.Math.pow
import Evaluator.eval

object Primitives {
  val swap: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: stack) => (env, y :: x :: stack)
    case _ => throw new StackUnderflowException()
  }
  val swapThird: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: z :: stack) => (env, z :: y :: x :: stack)
    case _ => throw new StackUnderflowException()
  }
  val swapFourth: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: z :: w :: stack) => (env, w :: y :: z :: x :: stack)
    case _ => throw new StackUnderflowException()
  }
  val pull: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: stack) => (env, y :: x :: stack)
    case _ => throw new StackUnderflowException()
  }
  val pullThird: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: z :: stack) => (env, z :: x :: y :: stack)
    case _ => throw new StackUnderflowException()
  }
  val pullFourth: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: z :: w :: stack) => (env, w :: x :: y :: z :: stack)
    case _ => throw new StackUnderflowException()
  }
  val drop: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, _ :: stack) => (env, stack)
    case _ => throw new StackUnderflowException()
  }
  val duplicate: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: stack) => (env, x :: x :: stack)
    case _ => throw new StackUnderflowException()
  }
  val add: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(x) :: Num(y) :: stack) => (env, Num(x + y) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val subtract: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(x) :: Num(y) :: stack) => (env, Num(x - y) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val multiply: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(x) :: Num(y) :: stack) => (env, Num(x * y) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val divide: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(x) :: Num(y) :: stack) => (env, Num(x / y) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val power: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(x) :: Num(y) :: stack) => (env, Num(pow(x, y)) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val equal: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: stack) => (env, Bool(x == y) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val greater: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(x) :: Num(y) :: stack) => (env, Bool(x > y) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val pair: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: y :: stack) => (env, Pair(x, y) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val split: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Pair(x, y) :: stack) => (env, x :: y :: stack)
    case (_, _ :: _) => throw new IllegalStateException("[Pair, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val isBoolean: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Bool(_) :: stack) => (env, Bool(true) :: stack)
    case (env, _ :: stack) => (env, Bool(false) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val isNumber: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Num(_) :: stack) => (env, Bool(true) :: stack)
    case (env, _ :: stack) => (env, Bool(false) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val isString: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Str(_) :: stack) => (env, Bool(true) :: stack)
    case (env, _ :: stack) => (env, Bool(false) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val isWord: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Word(_) :: stack) => (env, Bool(true) :: stack)
    case (env, _ :: stack) => (env, Bool(false) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val isPair: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Pair(_, _) :: stack) => (env, Bool(true) :: stack)
    case (env, _ :: stack) => (env, Bool(false) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val isQuote: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Quote(_) :: stack) => (env, Bool(true) :: stack)
    case (env, _ :: stack) => (env, Bool(false) :: stack)
    case _ => throw new StackUnderflowException()
  }
  val curry: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, x :: Quote(q) :: stack) => (env, Quote(x :: q) :: stack)
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Value, Quote, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val quote: (Environment, List[Value]) => (Environment, List[Value]) =
    (env, stack) => (env setMode CollectQuote, Quote(List()) :: stack)
  val call: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, Quote(q) :: stack) => eval(env, q, stack)
    case (_, _ :: _) => throw new IllegalStateException("[Quote, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val `if`: (Environment, List[Value]) => (Environment, List[Value]) = {
    case (env, alternative :: consequent :: Bool(b) :: stack) =>
      (env, (if (b) { consequent } else { alternative }) :: stack)
    case (_, _ :: _ :: _ :: _) => throw new IllegalStateException("[Value, Value, Boolean, ...] required")
    case _ => throw new StackUnderflowException()
  }
  def apply(): Environment = new Environment(Evaluate, Map[String, Verb](
    ("swap",        Native(swap)),
    ("swap third",  Native(swapThird)),
    ("swap fourth", Native(swapFourth)),
    ("pull",        Native(pull)),
    ("pull third",  Native(pullThird)),
    ("pull fourth", Native(pullFourth)),
    ("drop",        Native(drop)),
    ("duplicate",   Native(duplicate)),
    ("add",         Native(add)),
    ("subtract",    Native(subtract)),
    ("multiply",    Native(multiply)),
    ("divide",      Native(divide)),
    ("power",       Native(power)),
    ("equal",       Native(equal)),
    ("greater",     Native(greater)),
    ("pair",        Native(pair)),
    ("split",       Native(split)),
    ("is boolean",  Native(isBoolean)),
    ("is number",   Native(isNumber)),
    ("is string",   Native(isString)),
    ("is word",     Native(isWord)),
    ("is pair",     Native(isPair)),
    ("is quote",    Native(isQuote)),
    ("curry",       Native(curry)),
    ("quote",       Native(quote)),
    ("call",        Native(call)),
    ("if",          Native(`if`))
  ))
}
