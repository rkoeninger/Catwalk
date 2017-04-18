package catwalk

import java.lang.Math.pow
import Evaluator.eval

object Primitives {
  val swap: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: stack) => y :: x :: stack
    case _ => throw new StackUnderflowException()
  }
  val swapThird: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: z :: stack) => z :: y :: x :: stack
    case _ => throw new StackUnderflowException()
  }
  val swapFourth: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: z :: w :: stack) => w :: y :: z :: x :: stack
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
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val subtract: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x - y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val multiply: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x * y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val divide: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(x / y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val power: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Num(pow(x, y)) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val equal: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: stack) => Bool(x == y) :: stack
    case _ => throw new StackUnderflowException()
  }
  val greater: (Environment, List[Value]) => List[Value] = {
    case (_, Num(x) :: Num(y) :: stack) => Bool(x > y) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val pair: (Environment, List[Value]) => List[Value] = {
    case (_, x :: y :: stack) => Pair(x, y) :: stack
    case _ => throw new StackUnderflowException()
  }
  val split: (Environment, List[Value]) => List[Value] = {
    case (_, Pair(x, y) :: stack) => x :: y :: stack
    case (_, _ :: _) => throw new IllegalStateException("[Pair, ...] required")
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
  val curry: (Environment, List[Value]) => List[Value] = {
    case (_, x :: Quote(quote) :: stack) => Quote(x :: quote) :: stack
    case (_, _ :: _ :: _) => throw new IllegalStateException("[Value, Quote, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val uncurry: (Environment, List[Value]) => List[Value] = {
    case (_, Quote(x :: quote) :: stack) => x :: Quote(quote) :: stack
    case (_, _ :: _) => throw new IllegalStateException("[Quote[Value, ...], ...] required")
    case _ => throw new StackUnderflowException()
  }
  val call: (Environment, List[Value]) => List[Value] = {
    case (env, Quote(quote) :: stack) => eval(env, quote, stack)
    case (_, _ :: _) => throw new IllegalStateException("[Quote, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val `if`: (Environment, List[Value]) => List[Value] = {
    case (_, alternative :: consequent :: Bool(b) :: stack) => (if (b) { consequent } else { alternative }) :: stack
    case (_, _ :: _ :: _ :: _) => throw new IllegalStateException("[Value, Value, Boolean, ...] required")
    case _ => throw new StackUnderflowException()
  }
  def apply(): Environment = new Environment(Map[String, Verb](
    ("swap",        Native(swap)),
    ("swap third",  Native(swapThird)),
    ("swap fourth", Native(swapFourth)),
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
    ("is empty",    Native(isEmpty)),
    ("is boolean",  Native(isBoolean)),
    ("is number",   Native(isNumber)),
    ("is string",   Native(isString)),
    ("is word",     Native(isWord)),
    ("is pair",     Native(isPair)),
    ("is quote",    Native(isQuote)),
    ("curry",       Native(curry)),
    ("uncurry",     Native(uncurry)),
    ("call",        Native(call)),
    ("if",          Native(`if`))
  ))
}
