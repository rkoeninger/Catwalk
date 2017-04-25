package catwalk

import java.lang.Math.pow
import Evaluator.eval

object Primitives {
  val swap: State => State = Pure({
    case x :: y :: stack => y :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val swapThird: State => State = Pure({
    case x :: y :: z :: stack => z :: y :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val swapFourth: State => State = Pure({
    case x :: y :: z :: w :: stack => w :: y :: z :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val pull: State => State = Pure({
    case x :: y :: stack => y :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val pullThird: State => State = Pure({
    case x :: y :: z :: stack => z :: x :: y :: stack
    case _ => throw new StackUnderflowException()
  })
  val pullFourth: State => State = Pure({
    case x :: y :: z :: w :: stack => w :: x :: y :: z :: stack
    case _ => throw new StackUnderflowException()
  })
  val drop: State => State = Pure({
    case _ :: stack => stack
    case _ => throw new StackUnderflowException()
  })
  val duplicate: State => State = Pure({
    case x :: stack => x :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val add: State => State = Pure({
    case Num(x) :: Num(y) :: stack => Num(x + y) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val subtract: State => State = Pure({
    case Num(x) :: Num(y) :: stack => Num(x - y) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val multiply: State => State = Pure({
    case Num(x) :: Num(y) :: stack => Num(x * y) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val divide: State => State = Pure({
    case Num(x) :: Num(y) :: stack => Num(x / y) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val power: State => State = Pure({
    case Num(x) :: Num(y) :: stack => Num(pow(x, y)) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val equal: State => State = Pure({
    case x :: y :: stack => Bool(x == y) :: stack
    case _ => throw new StackUnderflowException()
  })
  val greater: State => State = Pure({
    case Num(x) :: Num(y) :: stack => Bool(x > y) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Number, Number, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val pair: State => State = Pure({
    case x :: y :: stack => Pair(x, y) :: stack
    case _ => throw new StackUnderflowException()
  })
  val split: State => State = Pure({
    case Pair(x, y) :: stack => x :: y :: stack
    case _ :: _ => throw new IllegalStateException("[Pair, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val isBoolean: State => State = Pure({
    case Bool(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isNumber: State => State = Pure({
    case Num(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isString: State => State = Pure({
    case Str(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isWord: State => State = Pure({
    case Word(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isPair: State => State = Pure({
    case Pair(_, _) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isQuote: State => State = Pure({
    case Quote(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val curry: State => State = Pure({
    case x :: Quote(q) :: stack => Quote(x :: q) :: stack
    case _ :: _ :: _ => throw new IllegalStateException("[Value, Quote, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val quote: State => State = Pure(stack => Quote(List()) :: stack)
  val call: State => State = {
    case State(verbs, Quote(q) :: stack) => eval(q, State(verbs, stack))
    case State(_, _ :: _) => throw new IllegalStateException("[Quote, ...] required")
    case _ => throw new StackUnderflowException()
  }
  val `if`: State => State = Pure({
    case y :: x :: Bool(b) :: stack => (if (b) x else y) :: stack
    case _ :: _ :: _ :: _ => throw new IllegalStateException("[Value, Value, Boolean, ...] required")
    case _ => throw new StackUnderflowException()
  })
  val `try`: State => State = {
    case State(verbs, Quote(handler) :: Quote(body) :: stack) =>
      try
        eval(body, State(verbs, stack))
      catch {
        case e: Exception => eval(handler, State(verbs, Str(e.getMessage) :: stack))
      }
    case State(_, _ :: _ :: _) => throw new IllegalStateException("[Quote, Quote, ...] required")
    case _ => throw new StackUnderflowException()
  }
  // TODO: include new env in exception?
  val `throw`: State => State = Pure({
    case Str(message) :: _ => throw new Exception(message)
    case _ :: _ => throw new IllegalStateException("[String, ...] required")
    case _ => throw new StackUnderflowException()
  })
  def apply(): Map[String, Verb] = Map[String, Verb](
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
    ("if",          Native(`if`)),
    ("try",         Native(`try`)),
    ("throw",       Native(`throw`))
  )
}
