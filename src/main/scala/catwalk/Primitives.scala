package catwalk

import java.lang.Math.pow
import Evaluator.eval

object Primitives {
  val swap: Verb = Pure({
    case x :: y :: stack => y :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val swapThird: Verb = Pure({
    case x :: y :: z :: stack => z :: y :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val swapFourth: Verb = Pure({
    case x :: y :: z :: w :: stack => w :: y :: z :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val pull: Verb = Pure({
    case x :: y :: stack => y :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val pullThird: Verb = Pure({
    case x :: y :: z :: stack => z :: x :: y :: stack
    case _ => throw new StackUnderflowException()
  })
  val pullFourth: Verb = Pure({
    case x :: y :: z :: w :: stack => w :: x :: y :: z :: stack
    case _ => throw new StackUnderflowException()
  })
  val drop: Verb = Pure({
    case _ :: stack => stack
    case _ => throw new StackUnderflowException()
  })
  val duplicate: Verb = Pure({
    case x :: stack => x :: x :: stack
    case _ => throw new StackUnderflowException()
  })
  val add: Verb = Pure({
    case Num(x) :: Num(y) :: stack => Num(x + y) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Number", "Number"))
    case _ => throw new StackUnderflowException()
  })
  val subtract: Verb = Pure({
    case Num(x) :: Num(y) :: stack => Num(x - y) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Number", "Number"))
    case _ => throw new StackUnderflowException()
  })
  val multiply: Verb = Pure({
    case Num(x) :: Num(y) :: stack => Num(x * y) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Number", "Number"))
    case _ => throw new StackUnderflowException()
  })
  val divide: Verb = Pure({
    case Num(x) :: Num(y) :: stack => Num(x / y) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Number", "Number"))
    case _ => throw new StackUnderflowException()
  })
  val power: Verb = Pure({
    case Num(x) :: Num(y) :: stack => Num(pow(x, y)) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Number", "Number"))
    case _ => throw new StackUnderflowException()
  })
  val greater: Verb = Pure({
    case Num(x) :: Num(y) :: stack => Bool(x > y) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Number", "Number"))
    case _ => throw new StackUnderflowException()
  })
  val equal: Verb = Pure({
    case x :: y :: stack => Bool(x == y) :: stack
    case _ => throw new StackUnderflowException()
  })
  val pair: Verb = Pure({
    case x :: y :: stack => Pair(x, y) :: stack
    case _ => throw new StackUnderflowException()
  })
  val split: Verb = Pure({
    case Pair(x, y) :: stack => x :: y :: stack
    case _ :: _ => throw new UnexpectedStackException(List("Pair"))
    case _ => throw new StackUnderflowException()
  })
  val isBoolean: Verb = Pure({
    case Bool(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isNumber: Verb = Pure({
    case Num(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isString: Verb = Pure({
    case Str(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isWord: Verb = Pure({
    case Word(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isPair: Verb = Pure({
    case Pair(_, _) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val isQuote: Verb = Pure({
    case Quote(_) :: stack => Bool(true) :: stack
    case _ :: stack => Bool(false) :: stack
    case _ => throw new StackUnderflowException()
  })
  val curry: Verb = Pure({
    case x :: Quote(q) :: stack => Quote(x :: q) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Value", "Quote"))
    case _ => throw new StackUnderflowException()
  })
  val quote: Verb = Pure(stack => Quote(List()) :: stack)
  val `if`: Verb = Pure({
    case y :: x :: Bool(b) :: stack => (if (b) x else y) :: stack
    case _ :: _ :: _ => throw new UnexpectedStackException(List("Value", "Value", "Boolean"))
    case _ => throw new StackUnderflowException()
  })
  val call: Verb = Native({
    case State(verbs, Quote(q) :: stack) => eval(q, State(verbs, stack))
    case State(_, _ :: _) => throw new UnexpectedStackException(List("Quote"))
    case _ => throw new StackUnderflowException()
  })
  val `try`: Verb = Native({
    case State(verbs, Quote(handler) :: Quote(body) :: stack) =>
      try
        eval(body, State(verbs, stack))
      catch {
        case e: Exception => eval(handler, State(verbs, Str(e.getMessage) :: stack))
      }
    case State(_, _ :: _ :: _) => throw new UnexpectedStackException(List("Quote", "Quote"))
    case _ => throw new StackUnderflowException()
  })
  val `throw`: Verb = Native({
    case State(_, Str(message) :: _) => throw new Exception(message)
    case State(_, _:: _) => throw new UnexpectedStackException(List("String"))
    case _ => throw new StackUnderflowException()
  })
  def apply(): Map[String, Verb] = Map[String, Verb](
    ("swap",        swap),
    ("swap third",  swapThird),
    ("swap fourth", swapFourth),
    ("pull",        pull),
    ("pull third",  pullThird),
    ("pull fourth", pullFourth),
    ("drop",        drop),
    ("duplicate",   duplicate),
    ("add",         add),
    ("subtract",    subtract),
    ("multiply",    multiply),
    ("divide",      divide),
    ("power",       power),
    ("greater",     greater),
    ("equal",       equal),
    ("pair",        pair),
    ("split",       split),
    ("is boolean",  isBoolean),
    ("is number",   isNumber),
    ("is string",   isString),
    ("is word",     isWord),
    ("is pair",     isPair),
    ("is quote",    isQuote),
    ("curry",       curry),
    ("quote",       quote),
    ("if",          `if`),
    ("call",        call),
    ("try",         `try`),
    ("throw",       `throw`)
  )
}
