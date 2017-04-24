package catwalk

sealed trait Value

final case class Bool(x: Boolean) extends Value

final case class Num(x: Double) extends Value

final case class Str(x: String) extends Value

final case class Word(x: String) extends Value

final case class Pair(x: Value, y: Value) extends Value

final case class Quote(xs: List[Value]) extends Value

sealed trait Verb

final case class Native(body: (Environment, List[Value]) => (Environment, List[Value])) extends Verb

final case class Definition(body: List[Value]) extends Verb

sealed trait Mode

/**
  * Interpret input by applying verbs and pushing literals on top of the stack.
  */
case object Evaluate extends Mode

/**
  * Interpret input by cons'ing words onto quote on top of the stack.
  */
case object CollectQuote extends Mode

final class Environment(val mode: Mode, val verbs: Map[String, Verb]) {
  def define(name: String, verb: Verb) = new Environment(mode, verbs + (name -> verb))
  def setMode(mode: Mode) = new Environment(mode, verbs)
}

class StackUnderflowException extends Exception
