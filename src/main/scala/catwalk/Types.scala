package catwalk

sealed trait Value

final case class Bool(x: Boolean) extends Value

final case class Num(x: Double) extends Value

final case class Str(x: String) extends Value

final case class Word(x: String) extends Value

final case class Pair(x: Value, y: Value) extends Value

final case class Quote(xs: List[Value]) extends Value

sealed trait Verb

final case class Native(body: State => State) extends Verb

final case class Definition(body: List[Value]) extends Verb

object Pure {
  def apply(f: List[Value] => List[Value]): Verb = Native({
    case State(verbs, stack) => State(verbs, f(stack))
  })
}

final case class State(verbs: Map[String, Verb], stack: List[Value]) {
  def define(name: String, verb: Verb) = State(verbs + (name -> verb), stack)
  def push(value: Value) = State(verbs, value :: stack)
}

class UnexpectedStackException(top: List[String]) extends Exception(s"[${top.mkString(", ")}] expected")

class StackUnderflowException extends Exception
