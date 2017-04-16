package catwalk

sealed trait Value

case object Empty extends Value

final case class Bool(x: Boolean) extends Value

final case class Num(x: Double) extends Value

final case class Str(x: String) extends Value

final case class Word(x: String) extends Value

final case class Pair(x: Value, y: Value) extends Value

final case class Quote(xs: List[Value]) extends Value

sealed trait Verb

final case class Native(name: String, body: (Environment, List[Value]) => List[Value]) extends Verb

final case class Definition(name: String, body: List[Value]) extends Verb

final class Environment(val verbs: Map[String, Verb])

class StackUnderflowException extends Exception
