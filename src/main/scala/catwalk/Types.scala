package catwalk

sealed trait Value

case object Empty extends Value

case class Cons(x: Value, y: Value) extends Value

case class Num(x: Int) extends Value

case class Str(x: String) extends Value

case class Sym(x: String) extends Value

case class Quote(xs: List[Value]) extends Value

sealed trait Verb

case class Native(name: String, body: (Environment, List[Value]) => List[Value]) extends Verb

case class Definition(name: String, body: List[Value]) extends Verb

class Environment(val verbs: Map[String, Verb])

class StackUnderflowException extends Exception
