package catwalk

import scala.annotation.tailrec

object Evaluator {
  @tailrec
  def eval(input: List[Value], state: State): State = input match {
    case Nil => state
    case Word(name) :: rest => eval(rest, app(state verbs name, state))
    case literal :: rest => eval(rest, state push literal)
  }
  def app(verb: Verb, state: State): State = verb match {
    case Native(f) => f(state)
    case Definition(body) => eval(body, state)
  }
}
