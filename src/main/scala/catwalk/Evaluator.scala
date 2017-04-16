package catwalk

import scala.annotation.tailrec

object Evaluator {
  @tailrec
  def eval(env: Environment, input: List[Value], stack: List[Value]): List[Value] = input match {
    case Nil => stack
    case Word(name) :: rest => eval(env, rest, app(env, env verbs name, stack))
    case literal :: rest => eval(env, rest, literal :: stack)
  }
  def app(env: Environment, verb: Verb, stack: List[Value]): List[Value] = verb match {
    case Native(_, f) => f(env, stack)
    case Definition(_, body) => eval(env, body, stack)
  }
}
