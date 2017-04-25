package catwalk

import scala.annotation.tailrec

object Evaluator {
  @tailrec
  def eval(env: Environment, input: List[Value], stack: List[Value]): (Environment, List[Value]) = input match {
    case Nil => (env, stack)
    case Word(name) :: rest =>
      val (newEnv, newStack) = app(env, env verbs name, stack)
      eval(newEnv, rest, newStack)
    case literal :: rest => eval(env, rest, literal :: stack)
  }
  def app(env: Environment, verb: Verb, stack: List[Value]): (Environment, List[Value]) = verb match {
    case Native(f) => f(env, stack)
    case Definition(body) => eval(env, body, stack)
  }
}
