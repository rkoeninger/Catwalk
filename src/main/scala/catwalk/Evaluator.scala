package catwalk

import scala.annotation.tailrec

object Evaluator {
  @tailrec
  def eval(env: Environment, input: List[Value], stack: List[Value]): (Environment, List[Value]) = env.mode match {
    case Evaluate => input match {
      case Nil => (env, stack)
      case Word(name) :: rest =>
        val (newEnv, newStack) = app(env, env verbs name, stack)
        eval(newEnv, rest, newStack)
      case literal :: rest => eval(env, rest, literal :: stack)
    }
    case CollectQuote => input match {
      case Nil => (env, stack)
      case Word("end-quote") :: rest => eval(env setMode Evaluate, rest, stack)
      case word :: rest => stack match {
        case Quote(q) :: restStack => eval(env, rest, Quote(word :: q) :: restStack)
        case _ :: _ => throw new IllegalStateException("[Quote, ...] required")
        case _ => throw new StackUnderflowException()
      }
    }
  }

  def app(env: Environment, verb: Verb, stack: List[Value]): (Environment, List[Value]) = verb match {
    case Native(f) => f(env, stack)
    case Definition(body) => eval(env, body, stack)
  }
}
