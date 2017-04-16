package catwalk

object Evaluator {
  def eval(env: Environment, input: List[Value], stack: List[Value]): List[Value] = input match {
    case Nil => stack
    case Sym(symbol) :: rest => eval(env, rest, app(env, env.verbs(symbol), stack))
    case literal :: rest => eval(env, rest, literal :: stack)
  }
  def app(env: Environment, verb: Verb, stack: List[Value]): List[Value] = verb match {
    case Native(_, f) => f(env, stack)
    case Definition(_, body) => eval(env, body, stack)
  }
}
