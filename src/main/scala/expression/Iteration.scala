package expression

import value._
import context._



case class Iteration(condition: Expression, body:Expression) extends SpecialForm{
  override def execute(env: Environment): Value = {
    if (!condition.execute(env).isInstanceOf[Boole]) throw new TypeException("Condition must be Boole")
    var exit = condition.execute(env).asInstanceOf[Boole].value
    while (exit) {
      body.execute(env)
      exit = condition.execute(env).asInstanceOf[Boole].value
    }
    Notification.DONE
  }
}