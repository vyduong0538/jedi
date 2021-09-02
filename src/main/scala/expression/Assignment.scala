package expression

import context._
import value._

case class Assignment(var vbl: Identifier, var update: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if(env(vbl).isInstanceOf[Variable]) {
      env(vbl).asInstanceOf[Variable].content = update.execute(env)
      Notification.DONE
    }
    else throw new TypeException("vbl requires variables")
  }
}