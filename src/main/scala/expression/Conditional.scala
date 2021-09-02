package expression

import value._
import context._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val arg = condition.execute(env)
    if (!arg.isInstanceOf[Boole]) throw new Exception("Input argument must be Boole")
    if (arg.asInstanceOf[Boole].value) consequent.execute(env)
    else if (alternative != null) alternative.execute(env) else Notification.OK
  }
  override def toString = {
    var res = "if (" + condition + ") " + consequent
    if (alternative != null) res = res + " otherwise " + alternative
    res
  }
}