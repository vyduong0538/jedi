package expression

//import value.Notification
import value.Value
import context.Environment

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    env.put(identifier, expression.execute(env))
    value.Notification.OK
  }
  override def toString = "The declaration identifier: " + identifier + ", expression: " + expression
}