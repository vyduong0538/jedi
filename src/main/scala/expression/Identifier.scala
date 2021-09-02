package expression
import context._
import value._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment): Value = {
    if (env.contains(this)) {
      val value = env(this)
      var arg: Value = value
      if (value.isInstanceOf[Thunk]) {
        arg = value.asInstanceOf[Thunk].apply()
      }
      arg
    }
    else throw new UndefinedException(this)
  }
}
