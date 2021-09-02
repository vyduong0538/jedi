package expression

import context._
import value._

//case class MakeThunk(val expression: Expression) extends SpecialForm {
//  def execute(env: Environment) = new Thunk(expression, env)
//}

case class MakeThunk(val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    val res = new Thunk(Nil, body, env)
    res
  }
}