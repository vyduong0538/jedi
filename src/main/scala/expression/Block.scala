package expression

import context._
import value._

case class Block(val expressions: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    var lastValue: Value = null
    for (expression <- expressions) lastValue = expression.execute(tempEnv)
    lastValue
  }
}
