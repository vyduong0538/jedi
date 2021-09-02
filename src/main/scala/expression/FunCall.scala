package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression  {
  def execute(env: Environment): Value = {
    var args: List[Value] = Nil

    if (env.contains(operator)) {
      if (flags.paramPassing == flags.BY_NAME) {
        args = operands.map(MakeThunk(_).execute(env))
      } else {
        args = operands.map(_.execute(env))
      }
      // etc.
      val test = operator.execute(env)
      if (test.isInstanceOf[Closure]) {
        val result = test.asInstanceOf[Closure]
        result.apply(args, env)
      }
      else throw new TypeException("operator must be a closure")

    } else {
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
    }
  }
    override def toString = {
      var result = operator.toString + "("
      if (operands != Nil) {
        result = result + operands.head.toString
        for(operand <- operands.tail) {
          result = result + ", " + operand
        }
      }
      result = result + ")"
      result
    }
}