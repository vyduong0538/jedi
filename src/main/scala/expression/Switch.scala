package expression


import context._
import value._

case class Switch(exp: Expression, expressions: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var res: Value = Integer(0)
    val expressionIndex = exp.toString.toInt
    if(!exp.toString.toInt.isValidInt) throw new TypeException("Input Arguments must be Integer")
    if(expressionIndex > expressions.length) throw new TypeException("UNDEFINED")
    res = Integer(expressions(expressionIndex).execute(env).toString.toInt)
    res
  }
}