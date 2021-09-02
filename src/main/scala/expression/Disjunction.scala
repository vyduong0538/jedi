package expression

import value._
import context._

case class Disjunction(args: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result = false
    for (arg <- args if result != true) {
      arg.execute(env) match {
        case Boole(a) => {
          if (a) result = true
        }
        case _ => throw new TypeException("Argument for || should be Boole!")
      }
    }
    Boole(result)
  }
}

