package expression
import value._
import context._

case class Conjunction(args: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result = true
    for (arg <- args if result) {
      arg.execute(env)
      match {
        case Boole(a) => {
          if (!a) result = false
        }
        case _ => throw new TypeException("Argument for && should be Boole!")
      }
    }
    Boole(result)
  }
}



