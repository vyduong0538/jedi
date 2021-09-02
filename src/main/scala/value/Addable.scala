package value

import expression.Literal

trait Addable extends Literal with Value {
  def +(other: Value): Addable

}
