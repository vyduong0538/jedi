package value

import context.TypeException
import context.IllegalValueException
import expression.Literal

case class Inexact(val value: Double) extends Numeric with Ordered[Value] with Literal {

  def +(other: Value): Addable =
    other match {
      case x: Exact => Inexact(this.value + x.value)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }
  def -(other: Value): Numeric =
    other match {
      case x: Exact => Inexact(this.value - x.value)
      case x: Inexact => Inexact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def *(other: Value): Numeric =
    other match {
      case x: Exact => Inexact(this.value * x.value)
      case x: Inexact => Inexact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def /(other: Value): Numeric =
    other match {
      case x: Exact => if(x.value!=0.0) Inexact(this.value / x.value) else throw new IllegalValueException("Divide by 0!")
      case x: Inexact => if(x.value!=0.0) Inexact(this.value / x.value) else throw new IllegalValueException("Divide by 0!")
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_-(): Numeric = Inexact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value)
      case x: Inexact => this.value.toDouble.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }


  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value.toDouble
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case _ => false
    }


  override def toString: String = this.value.toString


}
