package value

import context.TypeException
import expression.Literal

case class Boole(value: Boolean) extends Literal with Value {

  def &&(other:Value):Boole=
    other match {
      case a: Boole => if(!this.value) Boole(false) else Boole(a.value)
      case _ => throw new TypeException("Boolean operand required")
    }

  def ||(other: Value): Boole =
    other match {
      case a: Boole => if(this.value) Boole(true) else Boole(a.value)
      case _ => throw new TypeException("Boolean operand required")
    }

  def unary_!(): Boole = Boole(!this.value)
  override def toString = this.value.toString
  override def hashCode:Int =this.toString.hashCode
}

object Boole {
  val FALSE = Boole(false);
  val TRUE = Boole(true)
}