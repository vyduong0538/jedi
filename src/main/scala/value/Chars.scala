package value

import context.TypeException
import expression.Literal

case class Chars(val value: String) extends Addable with Ordered[Value] with Literal {

  def +(other: Value): Addable =
    other match {
      case x: Exact => Chars(this.value + x.value)
      case x: Inexact => Chars(this.value + x.value)
      case x: Chars=> Chars(this.value + x.value)
      case x: Boole=> Chars(this.value + x.value )
      case _ => throw new TypeException("Unacceptable operand")
    }

  override def compare(other: Value): Int =
    other match {
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  def size(): Exact ={
    Exact(value.length)
  }

  def subChars(to: Exact, from: Exact): Chars={
    Chars(value.slice(to.value,from.value))
  }

  override def toString = this.value.toString

}

