package value

case class Variable(var content: Value) extends Value with Ordered[Value] {
  override def toString = "[" + content.toString + "]"
  override def compare(that: Value) = this.compareTo(that)
}