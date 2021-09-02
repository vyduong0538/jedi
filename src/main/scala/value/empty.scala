package value

object empty extends Value with Ordered[Value] {
  override def toString = "Nil"
  override def compare(that: Value) = this.compareTo(that)
}