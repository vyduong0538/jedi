//package value
//
//case class Pair(var varLeft: Value, var varRight: Value) {
//  override def toString: String ="("+ varLeft.toString +", " + varRight.toString + ")"
//
//}


package value

case class Pair(val first: Value, val second: Value) extends Value with Ordered[Value] {
  override def toString: String ="("+ first.toString +", " + second.toString + ")"
  override def compare(that: Value) = this.compareTo(that)
}
