package value

import context._
import expression._



case class Closure(parameters: List[Identifier],  body: Expression, defEnv: Environment) extends Value {
  override def toString="closure"

  def apply(args: List[Value], callEnv: Environment = null) = {
    var tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(parameters, args)
    body.execute(tempEnv)
  }
}

//case class Closure(val parameters: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
////  def apply(args: List[Value]) = {
////    val tempEnv = new Environment(defEnv)
////    tempEnv.bulkPut(parameters, args)
////    body.execute(tempEnv)
////  }
//
//  def apply(args: List[Value], callingEnv: Environment): Value = {
//    var tempEnv = new Environment(defEnv)
//
//    // bind parameters to arguments
//    tempEnv.bulkPut(parameters, args)
//    body.execute(tempEnv)
//  }
//}