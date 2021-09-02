package value

import context._
import expression._

//class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv) {
//  private var cache: Value = null
//
//  def apply(): Value = {
//    if (cache == null)
//      cache = super.apply(Nil)
//    cache
//  }
//}

class Thunk(val params: List[Identifier], val bod: Expression, val env: Environment) extends Closure(params, bod, env) {
  var cache: Value = null

  def apply(): Value = {
    if (cache == null) {
      cache = super.apply(Nil, env)
    }
    cache
  }
}