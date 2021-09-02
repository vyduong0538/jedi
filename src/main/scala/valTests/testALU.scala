package valTests
import context._
import value._
import expression._

object testALU extends App {
  try {
    //Tests
    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Exact(7))))     // 18
    println(alu.execute(Identifier("add"), List(Chars("abc"), Exact(6), Exact(7)))) // abc67
    println(alu.execute(Identifier("less"), List(Chars("abc"), Chars("def"))))  // true
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("abc"))))  // true

    //Additional test cases
    println()
    println(Chars("Additional test cases"))
    println(alu.execute(Identifier("sub"), List(Exact(5), Exact(6), Exact(7)))) //-8
    println(alu.execute(Identifier("mul"), List(Exact(5), Exact(6), Exact(7)))) //210
    println(alu.execute(Identifier("div"), List(Exact(42), Exact(6), Exact(7)))) //1
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("jkl")))) //false
    println(alu.execute(Identifier("equals"), List(Chars("abcd"), Chars("abc")))) //false
    println(alu.execute(Identifier("not"), List(Boole(true)))) //false


  } catch {
    case e: Exception => println(e)
  }


}