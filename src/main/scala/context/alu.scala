package context

import value._
import expression._

import collection.mutable._
import scala.annotation.tailrec
//ALU
object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary
    case "write" => write(args)
    case "store" => store(args)
    case "put" => put(args)
    case "rem" => rem(args)
    case "contains" => contains(args)
    case "map" => map(args)
    case "filter" => filter(args)
    case "get" => get(args)
    case "addLast" => addLast(args)
    case "size" => size(args)
    case "prompt" => prompt(args)
    case "read" => read(args)
    case "dereference" => dereference(args)
    case "var" => makeVar(args)
    case "nil" => nil(args)
    case "Nil" => getEmpty
    case "cons" => cons(args)
    case "car" => car(args)
    case "cdr" => cdr(args)
    case "list" => list(args)


    case _ => throw new UndefinedException(opcode)
  }

  private def add(args: List[Value]): Value = {

    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def mul(args: List[Value]): Value = {

    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if(unseen == Nil) result
      else helper(result * unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to * must be Numeric")
    }
  }

  private def sub(args: List[Value]): Value = {

    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if(unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to - must be numeric")
    }
  }

  private def div(args: List[Value]): Value = {

    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if(unseen == Nil) result
      else helper(result / unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to / must be numeric")
    }
  }

  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  private def more(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by >")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to > must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] > args(1))
  }

  private def same(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by ==")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to == must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] == args(1))
  }


  private def unequals(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by !=")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to != must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] != args(1))
  }

  def not(args: List[Value]): Value = {
    if (args.length != 1) throw new TypeException("1 input required by !")
    if (!args.head.isInstanceOf[Boole]) throw new TypeException("input to not must be Boole")
    !args.head.asInstanceOf[Boole]
  }

  //IO
  def write(vals: List[Value]): Value = {
    println(vals(0)); Notification.DONE
  }

  def read(vals: List[Value]): Value = {
    val result = io.StdIn.readDouble();
    Inexact(result)
  }

  def prompt(vals: List[Value]): Value = {
    print("=> ");
    Notification.DONE
  }

  //variable
  def dereference(args: List[Value]): Value = {
    val test = args(0)
    if (args.length == 1) {
      if (test.isInstanceOf[Variable]) test.asInstanceOf[Variable].content
      else throw new TypeException("this is not a deference-able variable")
    }
    else throw new JediException("args must only contain one value")
  }

  def makeVar(args: List[Value]): Value = {
    if (args.length == 1)
      Variable(args(0))
    else throw new JediException("args must only contain one value")
  }


  def car(args: List[Value]): Value = {
    if (args.length == 1 && args(0).isInstanceOf[Pair])
      (args(0).asInstanceOf[Pair].first)
    else throw new JediException("args must only contain one value")
  }

 def cdr(args: List[Value]): Value = {
    if (args.length == 1 && args(0).isInstanceOf[Pair])
      (args(0).asInstanceOf[Pair].second)
    else throw new JediException("args must only contain one value")
  }


  def cons(args: List[Value]): Pair = {
    if (args.length != 2) throw new TypeException("2 input required by cons")
    Pair(args.head, args.last)
  }

    def list(args: List[Value]): Value = {
      if(args.size < 2) Pair(args.head, empty);
      else Pair(args.head, list(args.tail))
    }

    def nil(args: List[Value]): Value = {
      if (args.length != 0) throw new TypeException("Nil() should not contains any input")
      else
      empty;
    }

    def getEmpty = {
      empty
    }

  // store ops

  // returns a new store containing args
  private def store(args: List[Value]) = new Store(ArrayBuffer(args: _*))

  // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
  private def put(args: List[Value]) = {
    if (args.size != 3)
      throw new TypeException("Wrong argument! Expected argument form: put(v: Value, p: Integer, s: Store)")
    if(!args(1).isInstanceOf[Exact] || !args(2).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form:(v: Value, p: Integer, s: Store)")
    args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Exact])
    Notification.DONE
  }

  // rem(p: Integer, s: Store) calls s.rem(p)
  private def rem(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Exact] || !args(1).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form: (p: Integer, s: Store)")
    args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Exact])
    Notification.DONE
  }

  // get(p: Exact, s: Store) calls s.get(p)
  private def get(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Exact] || !args(1).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form: get(p: Integer, s: Store)")
    args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Exact])
  }

  // map(f: Closure, s: Store) calls s.map(f)
  private def map(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form: map(f: Closure, s: Store)")
    args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
  }

  // filter(f: Closure, s: Store) calls s.filter(f)
  private def filter(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form: filter(f: Closure, s: Store)")
    args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
  }

  // contains(v: Value, s: Store) calls s.contains(v)
  private def contains(args: List[Value]) = {
    if (args.size != 2 || !args(1).isInstanceOf[Store])
      throw new TypeException("eWrong argument! Expected argument form: contains(v: Value, s: Store)")
    args(1).asInstanceOf[Store].contains(args(0))
  }

  // addLast(v: Value, s: Store) calls s.add(v)
  private def addLast(args: List[Value]) = {
    if (args.size != 2 || !args(1).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form: addLast(v: Value, s: Store)")
    args(1).asInstanceOf[Store].add(args(0))
    Notification.DONE
  }

  // size(s: Store) calls s.size
  private def size(args: List[Value]) = {
    if (args.size != 1 || !args(0).isInstanceOf[Store])
      throw new TypeException("Wrong argument! Expected argument form: size(s: Store)")
    else
      args(0).asInstanceOf[Store].size
  }

}


