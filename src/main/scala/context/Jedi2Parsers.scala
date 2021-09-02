package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {


  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(identifier ~ more) => identifier :: more
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~> params ~ expression ^^ {
    case more ~ exp => Lambda(more, exp)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case exp ~ more => Block(exp :: more)
  }

//  def expressions: Parser[List[Expression]] = "{" > opt(expression ~ rep("," ~> expression)) <"}" ^^{
//    case None => Nil
//    case Some(expression ~ more) => expression :: more
//  }

  // freeze parser
  // freeze ::= "freeze" ~ "(" ~ expression ~ ")" // makes a thunk
  def freeze: Parser[MakeThunk] = "freeze" ~ "(" ~ expression ~ ")" ^^ {
    case "freeze" ~ "(" ~ expression ~ ")" => MakeThunk(expression)
  }

//    def delay: Parser[Expression] = "delay" ~ "(" ~ expression ~ ")" ^^ {
//      case "delay"~"("~exp~")" => MakeText(exp)
//    }
//
//    // override of term parser
//    override def term: Parser[Expression]  = lambda | freeze | delay | funCall | block | literal | "("~>expression<~")"
//  }

/*
  switch ::= "switch" ~ "(" ~ expression ~ ")" ~ expressions
  expressions ::= "{" ~ expression ~ rep(";" ~ expression) ~ "}"
   */

  def expressions: Parser[List[Expression]] = "{" ~> opt(expression ~ rep(";" ~> expression)) <~"}" ^^{
    case None => Nil
    case Some(expression ~ more) => expression :: more
  }

    def switch: Parser[Switch] = "switch" ~ "(" ~ expression ~ ")" ~ expressions ^^ {
      case "switch" ~ "(" ~ expression ~ ")" ~ expressions => Switch(expression, expressions)
    }

//  override of term parser

  override def term: Parser[Expression] = switch | lambda | freeze | funCall | block | literal | "(" ~> expression <~ ")"

}


