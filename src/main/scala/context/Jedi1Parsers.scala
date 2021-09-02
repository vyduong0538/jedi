package context

import scala.util.parsing.combinator._
import expression._
import value._


class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case eq ~ Nil  => eq
    case eq ~ more => Conjunction(eq :: more)
  }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case ineq ~ Nil  => ineq
    case ineq ~ more => FunCall(Identifier("equals"), ineq :: more)
  }

  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case sum ~ None => sum
    case s1 ~ Some("<" ~ s2)  => FunCall(Identifier("less"), List(s1, s2))
    case s1 ~ Some(">" ~ s2)  => FunCall(Identifier("more"), List(s1, s2))
    case s1 ~ Some("!=" ~ s2) => FunCall(Identifier("unequals"), List(s1, s2))
  }

  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
    case p ~ more => parseSums(p, more)
  }

  private def parseSums(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseSums(combiner(result, unseen.head), unseen.tail)
  }

  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case (t1 ~ t2) => parseProduct(t1, t2)
  }

  private def parseProduct(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "*" ~ p => FunCall(Identifier("mul"), List(exp, p))
        case "/" ~ p => FunCall(Identifier("div"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseProduct(combiner(result, unseen.head), unseen.tail)
  }

  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

  def literal = boole | inexact | exact | chars | identifier


  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
    case characters => Chars(characters.substring(1, characters.length - 1))
  }

  def exact: Parser[Exact] = """0|(\+|-)?[1-9][0-9]*""".r ^^ {
    case integers=> Exact(integers.toInt)
  }

  def inexact: Parser[Inexact] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ {
    case doubles=> Inexact(doubles.toDouble)
  }

  def boole: Parser[Boole] = """true|false""".r ^^ {
    case boolean => Boole(boolean.toBoolean)
  }

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case identifier => Identifier(identifier)
  }

  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case identifier ~ operator => FunCall(identifier, operator)
  }

  def operands: Parser[List[Expression]] = "(" ~> opt(expression~rep("," ~> expression)) <~")" ^^ {
    case None => Nil
    case Some(expression ~ Nil) => List(expression)
    case Some(expression ~ exp) => expression::exp
    case _  => List()
  }
}
