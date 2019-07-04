package com.github.kmizu.a2r


import scala.util.matching.Regex
import com.github.kmizu.a2r.Ast._
import com.github.kmizu.scomb
import com.github.kmizu.scomb.{Result, SCombinator}

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Parser {
  private object A2RParsers extends SCombinator[Expression] {
    implicit def stringToParser(literal: String): Parser[String] = $(literal)

    implicit def regexToParser(literal: Regex): Parser[String] = regularExpression(literal)

    def %% : Parser[SourceLocation] = % ^^ { l =>
      SourceLocation(l.line, l.column)
    }

    def commit[T](parser: Parser[T]): Parser[T] = parser.commit

    lazy val LINEFEED: Parser[String] = ("\r\n" | "\r" | "\n")

    lazy val SEMICOLON: Parser[String] = ";"

    lazy val ANY: Parser[String] = any ^^ {
      _.toString
    }

    lazy val SPACING: Parser[String] = rule {
      (COMMENT | "\r\n" | "\r" | "\n" | " " | "\t" | "\b" | "\f").* ^^ {
        _.mkString
      }
    }

    lazy val SPACING_WITHOUT_LF: Parser[String] = rule {
      (COMMENT | "\t" | " " | "\b" | "\f").* ^^ {
        _.mkString
      }
    }

    lazy val TERMINATOR: Parser[String] = rule {
      (LINEFEED | SEMICOLON | EOF) << SPACING
    }

    lazy val SEPARATOR: Parser[String] = rule {
      (LINEFEED | COMMA | EOF | SPACING_WITHOUT_LF) << SPACING
    }

    lazy val LINE_COMMENT: Parser[Any] = rule {
      "//" ~ (not(LINEFEED) ~ ANY).* ~ LINEFEED
    }

    lazy val COMMENT: Parser[Any] = rule {
      LINE_COMMENT
    }

    def CL[T](parser: Parser[T]): Parser[T] = parser << SPACING

    override def token(parser: String): Parser[String] = parser << SPACING_WITHOUT_LF

    def unescape(input: String): String = {
      val builder = new java.lang.StringBuilder
      val length = input.length
      var i = 0
      while (i < length - 1) {
        (input.charAt(i), input.charAt(i + 1)) match {
          case ('\\', 'r') => builder.append('\r'); i += 2
          case ('\\', 'n') => builder.append('\n'); i += 2
          case ('\\', 'b') => builder.append('\b'); i += 2
          case ('\\', 'f') => builder.append('\f'); i += 2
          case ('\\', 't') => builder.append('\t'); i += 2
          case ('\\', '\\') => builder.append('\\'); i += 2
          case (ch, _) => builder.append(ch); i += 1
        }
      }
      if (i == length - 1) {
        builder.append(input.charAt(i))
      }
      new String(builder)
    }

    lazy val PLUS: Parser[String] = token("+")
    lazy val STAR: Parser[String] = token("*")
    lazy val SPECIAL_LPAREN: Parser[String] = token("#(")
    lazy val LPAREN: Parser[String] = token("(")
    lazy val RPAREN: Parser[String] = token(")")
    lazy val LBRACE: Parser[String] = token("{")
    lazy val RBRACE: Parser[String] = token("}")
    lazy val LBRACKET: Parser[String] = token("[")
    lazy val RBRACKET: Parser[String] = token("]")
    lazy val COMMA: Parser[String] = token(",")
    lazy val DOT: Parser[String] = token(".")
    lazy val EQ: Parser[String] = token("=")
    lazy val QUES: Parser[String] = token("?")
    lazy val BAR: Parser[String] = token("|")
    lazy val KEYWORDS: Set[String] = Set(
      "+", "+", "(", ")", "+", "{", "}", "[", "]", ",", ".", "=", "?", "|"
    )

    def root: Parser[Expression] = (line)

    //line ::= expression
    lazy val line: Parser[Expression] = rule(expression)

    //expression ::= or
    lazy val expression: Parser[Expression] = rule(or)

    lazy val or: Parser[Expression] = rule {
      chainl(implicitSequence)(
        (%% << CL(BAR)) ^^ { location => (lhs: Expression, rhs: Expression) => BinaryExpression(location, BinaryOperator.Or, lhs, rhs) }
      )
    }

    lazy val implicitSequence: Parser[Expression] = rule {
      chainl(unary)(
        %% ~ unary ^^ { case location ~ mid => (lhs: Expression, rhs: Expression) => Sequence(location, List(lhs, mid, rhs)) }
      )
    }

    lazy val unary: Parser[Expression] = rule(
      %% ~ primary ~ CL(STAR) ^^ { case location ~ operand ~ _ => Repetition0(location, operand) }
    | %% ~ primary ~ CL(PLUS) ^^ { case location ~ operand ~ _ => Repetition1(location, operand) }
    | primary
    )

    //primary ::= string | explicitSequence | "(" expression ")"
    lazy val primary: Parser[Expression] = rule {
      (
        ident
      | predict(
          '"' -> string,
          '[' -> explicitSequence,
          '(' -> (CL(LPAREN) >> expression << RPAREN),
          '#' -> (((%% << CL(SPECIAL_LPAREN)) ~ expression << RPAREN) ^^ { case  location ~ operand => Capture(location, operand) })
        )
      )
    }

    lazy val string: Parser[Expression] = rule {
      ("\"" >> %% ~ """((?!("|#\{))(\\[rntfb"'\\]|[^\\]))+""".r << "\"" << SPACING_WITHOUT_LF) ^^ {
        case location ~ in => StringNode(location, in)
      }
    }

    lazy val explicitSequence: Parser[Expression] = rule(%% ~ (CL(LBRACKET) >> commit((CL(expression).repeat0By(SEPARATOR) << SEPARATOR.?) << RBRACKET)) ^^ {
      case location ~ contents => Sequence(location, contents)
    })

    lazy val component: Parser[String] = """[A-Za-z_][a-zA-Z0-9_]*""".r

    lazy val ident: Parser[Id] = (%% ~ component.filter { n =>
      !KEYWORDS(n)
    } ^^ { case location ~ name => Id(location, name) }) << SPACING_WITHOUT_LF
  }

  import A2RParsers._

  def parseExpression(input: String): Expression = {
    parse(input) match {
      case Result.Success(expression) => expression
      case Result.Failure(location, message) => throw new ParseException(s"${location}:${message}")
    }
  }

  def parseAll(input: String): Expression = {
    parse(input) match {
      case Result.Success(program) => program
      case Result.Failure(location, message) => throw new ParseException(s"${location}:${message}")
    }
  }
}
