package com.github.kmizu.a2r

/**
 * @author Kota Mizushima
 */

sealed abstract class Ast {
  val location: Location
}

object Ast {
  sealed abstract class Expression extends Ast

  case class BinaryExpression(location: Location, operator: BinaryOperator, lhs: Expression, rhs: Expression) extends Expression

  case class Repetition0(location: Location, operand: Expression) extends Expression

  case class Repetition1(location: Location, operand: Expression) extends Expression

  case class StringNode(location: Location, value: String) extends Expression

  case class Capture(location: Location, operand: Expression) extends Expression

  case class Id(location: Location, name: String) extends Expression
  object Id {
    def apply(name: String): Id = Id(NoLocation, name)
  }

  case class Sequence(location: Location, elements: List[Expression]) extends Expression
}