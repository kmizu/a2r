package com.github.kmizu.a2r

import com.github.kmizu.a2r.Ast._

class ParserSpec extends SpecHelper {
  describe("string literal") {
    it("is parsed correctly") {
      val result = parser.parseAll(""""abc"""")
      assertResult(StringNode(SourceLocation(1, 2), "abc"))(result)
    }
  }
  describe("explicit sequence") {
    it("is parsed correctly") {
      val result = parser.parseAll("""[a b c]""")
      assertResult(
        Sequence(SourceLocation(1,1),List(Sequence(SourceLocation(1,4),List(Id(SourceLocation(1,2),"a"), Id(SourceLocation(1,4),"b"), Id(SourceLocation(1,6),"c")))))
      )(result)
    }
  }
  describe("implicit sequence") {
    it("a simple expression is parsed correctly") {
      val result = parser.parseAll("""a b c""")
      assertResult(
        Sequence(SourceLocation(1,3),List(Id(SourceLocation(1,1),"a"), Id(SourceLocation(1,3),"b"), Id(SourceLocation(1,5),"c")))
      )(result)
    }
  }
  describe("repetition (*)") {
    it("a simple expression is parsed correctly") {
      val result = parser.parseAll("""a*""")
      assertResult(
        Repetition0(SourceLocation(1,1),Id(SourceLocation(1,1),"a"))
      )(result)
    }
  }
  describe("repetition (+)") {
    it("a simple expression is parsed correctly") {
      val result = parser.parseAll("""a+""")
      assertResult(
        Repetition1(SourceLocation(1,1),Id(SourceLocation(1,1),"a"))
      )(result)
    }
  }
  describe("alternation (|)") {
    it("a simple expression is parsed correctly") {
      val result = parser.parseAll("""a | b""")
      assertResult(
        BinaryExpression(SourceLocation(1,3),BinaryOperator.Or,Id(SourceLocation(1,1),"a"),Id(SourceLocation(1,5),"b"))
      )(result)
    }
    it("a complex expression is parsed correctly") {
      val result = parser.parseAll("""a | b | c | d""")
      assertResult(
        BinaryExpression(
          SourceLocation(1,11),
          BinaryOperator.Or,
          BinaryExpression(
            SourceLocation(1,7),
            BinaryOperator.Or,
            BinaryExpression(
              SourceLocation(1,3),
              BinaryOperator.Or,
              Id(SourceLocation(1,1),"a"),
              Id(SourceLocation(1,5),"b")),
            Id(SourceLocation(1,9),"c")),
          Id(SourceLocation(1,13),"d"))
      )(result)
    }
  }
  describe("grouping (`#(`)") {
    it("a simple expression is parsed correctly") {
      val result = parser.parseAll("""#(a) | b""")
      assertResult(
        BinaryExpression(
          SourceLocation(1,6),
          BinaryOperator.Or,
          Capture(
            SourceLocation(1,1),
            Id(
              SourceLocation(1,3),
              "a"
            )
          ),
          Id(
            SourceLocation(1,8),
            "b"
          )
        )
      )(result)
    }
  }
  describe("|, *, and +") {
    it("is parsed correctly") {
      val result = parser.parseAll("""a | (b* | c)+ | d | "foo"""")
      println(result)
      assertResult(
        BinaryExpression(
          SourceLocation(1,19),
          BinaryOperator.Or,
          BinaryExpression(
            SourceLocation(1,15),
            BinaryOperator.Or,
            BinaryExpression(
              SourceLocation(1,3),
              BinaryOperator.Or,
              Id(SourceLocation(1,1),"a"),
              Repetition1(
                SourceLocation(1,5),
                BinaryExpression(
                  SourceLocation(1,9),
                  BinaryOperator.Or,
                  Repetition0(
                    SourceLocation(1,6),
                    Id(SourceLocation(1,6),"b")
                  ),
                  Id(SourceLocation(1,11),"c")
                )
              )
            ),
            Id(SourceLocation(1,17),"d")
          ),
          StringNode(SourceLocation(1,22),"foo")
        )
      )(result)
    }
  }
}
