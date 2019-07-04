package com.github.kmizu.a2r

import org.scalatest.{DiagrammedAssertions, FunSpec}

trait SpecHelper extends FunSpec with DiagrammedAssertions {
  def parser: Parser = new Parser
}
