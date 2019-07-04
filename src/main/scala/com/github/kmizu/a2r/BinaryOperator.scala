package com.github.kmizu.a2r

/**
  * Created by Mizushima on 2016/05/20.
  */
sealed abstract class BinaryOperator(val descriptor: String)
object BinaryOperator {
  case object Or extends BinaryOperator("|")
}
