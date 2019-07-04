package com.github.kmizu.a2r

import java.io.File

import com.github.scaruby.SFile

import scala.collection.Iterator.continually

/**
 * @author Kota Mizushima
 */
object Main {
  def main(args: Array[String]): Unit = {
    parseCommandLine(args) match {
      case Some(("-e", line)) =>
        val input = line
        val parser = new Parser
        println(parser.parseAll(line))
      case Some(("-f", fileName)) =>
        val input = SFile.read(fileName)
        val parser = new Parser
        println(parser.parseAll(input))
      case _ =>
        Console.err.println(
          """
            |Usage: java -jar a2r.jar (-e <expression> | <fileName>)
            |-e <expression> : translate <expression>
            |<fileName>      : read an AST from <fileName> (.a2r) and translate it
          """.stripMargin)
    }
  }

  def parseCommandLine(args: Array[String]): Option[(String, String)] = {
    args match {
      case Array("-e", line) => Some("-e" -> line)
      case Array(fileName) => Some("-f" -> fileName)
      case otherwise  => None
    }
  }
}

