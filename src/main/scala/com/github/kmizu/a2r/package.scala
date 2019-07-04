package com.github.kmizu

import java.io.{BufferedReader, FileInputStream, InputStreamReader}

import scala.language.reflectiveCalls

/**
 * @author Kota Mizushima
 */
package object a2r {
  def openReader[A](fileName: String)(f: BufferedReader => A): A = {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), "UTF-8"))
    using(reader)(f)
  }
  def using[A <: AutoCloseable, B](resource: A)(f: A => B): B = try {
    f(resource)
  } finally {
    scala.util.control.Exception.allCatch(resource.close())
  }
}
