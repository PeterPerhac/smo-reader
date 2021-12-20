package com.perhac.tools.smo

import better.files.File

import java.io.IOException

sealed trait SmoReadError {
  def errorMessage: String
}
object SmoReadError {
  case class CannotOpenFile(f: File) extends SmoReadError {
    override def errorMessage: String = s"File could not be opened: $f"
  }

  case class SmoIOException(f: File, e: IOException) extends SmoReadError {
    override def errorMessage: String = s"IO Exception when processing file $f. Message: ${e.getMessage}"
  }

  case class SmoParseError(f: File, cause: String) extends SmoReadError {
    override def errorMessage: String = s"Failed to parse file: $f because: $cause"
  }
}