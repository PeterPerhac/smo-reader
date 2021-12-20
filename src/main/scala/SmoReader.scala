import SmoReadError.{CannotOpenFile, SmoIOException, SmoParseError}
import better.files._
import cats.Show
import cats.implicits._
import com.monovore.decline.Opts._
import com.monovore.decline._

import java.io.IOException
import scala.util.Try

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

case class SMS(file: File, from: String, parts: Int, message: String)
object SMS {
  implicit object SMSShow extends Show[SMS] {
    override def show(sms: SMS): String =
      s"""File: ${sms.file.pathAsString}
         |\tFrom = ${sms.from}
         |\tParts = ${sms.parts}
         |\tMessage = ${sms.message}
         | """.stripMargin
  }
}

object SmoReaderApp
    extends CommandApp(
      name = "smo-reader",
      header = "SMO file reader",
      main = {

        val processSmo: File => Either[SmoReadError, SMS] = f => {
          if (!f.isReadable) Left(CannotOpenFile(f))
          else {
            f.fileInputStream.apply { input =>
              val charStream: LazyList[Char] =
                LazyList.continually(input.read()).takeWhile(_ != -1).map(_.toChar)

              try {
                new SmoReader(charStream, f).read()
              } catch {
                case e: IOException => Left(SmoIOException(f, e))
              }
            }
          }
        }

        val dirOpt: Opts[String] =
          option[String]("directory", help = "directory to scan for .smo files", short = "d")

        val verboseOpt: Opts[Boolean] =
          Opts.flag(long = "verbose", help = "Be more verbose in the output", short = "v").orFalse

        (dirOpt, verboseOpt).mapN {
          (dir, verbose) =>
            val directory = File(dir)
            if (!directory.isDirectory) {
              System.err.println(s"$dir is not a directory")
              System.exit(1)
            }
            val smos = directory.list(_.extension.contains(".smo")).toVector.sortBy(_.nameWithoutExtension)
            if (verbose) {
              println(s"Scanning for SMO files in ${directory.pathAsString}")
              println(s"Found ${smos.size} files:")
            }
            val (errors, messages): (Vector[SmoReadError], Vector[SMS]) = smos.map(processSmo).partitionEither(identity)
            if (verbose) {
              if (errors.nonEmpty) {
                println(s"Encountered ${errors.size} errors:")
                errors.foreach(e => println(e.errorMessage))
              }
            }
            messages.foreach(message => println(message.show))
        }
      }
    )

class SmoReader(charStream: LazyList[Char], file: File) {

  def readParts(stream: LazyList[Char]): Either[SmoReadError, Int] =
    Try(stream.drop(5).head.toInt).toEither.leftMap(t => SmoParseError(file, t.getMessage))

  def readFrom(stream: LazyList[Char]): Either[SmoReadError, String] =
    Try{
      val howMany = stream.drop(27).head.toInt
      val fromBytes: LazyList[Char] = stream.slice(29, howMany / 2 + 29)
      fromBytes.foldLeft("")((tel, c) => tel + f"$c%02X".reverse)
    }.toEither.leftMap(t => SmoParseError(file, t.getMessage))


  def readMessage(stream: LazyList[Char]): Either[SmoReadError, String] = Right("message")

  def read(): Either[SmoReadError, SMS] =
    for {
      parts   <- readParts(charStream)
      from    <- readFrom(charStream)
      message <- readMessage(charStream)
    } yield SMS(file, from, parts, message)

}
