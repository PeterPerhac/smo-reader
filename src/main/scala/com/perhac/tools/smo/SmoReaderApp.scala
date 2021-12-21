package com.perhac.tools.smo

import better.files.File
import cats.implicits._
import com.monovore.decline.Opts._
import com.monovore.decline.{CommandApp, Opts}
import com.perhac.tools.smo.SmoReadError.{CannotOpenFile, SmoIOException}

import java.io.IOException
import java.lang.Byte.toUnsignedInt

object SmoReaderApp
    extends CommandApp(
      name = "smo-reader",
      header = "SMO file reader",
      main = {

        val processSmo: File => Either[SmoReadError, SMS] = f => {
          if (!f.isReadable) Left(CannotOpenFile(f))
          else {
            f.fileInputStream.apply { input =>
              val buffer = input.readAllBytes()
              try {
                new SmoReader(buffer.map(toUnsignedInt), f).read()
              } catch {
                case e: IOException => Left(SmoIOException(f, e))
              }
            }
          }
        }

        val inputDirOrFile: Opts[String] =
          option[String]("directory", help = "directory to scan for .smo files", short = "d")
            .orElse(option[String]("file", help = "path to an .smo file", short = "f"))

        val verboseOpt: Opts[Boolean] =
          Opts.flag(long = "verbose", help = "Be more verbose in the output", short = "v").orFalse

        (inputDirOrFile, verboseOpt).mapN {
          (fileOrDir, verbose) =>
            val input = File(fileOrDir)
            if (!input.exists) {
              System.err.println(s"$input does not exist!")
              System.exit(1)
            }
            val smos = if (input.isDirectory){
              input.list(_.extension.contains(".smo")).toVector.sortBy(_.nameWithoutExtension)
            } else {
              if (input.isRegularFile) Vector(input) else Vector.empty[File]
            }
            val (errors, messages): (Vector[SmoReadError], Vector[SMS]) = smos.map(processSmo).partitionEither(identity)
            if (verbose) {
              if (errors.nonEmpty) {
                println(s"Encountered ${errors.size} errors:")
                errors.foreach(e => println(e.errorMessage))
              }
            }
            messages.zipWithIndex.foreach({
              case (message, idx) =>
                println(s"Message number ${idx + 1}")
                println(message.show)
            })
        }
      }
    )
