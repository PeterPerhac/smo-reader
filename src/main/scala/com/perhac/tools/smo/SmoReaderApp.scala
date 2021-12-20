package com.perhac.tools.smo

import better.files.File
import cats.implicits._
import com.monovore.decline.Opts._
import com.monovore.decline.{CommandApp, Opts}
import com.perhac.tools.smo.SmoReadError.{CannotOpenFile, SmoIOException}

import java.io.IOException

object SmoReaderApp
  extends CommandApp(
    name = "smo-reader",
    header = "SMO file reader",
    main = {

      val processSmo: File => Either[SmoReadError, SMS] = f => {
        if (!f.isReadable) Left(CannotOpenFile(f))
        else {
          f.fileInputStream.apply { input =>
            val charStream: LazyList[Int] =
              LazyList.continually(input.read()).takeWhile(_ != -1)
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
            smos.foreach(println)
          }
          val (errors, messages): (Vector[SmoReadError], Vector[SMS]) = smos.map(processSmo).partitionEither(identity)
          if (verbose) {
            if (errors.nonEmpty) {
              println(s"Encountered ${errors.size} errors:")
              errors.foreach(e => println(e.errorMessage))
            }
          }
          messages.zipWithIndex.foreach({case (message, idx) =>
            println(s"Rank: $idx")
            println(message.show)})
      }
    }
  )