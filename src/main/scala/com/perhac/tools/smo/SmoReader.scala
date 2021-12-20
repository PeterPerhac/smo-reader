package com.perhac.tools.smo

import better.files._
import cats.implicits._
import com.perhac.tools.smo.PhoneNumberFormatter.formatPhoneNumber
import com.perhac.tools.smo.SmoReadError.SmoParseError

import scala.util.Try

class SmoReader(intStream: LazyList[Int], file: File) {

  def readMeta(stream: LazyList[Int]): Either[SmoReadError, (Meta, LazyList[Int])] =
    Try {
      val expected #:: actual #:: smsType #:: smsStatus #:: tail = stream.drop(5)
      (Meta(expected, actual, SmsType(smsType), SmsStatus(smsStatus)), tail)
    }.toEither.leftMap(t => SmoParseError(file, t.getMessage))

  def readFrom(stream: LazyList[Int]): Either[SmoReadError, (String, LazyList[Int])] =
    Try {
      val address = stream.drop(18)
      val howMany = address.head
      val addressType = AddressType(address.drop(1).head)
      val bytesToRead = Math.ceil(howMany / 2).toInt
      val fromBytes: LazyList[Int] = address.slice(2, bytesToRead + 2)
      val telNum = fromBytes.foldLeft("")((tel, c) => tel + f"$c%02X".reverse)
      (formatPhoneNumber(addressType, telNum), address.drop(2 + bytesToRead))
    }.toEither.leftMap(t => SmoParseError(file, t.getMessage))

  def readMessage(stream: LazyList[Int], meta: Meta): Either[SmoReadError, String] =
    Try {
      val msg1 = if (meta.partsExpected > 1) stream.drop(11) else stream.drop(4)
      val messageBuilder = msg1.takeWhile(_ != 0xff).foldLeft(new StringBuilder()){
        case (sb, byte) => byte match {
          case zero if zero == 0x00 => sb
          case linebreak if linebreak == 0x0d => sb.append("\n")
          case int => sb.append(int.toChar)
        }
      }

      messageBuilder.toString()
    }.toEither.leftMap(t => SmoParseError(file, t.getMessage))

  def read(): Either[SmoReadError, SMS] =
    readMeta(intStream).flatMap {
      case (meta, stream1) =>
        readFrom(stream1).flatMap {
          case (from, stream2) => readMessage(stream2, meta).map(message => SMS(file, from, meta, message))
        }
    }
}
