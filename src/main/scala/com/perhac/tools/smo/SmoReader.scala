package com.perhac.tools.smo

import better.files._
import cats.implicits._
import com.perhac.tools.smo.PhoneNumberFormatter.formatPhoneNumber
import com.perhac.tools.smo.SmoReadError.SmoParseError

import scala.util.Try

class SmoReader(buffer: Array[Int], file: File) {

  private def debugBufferAt(offset: Int, length: Int): Unit = {
    buffer.slice(offset, offset + length).foreach(c => print(f"$c%02X "))
    println()
  }
  private def debugBuffer(buf: Array[Int], length: Int = 10): Unit = {
    buf.slice(0, length).foreach(c => print(f"$c%02X "))
    println()
  }

  private def safePerform[A](block: => A): Either[SmoReadError, A] =
    Try(block).toEither.leftMap(t => SmoParseError(file, t.getMessage))

  def readMeta(): Either[SmoReadError, Meta] =
    safePerform {
      Meta(partsExpected = buffer(5), partsActual = buffer(6), SmsType(buffer(7)), SmsStatus(buffer(8)))
    }

  def readSegment(idx: Int, totalCount: Int): Either[SmoReadError, Segment] =
    safePerform {
      val segmentStart = 16 + (idx * 11 * 16)
      val segmentStatus = SegmentStatus(buffer(segmentStart))
      val skip = 2 + buffer(segmentStart + 1) + 3 //[status][len][1,2,3...len][dummy][dummy]
      val addressType = AddressType(buffer(segmentStart + skip))
      val phoneNumberSemiOctets = buffer(segmentStart + skip - 1)
      val phoneNumberBytes = Math.ceil(phoneNumberSemiOctets.toDouble / 2).toInt
      val phoneStartIdx = segmentStart + skip + 1
      val phoneNumber: Array[Int] = buffer.slice(phoneStartIdx, phoneStartIdx + phoneNumberBytes)
      val telNum = phoneNumber.foldLeft("")((tel, c) => tel + f"$c%02X".reverse)
      val formattedNumber = formatPhoneNumber(addressType, telNum)
      val dummyBytes = 3
      val messageLengthOffset = phoneStartIdx + phoneNumberBytes + dummyBytes
      val messageLen = buffer(messageLengthOffset)
      val dumbFillerLength = if (totalCount > 1) 7 else 0
      val messageStart = messageLengthOffset + 1 + dumbFillerLength
      val messageBytes = buffer.slice(messageStart, messageLengthOffset + messageLen + 1)
      val message = messageBytes
        .foldLeft(new StringBuilder()) {
          case (sb, byte) =>
            byte match {
              case zero if zero == 0x00           => sb
              case linebreak if linebreak == 0x0d => sb.append("\n")
              case int                            => sb.append(int.toChar)
            }
        }
        .toString

      Segment(idx, segmentStatus, formattedNumber, message)
    }

  def read(): Either[SmoReadError, SMS] =
    for {
      meta     <- readMeta()
      segments <- (0 until meta.partsActual).toList.traverse(readSegment(_, meta.partsActual))
    } yield SMS(file, meta, segments)

}
