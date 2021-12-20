package com.perhac.tools.smo

import better.files.File
import cats.Show

case class Meta(partsExpected: Int, partsActual: Int, smsType: SmsType, smsStatus: SmsStatus)

sealed trait SmsType
object SmsType {

  def apply(i: Int): SmsType =
    i match {
      case 0x00 => Incoming
      case 0x03 => Outgoing
      case _    => throw new IllegalArgumentException(s"Unrecognised SMS Type: $i")
    }

  case object Incoming extends SmsType

  case object Outgoing extends SmsType
}

sealed trait SmsStatus
object SmsStatus {

  def apply(i: Int): SmsStatus =
    i match {
      case 0x00 => Read
      case 0x01 => Unread
      case 0x03 => Sent
      case 0x04 => Unsent
      case _    => throw new IllegalArgumentException(s"Unrecognised SMS Status: $i")

    }

  case object Read extends SmsStatus
  case object Unread extends SmsStatus
  case object Sent extends SmsStatus
  case object Unsent extends SmsStatus
}

sealed trait AddressType
case object InternationalAddressType extends AddressType
case object LocalAddressType extends AddressType

object AddressType {
  def apply(i: Int): AddressType =
    i match {
      case 0x91 => InternationalAddressType
      case 0x81 => LocalAddressType
      case _    => throw new IllegalArgumentException(s"Unrecognised address type: $i")
    }
}

case class SMS(file: File, from: String, meta: Meta, message: String)
object SMS {
  implicit object SMSShow extends Show[SMS] {
    override def show(sms: SMS): String =
      s"""File: ${sms.file.pathAsString}
         |\tFrom = ${sms.from}
         |\t${sms.meta.partsActual} of ${sms.meta.partsExpected} parts
         |\tSMS Type = ${sms.meta.smsType}, Status = ${sms.meta.smsStatus}
         |\tMessage =
         |${sms.message}
         | """.stripMargin
  }
}
