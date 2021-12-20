package com.perhac.tools.smo

object PhoneNumberFormatter {

  def formatPhoneNumber(t: AddressType, n: String): String = {
    def stripTrailingF(s: String): String =
      if (s.toLowerCase().endsWith("f")) s.substring(0, s.length - 1) else s
    t match {
      case InternationalAddressType => s"+${stripTrailingF(n)}"
      case LocalAddressType         => stripTrailingF(n)
    }
  }
}
