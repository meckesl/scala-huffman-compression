package com.lms.comp.util

import java.math.BigInteger

object HexByte {
  def toHex(bytes: Seq[Byte]): String = new BigInteger(bytes.toArray).toString(16)
  def toBytes(hex: String) : Array[Byte] = new BigInteger(hex, 16).toByteArray
}