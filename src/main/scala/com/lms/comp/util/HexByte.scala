package com.lms.comp.util

import java.math.BigInteger

object HexByte {

  def toHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

  def toBytes(hex: String) : Array[Byte] = {
    new BigInteger(hex, 16).toByteArray
  }

}
