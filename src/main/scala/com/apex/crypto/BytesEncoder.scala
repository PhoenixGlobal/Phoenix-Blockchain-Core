package com.apex.crypto

import scala.util.Try

trait BytesEncoder {
  val Alphabet: String

  def encode(input: Array[Byte]): String

  def decode(input: String): Try[Array[Byte]]
}