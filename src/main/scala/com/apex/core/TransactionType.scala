package com.apex.core

object TransactionType extends Enumeration {
  val Miner = Value(0x00)
  val Transfer = Value(0x01)
  val Fee = Value(0x02)

  implicit class Extension(val value: TransactionType.Value) {
    def toByte: Byte = value.id.toByte
  }

}
