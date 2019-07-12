package com.apex.core

object TransactionType extends Enumeration {
  val Miner = Value(0x00)
  val Transfer = Value(0x01)
  val Deploy = Value(0x02)
  val Call = Value(0x03)
  val Refund = Value(0x04)
  val Schedule = Value(0x05)

  implicit class Extension(val value: TransactionType.Value) {
    def toByte: Byte = value.id.toByte
  }

}

object OperationType extends Enumeration {
  val register = Value(0x00)
  val resisterCancel = Value(0x01)

  implicit class Extension(val value: OperationType.Value) {
    def toByte: Byte = value.id.toByte
  }
}
