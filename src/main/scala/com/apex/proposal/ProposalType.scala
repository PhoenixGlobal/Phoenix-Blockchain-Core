package com.apex.proposal

object ProposalType extends Enumeration {
  val BlockReward = Value(0x01)
  val BlockGasLimit = Value(0x02)
  val TxGasLimit = Value(0x03)

  implicit class Extension(val value: ProposalType.Value) {
    def toByte: Byte = value.id.toByte
  }

}