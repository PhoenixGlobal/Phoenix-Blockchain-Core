package com.apex.proposal

object ProposalType extends Enumeration {
  val BlockAward = Value(0x01)
  val TxMinGasPrice = Value(0x02)
  val TxMaxGasLimit = Value(0x03)
  //val BlockGasLimit = Value(0x02)
  //val TxDataMaxSize = Value(0x04)
  //val ScheduleTxEnable = Value(0x05)

  implicit class Extension(val value: ProposalType.Value) {
    def toByte: Byte = value.id.toByte
  }

}

object ProposalStatus extends Enumeration {
  val Voting = Value(0x01)
  val PendingActive = Value(0x02)

  implicit class Extension(val value: ProposalStatus.Value) {
    def toByte: Byte = value.id.toByte
  }

}