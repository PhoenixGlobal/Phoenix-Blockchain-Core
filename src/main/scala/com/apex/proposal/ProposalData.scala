package com.apex.proposal

import java.io.DataOutputStream

import com.apex.common.{ApexLogging, Serializable}
import com.apex.crypto.{BinaryData, UInt256}

case class ProposalData(proposalID: UInt256,
                        proposalType: ProposalType.Value,
                        startVoteTime: Long,
                        activeTime: Long,
                        proposalValue: BinaryData,
                        version: Int = 0x01) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(proposalID)
    os.writeByte(proposalType.toByte)
    os.writeLong(startVoteTime)
    os.writeLong(activeTime)
    os.writeByteArray(proposalValue)
  }

}
