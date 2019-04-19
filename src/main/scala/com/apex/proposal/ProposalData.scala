package com.apex.proposal

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.{ApexLogging, Serializable}
import com.apex.crypto.{BinaryData, UInt160, UInt256}

case class ProposalData(proposalType: ProposalType.Value,
                        activeTime: Long,
                        proposalValue: BinaryData,
                        version: Int = 0x01) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.writeByte(proposalType.toByte)
    os.writeLong(activeTime)
    os.writeByteArray(proposalValue)
  }

}

object ProposalData {

  def deserialize(is: DataInputStream): ProposalData = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val proposalType = ProposalType(is.readByte)
    val activeTime = is.readLong()
    val proposalValue = is.readByteArray

    new ProposalData(proposalType, activeTime, proposalValue, version)
  }

}
