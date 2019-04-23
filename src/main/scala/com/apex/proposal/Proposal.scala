package com.apex.proposal

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.{ApexLogging, Serializable}
import com.apex.crypto.{BinaryData, UInt160, UInt256}

case class Proposal(proposalID: UInt256,
                    proposalType: ProposalType.Value,
                    startVoteTime: Long,
                    endVoteTime: Long,
                    activeTime: Long,
                    voters: Array[UInt160],
                    proposalValue: BinaryData,
                    version: Int = 0x01) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(proposalID)
    os.writeByte(proposalType.toByte)
    os.writeLong(startVoteTime)
    os.writeLong(endVoteTime)
    os.writeLong(activeTime)
    os.writeSeq(voters)
    os.writeByteArray(proposalValue)
  }

}

object Proposal {

  def deserialize(is: DataInputStream): Proposal = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val proposalID = UInt256.deserialize(is)
    val proposalType = ProposalType(is.readByte)
    val startVoteTime = is.readLong()
    val endVoteTime = is.readLong()
    val activeTime = is.readLong()
    val voters = is.readSeq(UInt160.deserialize)
    val proposalValue = is.readByteArray

    new Proposal(proposalID, proposalType, startVoteTime, endVoteTime, activeTime, voters.toArray, proposalValue, version)
  }

}
