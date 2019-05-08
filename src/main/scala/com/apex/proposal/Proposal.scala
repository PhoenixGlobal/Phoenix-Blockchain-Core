package com.apex.proposal

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.{ApexLogging, Helper, Serializable}
import com.apex.crypto.{BinaryData, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

case class Proposal(proposalID: UInt256,
                    proposalType: ProposalType.Value,
                    status: ProposalStatus.Value,
                    startVoteTime: Long,
                    endVoteTime: Long,
                    activeTime: Long,
                    voters: Array[UInt160],
                    proposalValue: BinaryData,
                    version: Int = 0x01) extends Serializable {

  def voterValid(voter: UInt160): Boolean = {
    voters.count(p => p == voter) > 0
  }

  def setNewStatus(newStatus: ProposalStatus.Value): Proposal = {
    new Proposal(proposalID, proposalType, newStatus, startVoteTime, endVoteTime,
      activeTime, voters, proposalValue)
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(proposalID)
    os.writeByte(proposalType.toByte)
    os.writeByte(status.toByte)
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
    val status = ProposalStatus(is.readByte)
    val startVoteTime = is.readLong()
    val endVoteTime = is.readLong()
    val activeTime = is.readLong()
    val voters = is.readSeq(UInt160.deserialize)
    val proposalValue = is.readByteArray

    new Proposal(proposalID, proposalType, status, startVoteTime, endVoteTime, activeTime, voters.toArray, proposalValue, version)
  }

  implicit val proposalWrites = new Writes[Proposal] {
    override def writes(o: Proposal): JsValue = {
      Json.obj(
        "proposalID" -> o.proposalID.toString,
        "proposalType" -> o.proposalType,
        "status" -> o.status,
        "startVoteTime" -> Helper.timeString(o.startVoteTime),
        "endVoteTime" ->  Helper.timeString(o.endVoteTime),
        "activeTime" -> Helper.timeString(o.activeTime),
        "voters" -> o.voters,
        "proposalValue" -> o.proposalValue.toString
      )
    }
  }

}
