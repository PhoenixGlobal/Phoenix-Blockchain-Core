package com.apex.proposal

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{UInt160, UInt256}

case class ProposalVoteData(proposalID: UInt256,
                            agree: Boolean,
                            version: Int = 0x01) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(proposalID)
    os.writeBoolean(agree)
  }

}

object ProposalVoteData {

  def fromBytes(data: Array[Byte]): ProposalVoteData = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }

  def deserialize(is: DataInputStream): ProposalVoteData = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val proposalID = UInt256.deserialize(is)
    val agree = is.readBoolean()

    new ProposalVoteData(proposalID, agree, version)
  }

}
