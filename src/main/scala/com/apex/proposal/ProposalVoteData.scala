package com.apex.proposal

import java.io.DataOutputStream

import com.apex.common.Serializable
import com.apex.crypto.{UInt160, UInt256}

case class ProposalVoteData(proposalID: UInt256,
                            voter: UInt160,
                            agree: Boolean,
                            version: Int = 0x01) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(proposalID)
    os.write(voter)
    os.writeBoolean(agree)
  }


}
