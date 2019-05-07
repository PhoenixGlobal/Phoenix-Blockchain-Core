package com.apex.proposal

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

case class ProposalVote(proposalID: UInt256,
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

object ProposalVote {

  def deserialize(is: DataInputStream): ProposalVote = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val proposalID = UInt256.deserialize(is)
    val voter = UInt160.deserialize(is)
    val agree = is.readBoolean()

    new ProposalVote(proposalID, voter, agree, version)
  }

  implicit val proposalVoteWrites = new Writes[ProposalVote] {
    override def writes(o: ProposalVote): JsValue = {
      Json.obj(
        "proposalID" -> o.proposalID.toString,
        "voter" -> o.voter.address,
        "agree" -> o.agree,
        "version" -> o.version
      )
    }
  }

}
