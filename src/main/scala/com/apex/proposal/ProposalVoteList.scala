package com.apex.proposal

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.common.ApexLogging
import com.apex.crypto.UInt256

class ProposalVoteList(val votes: Array[ProposalVote],
                       val version: Int = 0x01) extends Serializable with ApexLogging {

  def add(newVote: ProposalVote): ProposalVoteList = {
    new ProposalVoteList(votes ++ Array(newVote))
  }

  def remove(proposalID: UInt256): ProposalVoteList = {
    new ProposalVoteList(votes.filter(v => !(v.proposalID == proposalID)))
  }

  def agreeCount(proposalID: UInt256): Int = {
    votes.count(v => {v.proposalID == proposalID && v.agree})
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.writeSeq(votes)
  }

}

object ProposalVoteList {

  def deserialize(is: DataInputStream): ProposalVoteList = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val votes = is.readSeq(ProposalVote.deserialize)

    new ProposalVoteList(votes.toArray, version)
  }


}