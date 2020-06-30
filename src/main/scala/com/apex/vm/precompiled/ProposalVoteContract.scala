package com.apex.vm.precompiled

import com.apex.common.ApexLogging
import com.apex.core.{DataBase, Transaction}
import com.apex.proposal.{Proposal, ProposalData, ProposalVote, ProposalVoteData}
import com.apex.vm.VM

class ProposalVoteContract(track: DataBase,
                           tx: Transaction,
                           timeStamp: Long) extends PrecompiledContract with ApexLogging {

  override def getGasForData(data: Array[Byte]): Long = {
    if (data == null) {
      60
    } else {
      60 + VM.getSizeInWords(data.length) * 12
    }
  }

  override def execute(data: Array[Byte]): (Boolean, Array[Byte]) = {
    try {
      val ownerAddress = track.getWitness(tx.from).orNull.ownerAddress
      if(!ownerAddress.equals(tx.from) && ownerAddress!= null) return (false, ("the witness has an owner and has no right to vote a proposal").getBytes())
      val proposalVoteData = ProposalVoteData.fromBytes(data)
      log.info(s"new proposal vote, voter=${tx.from.shortAddr} agree=${proposalVoteData.agree} proposalID=${proposalVoteData.proposalID}")
      val (valid, errString) = checkValid(proposalVoteData)
      if (valid) {
        log.info("vote valid, add to databae")
        track.addProposalVote(new ProposalVote(
          proposalVoteData.proposalID,
          tx.from,
          proposalVoteData.agree
        ))
        (true, Array.empty)
      }
      else {
        (valid, errString)
      }
    }
    catch {
      case e: Throwable => {
        (false, ("ProposalVoteContract throw Exception:" + e.toString).getBytes())
      }
    }
  }

  private def checkValid(proposalVoteData: ProposalVoteData): (Boolean, Array[Byte]) = {
    var valid = true
    var errString = Array.empty[Byte]

    var proposalExist = false
    var targetProposal: Proposal = null
    track.getAllProposal().foreach(p => {
      if (p.proposalID == proposalVoteData.proposalID) {
        proposalExist = true
        targetProposal = p
      }
    })
    if (!proposalExist) {
      valid = false
      errString = ("proposal not exist or time out").getBytes
    }
    else if (track.getProposalVoteList().contain(proposalVoteData.proposalID, tx.from)) {
      valid = false
      errString = ("voter have already voted this proposal, can NOT vote again").getBytes
    }
    else {
      if (!targetProposal.voterValid(tx.from)) {
        valid = false
        errString = ("voter not valid").getBytes
      }
    }

    (valid, errString)
  }

}
