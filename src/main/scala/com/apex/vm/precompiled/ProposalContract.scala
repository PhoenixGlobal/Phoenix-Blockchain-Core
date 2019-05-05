package com.apex.vm.precompiled

import com.apex.common.ApexLogging
import com.apex.core.{DataBase, Transaction}
import com.apex.proposal.{Proposal, ProposalData, ProposalStatus}
import com.apex.vm.VM

class ProposalContract(track: DataBase,
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
      val proposalData = ProposalData.fromBytes(data)
      log.info(s"new proposal: proposer=${tx.from.shortAddr} type=${proposalData.proposalType} activeTime=${proposalData.activeTime} proposalValue=${proposalData.proposalValue.toString} txid=${tx.id}")
      val (valid, errString) = checkValid(proposalData)
      if (valid) {
        log.info(s"valid voters:")
        track.getLastWeekValidVoters().foreach(p => {
          log.info(s"   ${p.shortAddr}")
        })
        track.setProposal(new Proposal(tx.id,
          proposalData.proposalType,
          ProposalStatus.Voting,
          timeStamp,
          timeStamp + 72 * 3600 * 1000,
          proposalData.activeTime,
          track.getLastWeekValidVoters(),
          proposalData.proposalValue
        ))
        (true, Array.empty)
      }
      else {
        (valid, errString)
      }
    }
    catch {
      case e: Throwable => {
        (false, ("ProposalContract throw Exception:" + e.toString).getBytes())
      }
    }
  }

  private def checkValid(proposalData: ProposalData): (Boolean, Array[Byte]) = {
    var valid = true
    var errString = Array.empty[Byte]
    track.getAllProposal().foreach(p => {
     if (p.proposalType == proposalData.proposalType) {
       valid = false
       errString = ("same proposal type already ongoing").getBytes
     }
    })
    if (proposalData.activeTime != 0 && proposalData.activeTime < timeStamp + 72 * 3600 * 1000) {
      valid = false
      errString = ("activeTime too early").getBytes
    }
    var proposalIniterValid = false
    track.getLastWeekValidVoters().foreach(p => {
      if (p == tx.from)
        proposalIniterValid = true
    })
    if (proposalIniterValid == false) {
      valid = false
      errString = ("proposal initiator not valid").getBytes
    }
    (valid, errString)
  }

}
