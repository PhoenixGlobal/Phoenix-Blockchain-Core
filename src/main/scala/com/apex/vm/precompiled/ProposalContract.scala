package com.apex.vm.precompiled

import java.io.{ByteArrayInputStream, DataInputStream}

import com.apex.common.ApexLogging
import com.apex.core.{DataBase, Transaction}
import com.apex.crypto.FixedNumber
import com.apex.proposal.{Proposal, ProposalData, ProposalStatus, ProposalType}
import com.apex.vm.VM

class ProposalContract(track: DataBase,
                       tx: Transaction,
                       timeStamp: Long) extends PrecompiledContract with ApexLogging {
  // 72 hour
  private val voteTime: Long = 72 * 3600 * 1000  // 1800 * 1000

  override def getGasForData(data: Array[Byte]): Long = {
    if (data == null) {
      60
    } else {
      60 + VM.getSizeInWords(data.length) * 12
    }
  }

  override def execute(data: Array[Byte]): (Boolean, Array[Byte]) = {
    try {
      val owner = track.getWitness(tx.from)
      if(owner.isDefined && owner.get.ownerAddress != null && owner.get.ownerAddress.equals(tx.from))
        return (false, ("the witness has an owner and has no right to raise a proposal").getBytes())
      val proposalData = ProposalData.fromBytes(data)
      log.info(s"new proposal: proposer=${tx.from.shortAddr} type=${proposalData.proposalType} activeTime=${proposalData.activeTime} proposalValue=${proposalData.proposalValue.toString} txid=${tx.id}")
      if (proposalData.proposalType == ProposalType.BlockAward
       || proposalData.proposalType == ProposalType.TxMinGasPrice
       || proposalData.proposalType == ProposalType.TxMaxGasLimit) {
        val bs = new ByteArrayInputStream(proposalData.proposalValue)
        val is = new DataInputStream(bs)
        val newValue = FixedNumber.deserialize(is)
        log.info(s"new value is ${newValue}")
      }
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
          timeStamp + voteTime,
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
     if (p.proposalType == proposalData.proposalType && p.activeTime == proposalData.activeTime) {
       valid = false
       errString = ("same proposal type with same activeTime already ongoing").getBytes
     }
    })
    if (proposalData.activeTime != 0 && proposalData.activeTime < timeStamp + voteTime) {
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
