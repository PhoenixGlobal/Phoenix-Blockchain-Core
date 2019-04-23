package com.apex.vm.precompiled

import com.apex.core.{DataBase, Transaction}
import com.apex.proposal.{Proposal, ProposalData}
import com.apex.vm.VM

class ProposalContract(track: DataBase,
                       tx: Transaction,
                       timeStamp: Long) extends PrecompiledContract {

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

      track.setProposal(new Proposal(tx.id,
        proposalData.proposalType,
        timeStamp,
        timeStamp + 72 * 3600 * 1000,
        proposalData.activeTime,
        Array.empty,
        proposalData.proposalValue
      ))

      (true, Array.empty)

    }
    catch {
      case e: Throwable => {
        (false, ("ProposalContract throw Exception:" + e.toString).getBytes())
      }
    }
  }

  private def checkValid() = {

  }

}
