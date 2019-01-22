package com.apex.vm

import com.apex.consensus.{RegisterData, VoteData}
import com.apex.core.{DataBase, OperationType, Transaction}
import com.apex.crypto.FixedNumber
import com.apex.crypto.FixedNumber.One

object VoteContractExecutor {
  implicit class VoteContractContext(track: DataBase){

    def isAccountExist(data: Array[Byte], tx: Transaction): Boolean = {
      //val voteData = VoteData.fromBytes(data)
      if(track.getAccount(tx.from).isDefined)
        return true
      false
    }

    def isAccountBalanceEnough(data: Array[Byte], tx: Transaction): Boolean ={
      val voteData = VoteData.fromBytes(data)
      val account = track.getAccount(tx.from).get
      if(voteData.operationType == OperationType.register &&
        (!account.balance.> (voteData.voterCount))) return false
      true
    }

    def isVoteWitnessExist(data: Array[Byte]): Boolean = {
      val voteData = VoteData.fromBytes(data)
      val witness = track.getWitness(voteData.candidate)
      if(witness.isEmpty) return false
      true
    }

    def isCancelWitnessNotExist(data: Array[Byte]): Boolean = {
      val voteData = VoteData.fromBytes(data)
      val witness = track.getWitness(voteData.candidate)
      if(witness.isEmpty && voteData.operationType ==OperationType.resisterCancel){
        return false
      }
      true
    }

    def processReq(data: Array[Byte], track: DataBase, tx: Transaction): Boolean ={
      val voteData = VoteData.fromBytes(data)
      val witness = track.getWitness(voteData.candidate).get
      if(voteData.operationType == OperationType.register){
        track.addBalance(tx.from, -voteData.voterCount)
        witness.updateVoteCounts(voteData.voterCount)
        track.createWitness(witness.addr, witness)
        true
      }
      else {
        track.addBalance(tx.from, voteData.voterCount)
        witness.updateVoteCounts(-voteData.voterCount)
        track.createWitness(witness.addr, witness)
        true
      }
    }

  }
}
