package com.apex.vm

import com.apex.consensus.{Vote, VoteData, WitnessInfo}
import com.apex.core.{DataBase, OperationType, Transaction}

object VoteContractExecutor {

  import VoteOperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction): (Boolean, Array[Byte]) ={
    val voteData = VoteData.fromBytes(data)
    voteData.isAccountExist(track, tx)
      .isAccountBalanceEnough(track, tx)
      .isVoteWitnessExist(track)
      .isCancelWitnessNotExist(track)
      .isVoterExist(track, tx)
      .voterWitnessChanged(track, tx)
      .voterCancelWitnessCounterInvalid(track, tx)
      .processReq(track, tx)
      .returnResult()
  }

  implicit class VoteContractContext(voteData:VoteData){
    def isAccountExist(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        if(track.getAccount(tx.from).isEmpty){
          setResult(false)
        }
      }
      this
    }



    def isAccountBalanceEnough( track: DataBase, tx: Transaction): VoteContractContext ={
      errorDetected{
        val account = track.getAccount(tx.from).get
        if(voteData.operationType == OperationType.register &&
          (!account.balance.> (voteData.voterCount))){
          setResult(false)
        }
      }
      this
    }

    def isVoteWitnessExist(track: DataBase): VoteContractContext = {
      errorDetected{
        val witness = track.getWitness(voteData.candidate)
        if(witness.isEmpty){
          setResult(false)
        }
      }
      this
    }

    def isCancelWitnessNotExist(track: DataBase): VoteContractContext = {
      errorDetected{
        val witness = track.getWitness(voteData.candidate)
        if(witness.isEmpty && voteData.operationType ==OperationType.resisterCancel){
          setResult(false)
        }
      }
      this
    }

    def isVoterExist(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        if(voteData.operationType == OperationType.resisterCancel && track.getVote(tx.from).isEmpty){
          setResult(false)
        }
      }
      this
    }

    def voterWitnessChanged(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        track.getVote(tx.from).fold()(vote => {
          if(vote.target != voteData.candidate){
            setResult(false)
          }
        })
      }
      this
    }

    def voterCancelWitnessCounterInvalid(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        val vote = track.getVote(tx.from).get
        if(voteData.operationType == OperationType.resisterCancel && voteData.voterCount.>(vote.count)) {
          setResult(false)
        }
      }
      this
    }

    def processReq(track: DataBase, tx: Transaction): VoteContractContext ={
      errorDetected{
        val witness = track.getWitness(voteData.candidate).get
        if(voteData.operationType == OperationType.register){
          voteWitness(track, tx, witness)
        }
        else {
          cancelCounterFromWitness(track, tx, witness)
        }
      }
      this
    }

    private def voteWitness(track: DataBase, tx: Transaction, witness: WitnessInfo): Unit ={
      track.addBalance(tx.from, -voteData.voterCount)
      witness.updateVoteCounts(voteData.voterCount)
      track.createWitness(witness.addr, witness)
      track.getVote(tx.from).fold(track.createVote(tx.from, Vote(tx.from, voteData.candidate, voteData.voterCount)))(
        vote =>{
        vote.updateCounts(voteData.voterCount)
      })
    }

    private def cancelCounterFromWitness(track: DataBase, tx: Transaction, witness: WitnessInfo): Unit ={
      track.addBalance(tx.from, voteData.voterCount)
      witness.updateVoteCounts(-voteData.voterCount)
      track.createWitness(witness.addr, witness)
      track.getVote(tx.from).fold()(
        vote =>{
          vote.updateCounts(-voteData.voterCount)
        })
    }

    def returnResult(): (Boolean, Array[Byte]) ={
      VoteOperationChecker.returnResult()
    }

  }
}

object VoteOperationChecker{

  var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

  def errorDetected(f: => Unit): Unit ={
    if(result._1) f
    else f
  }

  def setResult(flag: Boolean): Unit ={
    result =  (flag, new Array[Byte](0))
  }

  def returnResult():(Boolean, Array[Byte]) ={
    result
  }
}
