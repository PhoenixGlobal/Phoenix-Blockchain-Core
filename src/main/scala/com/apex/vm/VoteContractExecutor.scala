package com.apex.vm

import com.apex.consensus.{Vote, VoteData, WitnessInfo}
import com.apex.core.{DataBase, OperationType, Transaction}
import com.apex.crypto.FixedNumber
import com.apex.vm.OperationChecker.result

object VoteContractExecutor {

  import OperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction): (Boolean, Array[Byte]) ={
    val voteData = VoteData.fromBytes(data)
    voteData.isAccountBalanceEnough(track, tx)
      .isVoteWitnessExist(track)
      .isVoterExist(track, tx)
      .voterWitnessChanged(track, tx)
      .voterCancelWitnessCounterInvalid(track, tx)
      .processReq(track, tx)
      .returnResult()
  }

  implicit class VoteContractContext(voteData:VoteData){
    var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

    def isAccountBalanceEnough( track: DataBase, tx: Transaction): VoteContractContext ={
      errorDetected{
        val account = track.getAccount(tx.from).get
        if(voteData.operationType == OperationType.register &&
          (!account.balance.> (voteData.voterCount))){
          setResult(false, ("account balance is not enough").getBytes)
        }
      }
      this
    }

    def isVoteWitnessExist(track: DataBase): VoteContractContext = {
      errorDetected{
        val witness = track.getWitness(voteData.candidate)
        if(witness.isEmpty){
          setResult(false, ("vote target must be in witness list").getBytes)
        }
      }
      this
    }

    def isVoterExist(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        if(voteData.operationType == OperationType.resisterCancel && track.getVote(tx.from).isEmpty){
          setResult(false, ("voter can not cancel a vote if it not exist in votes list").getBytes)
        }
      }
      this
    }

    def voterWitnessChanged(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        track.getVote(tx.from).fold()(vote => {
          if(vote.target != voteData.candidate){
            setResult(false, ("voter can not change a target").getBytes())
          }
        })
      }
      this
    }

    def voterCancelWitnessCounterInvalid(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        val vote = track.getVote(tx.from).getOrElse(new Vote(tx.from, voteData.candidate, FixedNumber.Zero))
        if(voteData.operationType == OperationType.resisterCancel && voteData.voterCount.>(vote.count)) {
          setResult(false, ("voter cancel a witness but request counter bigger than counter in db").getBytes)
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
          track.createVote(vote.voter, vote)
      })
    }

    private def cancelCounterFromWitness(track: DataBase, tx: Transaction, witness: WitnessInfo): Unit ={
      track.addBalance(tx.from, voteData.voterCount)
      witness.updateVoteCounts(-voteData.voterCount)
      track.createWitness(witness.addr, witness)
      track.getVote(tx.from).fold()(
        vote =>{
          vote.updateCounts(-voteData.voterCount)
          track.createVote(vote.voter, vote)
        })
    }

    def returnResult(): (Boolean, Array[Byte]) ={
      result = OperationChecker.returnResult()
      OperationChecker.setResultToInit()
      result
    }

  }
}

//object VoteOperationChecker{
//
//  var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))
//
//  def errorDetected(f: => Unit): Unit ={
//    if(result._1) f
//  }
//
//  def setResult(flag: Boolean, description: Array[Byte] = new Array[Byte](0)): Unit ={
//    result  =  (flag, description)
//  }
//
//  def returnResult():(Boolean, Array[Byte]) ={
//    result
//  }
//
//  def setResultToInit(){
//    result = (true, new Array[Byte](0))
//  }
//}
