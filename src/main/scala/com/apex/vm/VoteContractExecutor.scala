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

    //check account balance is enough to vote a witness
    def isAccountBalanceEnough( track: DataBase, tx: Transaction): VoteContractContext ={
      errorDetected{
        val account = track.getAccount(tx.from).get
        if(voteData.operationType == OperationType.register &&
          (account.balance.value < voteData.voterCount.value)){
          setResult(false, ("account balance is not enough").getBytes)
        }
      }
      this
    }

    //check voter target exists in witness db or not
    def isVoteWitnessExist(track: DataBase): VoteContractContext = {
      errorDetected{
        val witness = track.getWitness(voteData.candidate)
        if(witness.isEmpty){
          setResult(false, ("vote target must be in witness list").getBytes)
        }
      }
      this
    }

    //when a voter cancel vote to a witness,check it exist in vote db or not
    def isVoterExist(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        if(voteData.operationType == OperationType.resisterCancel && track.getVote(tx.from).isEmpty){
          setResult(false, ("voter can not cancel a vote if it not exist in votes list").getBytes)
        }
      }
      this
    }

    //when a voter vote to a witness a, it cannot vote witness b later
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

    //when a voter cancel a witness, it request counter cannot be larger than its counter in vote db
    def voterCancelWitnessCounterInvalid(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        val vote = track.getVote(tx.from).getOrElse(new Vote(tx.from, voteData.candidate, FixedNumber.Zero))
        if(voteData.operationType == OperationType.resisterCancel && voteData.voterCount.value > vote.count.value) {
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
      val newWitness = witness.copy(voteCounts = witness.voteCounts + voteData.voterCount)
      track.createWitness(newWitness)
      track.getVote(tx.from).fold(track.createVote(tx.from, Vote(tx.from, voteData.candidate, voteData.voterCount)))(
        vote =>{
          val newVote = vote.updateCounts(voteData.voterCount)
          track.createVote(vote.voter, newVote)
      })
    }

    private def cancelCounterFromWitness(track: DataBase, tx: Transaction, witness: WitnessInfo): Unit ={
      track.addBalance(tx.from, voteData.voterCount)
      val newWitness = witness.copy(voteCounts = witness.voteCounts - voteData.voterCount)
      track.createWitness(newWitness)
      track.getVote(tx.from).fold()(
        vote =>{
          val newVote = vote.updateCounts(-voteData.voterCount)
          track.createVote(vote.voter, newVote)
        })
    }

    def returnResult(): (Boolean, Array[Byte]) ={
      result = OperationChecker.returnResult()
      OperationChecker.setResultToInit()
      result
    }

  }
}
