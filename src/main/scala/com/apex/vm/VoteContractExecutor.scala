package com.apex.vm

import com.apex.consensus.{Vote, VoteData, WitnessInfo}
import com.apex.core.{DataBase, OperationType, Transaction, TransactionType}
import com.apex.crypto.{FixedNumber, UInt160}

object VoteContractExecutor {

  import OperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction, timeStamp: Long): (Boolean, Array[Byte]) ={
    val voteData = VoteData.fromBytes(data)
    voteData.isVoterRequestValid()
      .isAccountBalanceEnough(track, tx)
      .isVoteWitnessExist(track)
      .isVoteWitnessExistAndRegistered(track)
      .isCancelWitnessExistInVoterTargetMap(track, tx)
      .isVoterExist(track, tx)
      .voterCancelWitnessCounterInvalid(track, tx)
      .processReq(track, tx, timeStamp)
      .returnResult()
  }

  implicit class VoteContractContext(voteData:VoteData){
    var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

    //when vote a witness, the counter must be larger than 0
    def isVoterRequestValid(): VoteContractContext ={
      errorDetected{
        if(voteData.voterCount.value <= 0){
          setResult(false, ("the counter you vote a witness must be larger than 0").getBytes)
        }
      }
      this
    }

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
        if(witness.isEmpty && voteData.operationType == OperationType.register){
          setResult(false, ("vote target must be in witness list").getBytes)
        }
      }
      this
    }

    //check voter target exists in witness db and register field is true
    def isVoteWitnessExistAndRegistered(track: DataBase): VoteContractContext = {
      errorDetected{
        val witness = track.getWitness(voteData.candidate)
        if(witness.isDefined && voteData.operationType == OperationType.register && !witness.get.register){
          setResult(false, ("voter target exists in witness db but is unregistered so can not be vote").getBytes)
        }
      }
      this
    }

    //when a voter cancel vote to a witness,check it exist in vote target map (UInt160, FixedNumber)
    def isCancelWitnessExistInVoterTargetMap(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        if(voteData.operationType == OperationType.resisterCancel){
          val vote = track.getVote(tx.from)
          if(vote.isDefined){
            if(vote.get.targetMap.get(voteData.candidate).isEmpty)
            setResult(false, ("voter can not cancel a vote if it not exist in vote target map").getBytes)
          }
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

    //when a voter cancel a witness, it request counter cannot be larger than its counter in vote target map value
    def voterCancelWitnessCounterInvalid(track: DataBase, tx: Transaction): VoteContractContext = {
      errorDetected{
        if(voteData.operationType == OperationType.resisterCancel){
          val vote = track.getVote(tx.from)
          if(vote.isDefined){
            if(vote.get.targetMap.getOrElse(voteData.candidate, FixedNumber.Zero).value < voteData.voterCount.value)
              setResult(false, ("cancel request counter bigger than its counter in target map is forbidden").getBytes)
          }
        }
      }
      this
    }

    def processReq(track: DataBase, tx: Transaction, timeStamp: Long): VoteContractContext ={
      errorDetected{
        val witness = track.getWitness(voteData.candidate)
        if(voteData.operationType == OperationType.register && witness.isDefined){
          voteWitness(track, tx, witness.get)
        }
        else {
          cancelCounterFromWitness(track, tx, witness, timeStamp)
        }
      }
      this
    }

    private def voteWitness(track: DataBase, tx: Transaction, witness: WitnessInfo): Unit ={
      track.addBalance(tx.from, -voteData.voterCount)
      track.addBalance(new UInt160(PrecompiledContracts.voteAddr.getLast20Bytes), voteData.voterCount)
      val newWitness = witness.copy(voteCounts = witness.voteCounts + voteData.voterCount)
      track.createWitness(newWitness)
      track.getVote(tx.from).fold(track.createVote(tx.from, Vote(tx.from,
        scala.collection.mutable.Map[UInt160, FixedNumber](voteData.candidate -> voteData.voterCount))))(
        vote =>{
          val newVote = vote.updateTargetCounter(voteData.candidate, voteData.voterCount)
          track.createVote(vote.voter, newVote)
      })
    }

    private def cancelCounterFromWitness(track: DataBase, tx: Transaction, witness: Option[WitnessInfo],timeStamp: Long): Unit ={
      //TODO read from settings
      val time = timeStamp + 24 * 60 * 60 * 1000/*750*/
      //note: this scheduleTx from and to address are opposite to tx; amount is the register spend;
      // the tx hash exists in the data filed of scheduleTx
      val scheduleTx = new Transaction(TransactionType.Refund, tx.toPubKeyHash, tx.from, voteData.voterCount, tx.nonce, tx.id.data,
        tx.gasPrice, tx.gasLimit, tx.signature, tx.version, time)
      track.setScheduleTx(scheduleTx.id, scheduleTx)
      if(witness.isDefined){
        val newWitness = witness.get.copy(voteCounts = witness.get.voteCounts - voteData.voterCount)
        track.createWitness(newWitness)
      }
      track.getVote(tx.from).fold({})(
        vote =>{
          val newVote = vote.updateTargetCounter(voteData.candidate, -voteData.voterCount)
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
