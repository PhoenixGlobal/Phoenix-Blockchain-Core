package com.apex.vm

import com.apex.consensus.{RegisterData, WitnessInfo}
import com.apex.core.{DataBase, OperationType, Transaction, TransactionType}
import com.apex.crypto.{FixedNumber, UInt160}
import com.apex.settings.Witness

object RegisterContractExecutor {
  import OperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction, registerSpend: FixedNumber, timeStamp: Long): (Boolean, Array[Byte]) ={
    val registerData = RegisterData.fromBytes(data)
    registerData.isValid(track, tx)
            .isNotGenesisWitness()
            .isAccountBalanceEnough(track, registerSpend)
            .isRegisterWitnessExist(track)
            .isUnRegisterWitnessExistInDb(track)
            .isCancelWitnessNotExist(track)
            .isCancelWitnessGenesis(track)
            .processReq(track, registerSpend, tx, timeStamp)
            .returnResult()
  }

  implicit class RegisterContractContext(registerData: RegisterData) {

    var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

    //check the register account address is equal to transaction sender
    def isValid(track: DataBase, tx: Transaction): RegisterContractContext = {
      errorDetected {
        if (!(tx.from == registerData.registerAccount && registerData.registerAccount == registerData.registerInfo.addr)){
          setResult(false, ("register address must be same as transaction from address").getBytes)
        }
      }
      this
    }

    // Not allowed to register as genesis witness
    def isNotGenesisWitness(): RegisterContractContext = {
      errorDetected {
        if (registerData.registerInfo.isGenesisWitness) {
          setResult(false, ("register must NOT be genesis witness").getBytes)
        }
      }
      this
    }

    //check the account balance is enough to register a witness
    def isAccountBalanceEnough(track: DataBase, registerSpend: FixedNumber): RegisterContractContext ={
      errorDetected {
        val account = track.getAccount(registerData.registerAccount).get
        if (registerData.operationType == OperationType.register &&
          (account.balance.value < registerSpend.value)) {
          setResult(false, ("register account balance is not enough to register a producer").getBytes)
        }
      }
      this
    }

    //if a witness exists in witness db, it cannot be registered later.
    def isRegisterWitnessExist(track: DataBase): RegisterContractContext = {
      errorDetected {
        val witness = track.getWitness(registerData.registerAccount)
        if (witness.isDefined && registerData.operationType == OperationType.register && witness.get.register) {
          setResult(false, ("register witness has already be registered").getBytes)
        }
      }
      this
    }

    //if a witness exists in witness db, it cannot be registered later.
    def isUnRegisterWitnessExistInDb(track: DataBase): RegisterContractContext = {
      errorDetected {
        val witness = track.getWitness(registerData.registerAccount)
        if (witness.isDefined && registerData.operationType == OperationType.resisterCancel && !witness.get.register) {
          setResult(false, ("the witness you unregister has already be unregistered in db").getBytes)
        }
      }
      this
    }

    //if a witness does not exist in witness db, it cannot be cancel register.
    def isCancelWitnessNotExist(track: DataBase): RegisterContractContext = {
      errorDetected {
        val witness = track.getWitness(registerData.registerAccount)
        if (witness.isEmpty && registerData.operationType ==OperationType.resisterCancel) {
          setResult(false, ("a witness not be registered before is not allowed to cancel").getBytes)
        }
      }
      this
    }

    //if witness is in init configuration 21 witness lists, it cannot be cancelled.
    def isCancelWitnessGenesis(track: DataBase): RegisterContractContext = {
      errorDetected {
        val witness = track.getWitness(registerData.registerAccount)
        if (witness.isDefined) {
          if (witness.get.isGenesisWitness) {
            setResult(false, ("a genesis witness is not allowed to cancel").getBytes)
          }
        }
      }
      this
    }

    def processReq(track: DataBase, registerSpend: FixedNumber, tx: Transaction, timeStamp: Long): RegisterContractContext = {
      errorDetected {
        if (registerData.operationType == OperationType.register) {
          registerWitness(track, registerSpend)
        }
        else {
          cancelRegisterWitness(track, registerSpend, tx, timeStamp)
        }
      }
      this
    }

    private def cancelRegisterWitness(track: DataBase, registerSpend: FixedNumber, tx: Transaction, timeStamp: Long) = {
      val time = timeStamp + 24 * 60 * 60 * 1000/*750/*60 * 1000 * 2*/*/
      //note: this scheduleTx from and to address are opposite to tx; amount is the register spend;
      // the tx hash exists in the data filed of scheduleTx
      val scheduleTx = new Transaction(TransactionType.Refund, tx.toPubKeyHash, tx.from,
        FixedNumber(registerSpend.value), tx.nonce, tx.id.data, tx.gasPrice, tx.gasLimit, tx.signature, tx.version, time)
      track.setScheduleTx(scheduleTx.id, scheduleTx)
      val updatedWitness = track.getWitness(registerData.registerInfo.addr).get.copy(register = false)
      track.setWitness(updatedWitness)
      //RegisterFeeScheduleActor(track, registerData, registerSpend)
      //track.deleteWitness(registerData.registerAccount)
    }

    private def registerWitness(track: DataBase, registerSpend: FixedNumber) = {
      track.addBalance(registerData.registerAccount, -registerSpend.value)
      track.addBalance(new UInt160(PrecompiledContracts.registerNodeAddr.getLast20Bytes), registerSpend.value)
      val oldInfo = track.getWitness(registerData.registerInfo.addr)
      if (oldInfo.isDefined) {
        val witnessInfo = registerData.registerInfo.copy(voteCounts = oldInfo.get.voteCounts, register = true)
        track.setWitness(witnessInfo)
      }
      else {
        val witnessInfo = registerData.registerInfo.copy(register = true)
        track.setWitness(witnessInfo)
      }
    }


    def returnResult(): (Boolean, Array[Byte]) = {
      result = OperationChecker.returnResult()
      OperationChecker.setResultToInit()
      result
    }

  }
}

object OperationChecker {

  var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

  def errorDetected(f: => Unit): Unit = {
    if(result._1) f
  }

  def setResult(flag: Boolean, description: Array[Byte] = new Array[Byte](0)): Unit = {
    result =  (flag, description)
  }

  def returnResult():(Boolean, Array[Byte]) = {
    result
  }

  def setResultToInit(){
    result = (true, new Array[Byte](0))
  }
}
