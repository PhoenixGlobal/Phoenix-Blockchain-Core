package com.apex.vm

import com.apex.consensus.RegisterData
import com.apex.core.{DataBase, OperationType, Transaction}
import com.apex.crypto.{BinaryData, FixedNumber}
import com.apex.crypto.FixedNumber.One

object RegisterContractExecutor {
  import OperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction): (Boolean, Array[Byte]) ={
    val registerData = RegisterData.fromBytes(data)
    registerData.isValid(track, tx)
            .isAccountBalanceEnough(track)
            .isRegisterWitnessExist(track)
            .isCancelWitnessNotExist(track)
            .processReq(track)
            .returnResult()
  }

  implicit class RegisterContractContext(registerData: RegisterData) {

    var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

    def isValid(track: DataBase, tx: Transaction): RegisterContractContext = {
      errorDetected{
        if(!(tx.from == registerData.registerAccount && registerData.registerAccount == registerData.registerInfo.addr)){
          setResult(false, ("register address must be same as transaction from address").getBytes)
        }
      }
      this
    }

    def isAccountBalanceEnough(track: DataBase): RegisterContractContext ={
      errorDetected{
        val account = track.getAccount(registerData.registerAccount).get
        if(registerData.operationType == OperationType.register &&
          (!account.balance.>(FixedNumber(One.value)))){
          setResult(false, ("register account balance is not enough to register a producer").getBytes)
        }
      }
      this
    }

    def isRegisterWitnessExist(track: DataBase): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerData.registerAccount)
        if(witness.isDefined && registerData.operationType == OperationType.register){
          setResult(false, ("register witness has already exist").getBytes)
        }
      }
      this
    }

    def isCancelWitnessNotExist(track: DataBase): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerData.registerAccount)
        if(witness.isEmpty && registerData.operationType ==OperationType.resisterCancel){
          setResult(false, ("a witness not be registered before is not allowed to cancel").getBytes)
        }
      }
      this
    }

    def isCancelWitnessGenesis(track: DataBase): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerData.registerAccount)
        if(witness.get.isGenesisWitness){
          setResult(false, ("a genesis witness is not allowed to cancel").getBytes)
        }
      }
      this
    }

    def processReq(track: DataBase): RegisterContractContext ={
      errorDetected{
        if(registerData.operationType == OperationType.register){
          registerWitness(track)
        }
        else {
          cancelRegisterWitness(track)
        }
      }
      this
    }

    private def cancelRegisterWitness(track: DataBase) = {
      track.addBalance(registerData.registerAccount, FixedNumber(One.value))
      track.deleteWitness(registerData.registerAccount)
    }

    private def registerWitness(track: DataBase) = {
      track.addBalance(registerData.registerAccount, FixedNumber(-(One.value)))
      track.createWitness(registerData.registerInfo)
    }

    def returnResult(): (Boolean, Array[Byte]) ={
      result = OperationChecker.returnResult()
      OperationChecker.setResultToInit()
      result
    }

  }
}

object OperationChecker{

  var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

  def errorDetected(f: => Unit): Unit ={
    if(result._1) f
  }

  def setResult(flag: Boolean, description: Array[Byte] = new Array[Byte](0)): Unit ={
    result =  (flag, description)
  }

  def returnResult():(Boolean, Array[Byte]) ={
    result
  }

  def setResultToInit(){
    result = (true, new Array[Byte](0))
  }
}
