package com.apex.vm

import com.apex.consensus.RegisterData
import com.apex.core.{DataBase, OperationType, Transaction}
import com.apex.crypto.FixedNumber
import com.apex.crypto.FixedNumber.One

object RegisterContractExecutor {
  import OperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction): (Boolean, Array[Byte]) ={
    val registerData = RegisterData.fromBytes(data)
    registerData.isValid(track, tx)
            .isAccountExist(track)
            .isAccountBalanceEnough(track)
            .isRegisterWitnessExist(track)
            .isCancelWitnessNotExist(track)
            .processReq(track)
            .returnResult()
  }

  implicit class RegisterContractContext(registerData: RegisterData) {

    def isValid(track: DataBase, tx: Transaction): RegisterContractContext = {
      errorDetected{
        if(tx.from != registerData.registerAccount){
          setResult(false)
        }
      }
      this
    }

    def isAccountExist(track: DataBase): RegisterContractContext = {
      errorDetected{
        if(track.getAccount(registerData.registerAccount).isEmpty){
          setResult(false)
        }
      }
      this
    }

    def isAccountBalanceEnough(track: DataBase): RegisterContractContext ={
      errorDetected{
        val account = track.getAccount(registerData.registerAccount).get
        if(registerData.operationType == OperationType.register &&
          (!account.balance.>(FixedNumber(One.value * 1000)))){
          setResult(false)
        }
      }
      this
    }

    def isRegisterWitnessExist(track: DataBase): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerData.registerAccount)
        if(witness.isDefined && registerData.operationType == OperationType.register){
          setResult(false)
        }
      }
      this
    }

    def isCancelWitnessNotExist(track: DataBase): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerData.registerAccount)
        if(witness.isEmpty && registerData.operationType ==OperationType.resisterCancel){
          setResult(false)
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
      track.addBalance(registerData.registerAccount, FixedNumber(One.value * 1000))
      track.deleteWitness(registerData.registerAccount)
    }

    private def registerWitness(track: DataBase) = {
      track.addBalance(registerData.registerAccount, FixedNumber(-(One.value * 1000)))
      track.createWitness(registerData.registerAccount, registerData.registerInfo)
    }

    def returnResult(): (Boolean, Array[Byte]) ={
      OperationChecker.returnResult()
    }

  }
}

object OperationChecker{

  var result: (Boolean, Array[Byte]) = (true, new Array[Byte](0))

  def errorDetected(f: => Unit): Unit ={
    if(result._1) f
  }

  def setResult(flag: Boolean): Unit ={
    result =  (flag, new Array[Byte](0))
  }

  def returnResult():(Boolean, Array[Byte]) ={
    result
  }
}
