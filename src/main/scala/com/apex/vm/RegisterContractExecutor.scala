package com.apex.vm

import com.apex.consensus.RegisterData
import com.apex.core.{DataBase, OperationType, Transaction}
import com.apex.crypto.FixedNumber
import com.apex.crypto.FixedNumber.One

object RegisterContractExecutor {
  def execute(data: Array[Byte], track: DataBase, tx: Transaction): Unit ={
  }

  implicit class RegisterContractContext(track: DataBase){

    def isAccountExist(data: Array[Byte]): Boolean = {
      val registerTransaction = RegisterData.fromBytes(data)
      if(track.getAccount(registerTransaction.registerAccount).isDefined)
        return true
      false
    }

    def isAccountBalanceEnough(data: Array[Byte]): Boolean ={
      val registerTransaction = RegisterData.fromBytes(data)
      val account = track.getAccount(registerTransaction.registerAccount).get
      if(registerTransaction.operationType == OperationType.register &&
        (!account.balance.>(FixedNumber(One.value * 1000)))) return false
      true
    }

    def isRegisterWitnessExist(data: Array[Byte]): Boolean = {
      val registerTransaction = RegisterData.fromBytes(data)
      val witness = track.getWitness(registerTransaction.registerAccount)
      if(witness.isDefined && registerTransaction.operationType == OperationType.register){
        return false
      }
      if(witness.isEmpty && registerTransaction.operationType ==OperationType.resisterCancel){
        return false
      }
      true
    }

    def isCancelWitnessNotExist(data: Array[Byte]): Boolean = {
      val registerTransaction = RegisterData.fromBytes(data)
      val witness = track.getWitness(registerTransaction.registerAccount)
      if(witness.isEmpty && registerTransaction.operationType ==OperationType.resisterCancel){
        return false
      }
      true
    }

    def processReq(data: Array[Byte], track: DataBase, tx: Transaction): Boolean ={
      val registerTransaction = RegisterData.fromBytes(data)
      if(registerTransaction.operationType == OperationType.register){
        track.addBalance(registerTransaction.registerAccount, FixedNumber(-(One.value * 1000)))
        track.createWitness(registerTransaction.registerAccount, registerTransaction.registerInfo)
        true
      }
      else {
        track.addBalance(registerTransaction.registerAccount, FixedNumber(One.value * 1000))
        track.deleteWitness(registerTransaction.registerAccount)
        true
      }
    }

  }
}

object OperationResult{
  def errorDetected()(f: => Boolean): Unit ={
    if(f) return
  }
}
