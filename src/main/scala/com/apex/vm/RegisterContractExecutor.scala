package com.apex.vm

import com.apex.consensus.RegisterData
import com.apex.core.{DataBase, OperationType, Transaction}
import com.apex.crypto.FixedNumber
import com.apex.crypto.FixedNumber.One

object RegisterContractExecutor {
  import OperationChecker._

  def execute(data: Array[Byte], track: DataBase, tx: Transaction): (Boolean, Array[Byte]) ={
    val registerTransaction = RegisterData.fromBytes(data)
    track.isValid(registerTransaction, tx)
            .isAccountExist(registerTransaction)
            .isAccountBalanceEnough(registerTransaction)
            .isRegisterWitnessExist(registerTransaction)
            .isCancelWitnessNotExist(registerTransaction)
            .processReq(registerTransaction)
            .returnResult()
  }

  implicit class RegisterContractContext(track: DataBase) {

    def isValid(registerTransaction: RegisterData, tx: Transaction): RegisterContractContext = {
      errorDetected{
        if(tx.from != registerTransaction.registerAccount){
          setResult(false)
        }
      }
      this
    }

    def isAccountExist(registerTransaction: RegisterData): RegisterContractContext = {
      errorDetected{
        if(track.getAccount(registerTransaction.registerAccount).isEmpty){
          setResult(false)
        }
      }
      this
    }

    def isAccountBalanceEnough(registerTransaction: RegisterData): RegisterContractContext ={
      errorDetected{
        val account = track.getAccount(registerTransaction.registerAccount).get
        if(registerTransaction.operationType == OperationType.register &&
          (!account.balance.>(FixedNumber(One.value * 1000)))){
          setResult(false)
        }
      }
      this
    }

    def isRegisterWitnessExist(registerTransaction: RegisterData): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerTransaction.registerAccount)
        if(witness.isDefined && registerTransaction.operationType == OperationType.register){
          setResult(false)
        }
      }
      this
    }

    def isCancelWitnessNotExist(registerTransaction: RegisterData): RegisterContractContext = {
      errorDetected{
        val witness = track.getWitness(registerTransaction.registerAccount)
        if(witness.isEmpty && registerTransaction.operationType ==OperationType.resisterCancel){
          setResult(false)
        }
      }
      this
    }

    def processReq(registerTransaction: RegisterData): RegisterContractContext ={
      if(registerTransaction.operationType == OperationType.register){
        track.addBalance(registerTransaction.registerAccount, FixedNumber(-(One.value * 1000)))
        track.createWitness(registerTransaction.registerAccount, registerTransaction.registerInfo)
      }
      else {
        track.addBalance(registerTransaction.registerAccount, FixedNumber(One.value * 1000))
        track.deleteWitness(registerTransaction.registerAccount)
      }
      this
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
    else f
  }

  def setResult(flag: Boolean): Unit ={
    result =  (flag, new Array[Byte](0))
  }

  def returnResult():(Boolean, Array[Byte]) ={
    result
  }
}
