package com.apex.core

import com.apex.crypto.UInt160
import com.apex.vm.program.ProgramResult
import com.apex.vm.program.trace.LogInfo

import scala.collection.mutable.{ListBuffer, Set}

class TransactionExecutionSummary(var tx: Transaction) {

  var value: BigInt = tx.amount.value
  var gasPrice: BigInt = tx.gasPrice.value
  var gasLimit: BigInt = tx.gasLimit
  var gasUsed: BigInt = 0
  var gasLeftover: BigInt = 0
  var gasRefund: BigInt = 0

  //deletedAccounts
  //internalTransactions
  //storageDiff
  //touchedStorage

  var result = Array.empty[Byte]
  var logs = ListBuffer.empty[LogInfo]
  var failed = false
  var parsed = false

  def getFee(): BigInt = calcCost(gasLimit - gasLeftover - gasRefund)
  def getLeftover(): BigInt = calcCost(gasLeftover)
  def getRefund(): BigInt = calcCost(gasRefund)

  private def calcCost(gas: BigInt): BigInt = {
    gasPrice * gas
  }
}

object TransactionExecutionSummary {

  class Builder(val tx: Transaction) {

    val summary = new TransactionExecutionSummary(tx)

    def build(): TransactionExecutionSummary = {

      summary.parsed = true

      // TODO reject internalTransactions

      summary
    }

    def gasUsed(gasUsed: BigInt) = summary.gasUsed = gasUsed
    def gasLeftover(gasLeftover: BigInt) = summary.gasLeftover = gasLeftover
    def gasRefund(gasRefund: BigInt) = summary.gasRefund = gasRefund
    def markAsFailed() = summary.failed = true
    def logs(logs: ListBuffer[LogInfo]) = summary.logs = logs
    def result(result: Array[Byte]) = summary.result = result

    def deletedAccounts(deletedAccounts: Set[UInt160]) = {

    }

    def internalTransactions() = {

    }
  }

}
