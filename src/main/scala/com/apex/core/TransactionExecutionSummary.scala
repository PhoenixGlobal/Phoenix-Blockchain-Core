package com.apex.core

import com.apex.crypto.UInt160
import com.apex.vm.program.ProgramResult
import com.apex.vm.program.trace.LogInfo

import scala.collection.mutable.{ListBuffer, Set}

class TransactionExecutionSummary(var tx: Transaction) {

  var value: BigInt = 0
  var gasPrice: BigInt = 0
  var gasLimit: BigInt = 0
  var gasUsed: BigInt = 0
  var gasLeftover: BigInt = 0
  var gasRefund: BigInt = 0

  var result = Array.empty[Byte]
  var logs = ListBuffer.empty[LogInfo]
  var failed = false
  var parsed = false


}

object TransactionExecutionSummary {

  class Builder(val tx: Transaction) {

    val summary = new TransactionExecutionSummary(tx)

    def build(): TransactionExecutionSummary = {

      summary.parsed = true

      // TODO

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
