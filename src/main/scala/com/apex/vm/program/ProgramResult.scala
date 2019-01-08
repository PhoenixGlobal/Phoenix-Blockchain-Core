/*
 * Copyright (c) [2016] [ <ether.camp> ]
 * This file is part of the ethereumJ library.
 *
 * The ethereumJ library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The ethereumJ library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the ethereumJ library. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ProgramResult.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午2:03@version: 1.0
 *
 */

package com.apex.vm.program

import com.apex.crypto.UInt160
import com.apex.vm.CallCreate
import com.apex.vm.program.trace.LogInfo

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Set}

class ProgramResult {
  private var gasUsed = BigInt(0)
  private var hReturn = Array.empty[Byte]
  private var exception: RuntimeException = _
  private var revert = false
  private var futureRefund = BigInt(0)
  private val logInfoList = ListBuffer.empty[LogInfo]
  private val touchedAccounts = Set.empty[UInt160]
  private val deleteAccounts = Set.empty[UInt160]
  private val callCreateList = ListBuffer.empty[CallCreate]

  def spendGas(gas: BigInt): Unit = {
    gasUsed += gas
  }

  def setRevert(): Unit = {
    this.revert = true
  }

  def isRevert: Boolean = revert

  def refundGas(gas: BigInt): Unit = {
    gasUsed -= gas
  }

  def setHReturn(hReturn: Array[Byte]): Unit = {
    this.hReturn = hReturn
  }

  def addFutureRefund(gasValue: BigInt): Unit = {
    futureRefund += gasValue
  }

  def getFutureRefund: BigInt = {
    futureRefund
  }

  def resetFutureRefund(): Unit = {
    futureRefund = 0
  }

  def getHReturn: Array[Byte] = hReturn

  def getException: RuntimeException = exception

  def getGasUsed: BigInt = gasUsed

  def setException(exception: RuntimeException): Unit = {
    this.exception = exception
  }

  def getLogInfoList = {
    logInfoList
  }

  def addLogInfo(logInfo: LogInfo): Unit = {
    logInfoList.append(logInfo)
  }

  def addLogInfos(logInfos: ListBuffer[LogInfo]): Unit = {
    logInfoList.appendAll(logInfos)
  }

  def addCallCreate(data: Array[Byte], destination: Array[Byte], gasLimit: Array[Byte], value: Array[Byte]): Unit = {
    callCreateList.append(CallCreate(data, destination, gasLimit, value))
  }

  def getTouchedAccounts = {
    touchedAccounts
  }

  def addTouchAccount(addr: UInt160): Unit = {
    touchedAccounts.add(addr)
  }

  def addTouchAccounts(accounts: mutable.Set[UInt160]): Unit = {
    if (!accounts.isEmpty) {
      accounts.foreach(touchedAccounts.add)
    }
  }

  def getDeleteAccounts = {
    deleteAccounts
  }

  def addDeleteAccount(addr: UInt160): Unit = {
    deleteAccounts.add(addr)
  }

  def addDeleteAccounts(accounts: mutable.Set[UInt160]): Unit = {
    if (!accounts.isEmpty) {
      accounts.foreach(deleteAccounts.add)
    }
  }

  def merge(another: ProgramResult): Unit = {
    if (another.getException == null && !another.isRevert) {
      addDeleteAccounts(another.getDeleteAccounts)
      addLogInfos(another.getLogInfoList)
      addFutureRefund(another.getFutureRefund)
      addTouchAccounts(another.getTouchedAccounts)
    }
  }

}
