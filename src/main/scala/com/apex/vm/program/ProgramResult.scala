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

import com.apex.vm.program.trace.LogInfo

import scala.collection.mutable.ListBuffer

class ProgramResult {
  private var gasUsed = 0L
  private var hReturn = Array.empty[Byte]
  private var exception: RuntimeException = _
  private var revert = false
  private var futureRefund = 0L
  private var logInfoList: ListBuffer[LogInfo] = _

  def spendGas(gas: Long): Unit = {
    gasUsed += gas
  }

  def setRevert(): Unit = {
    this.revert = true
  }

  def isRevert: Boolean = revert

  def refundGas(gas: Long): Unit = {
    gasUsed -= gas
  }

  def setHReturn(hReturn: Array[Byte]): Unit = {
    this.hReturn = hReturn
  }

  def addFutureRefund(gasValue: Long): Unit = {
    futureRefund += gasValue
  }

  def getFutureRefund: Long = {
    futureRefund
  }

  def resetFutureRefund(): Unit = {
    futureRefund = 0
  }

  def getHReturn: Array[Byte] = hReturn

  def getException: RuntimeException = exception

  def getGasUsed: Long = gasUsed

  def setException(exception: RuntimeException): Unit = {
    this.exception = exception
  }

  def getLogInfoList: ListBuffer[LogInfo] = {
    if (logInfoList == null) {
      logInfoList = ListBuffer.empty[LogInfo]
    }
    logInfoList
  }

  def addLogInfo(logInfo: LogInfo): Unit = {
    getLogInfoList.append(logInfo)
  }

  def addLogInfos(logInfos: List[LogInfo]): Unit = {
    if (!logInfos.isEmpty) getLogInfoList.appendAll(logInfos)
  }

  def addTouchAccount(addr: Array[Byte]): Unit = {
    throw new NotImplementedError
  }

}
