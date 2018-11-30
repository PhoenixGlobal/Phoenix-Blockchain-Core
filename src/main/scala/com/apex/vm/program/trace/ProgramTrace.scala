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
 * FileName: ProgramTrace.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午2:55@version: 1.0
 *
 */

package com.apex.vm.program.trace

import com.apex.vm.DataWord
import com.apex.vm.program.invoke.ProgramInvoke
import org.apex.vm.OpCode

import scala.collection.mutable.ListBuffer

case class LogInfo(address: Array[Byte] = Array.empty, topics: ListBuffer[DataWord] = ListBuffer.empty, data: Array[Byte] = Array.empty)

class ProgramTrace(programInvoke: ProgramInvoke = null) {
  private var _ops = new ListBuffer[Op]
  private var _result: String = _
  private var _error: String = _
  private var _contractAddress: String = _

  import com.apex.vm._

  if (programInvoke != null) {
    _contractAddress = programInvoke.getOwnerAddress.getLast20Bytes.toHex
  }

  def ops = _ops

  def ops_=(value: ListBuffer[Op]): Unit = {
    _ops = value
  }

  def result = _result

  def result_=(value: String): Unit = {
    _result = value
  }

  def error_=(value: String): Unit = {
    _error = value
  }

  def contractAddress_=(value: String): Unit = {
    _contractAddress = value
  }

  def result(ret: Array[Byte]): ProgramTrace = {
    result = ret.toHex
    this
  }

  def error(err: Exception): ProgramTrace = {
    _error = if (err == null) "" else s"${err.getClass}: ${err.getMessage}"
    this
  }

  def addOp(code: Byte, pc: Int, deep: Int, gas: BigInt, actions: OpActions): Op = {
    val op = Op(OpCode(code), pc, deep, gas, actions)
    ops.append(op)
    op
  }

  /**
    * Used for merging sub calls execution.
    */
  def merge(programTrace: ProgramTrace): Unit = {
    this.ops.appendAll(programTrace.ops)
  }
}
