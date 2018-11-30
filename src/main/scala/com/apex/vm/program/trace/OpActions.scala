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
 * FileName: OpAction.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午2:09@version: 1.0
 *
 */

package com.apex.vm.program.trace

import com.apex.vm.DataWord

import scala.collection.mutable.ListBuffer

object Name extends Enumeration {
  val pop = Value
  val push = Value
  val swap = Value
  val extend = Value
  val write = Value
  val put = Value
  val remove = Value
  val clear = Value
}

class Action(val name: Name.Value) {
  private val params = collection.mutable.Map.empty[String, AnyRef]

  def getParams = params.toMap

  private[trace] def addParam(name: String, value: Any) = {
    params.put(name, value.toString)
    this
  }
}

class OpActions {
  val stack =  ListBuffer.empty[Action]
  val memory =  ListBuffer.empty[Action]
  val storage =  ListBuffer.empty[Action]

  def addStackPop: Action = addAction(stack, Name.pop)

  def addStackPush(value: DataWord): Action = {
    addAction(stack, Name.push).addParam("value", value)
  }

  def addStackSwap(from: Int, to: Int): Action = {
    addAction(stack, Name.swap).addParam("from", from).addParam("to", to)
  }

  def addMemoryExtend(delta: Long): Action = {
    addAction(memory, Name.extend).addParam("delta", delta)
  }

  def addMemoryWrite(address: Int, data: Array[Byte], size: Int): Action = {
    import com.apex.vm._
    addAction(memory, Name.write)
      .addParam("address", address)
      .addParam("data", data.toHex.substring(0, size))
  }

  def addStoragePut(key: DataWord, value: DataWord): Action = {
    addAction(storage, Name.put).addParam("key", key).addParam("value", value)
  }

  def addStorageRemove(key: DataWord): Action = {
    addAction(storage, Name.remove).addParam("key", key)
  }

  def addStorageClear: Action = addAction(storage, Name.clear)

  private def addAction(container: ListBuffer[Action], name: Name.Value) = {
    val action = new Action(name)
    container.append(action)
    action
  }
}
