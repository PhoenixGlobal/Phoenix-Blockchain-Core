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
 * FileName: CompositeProgramListener.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午2:04@version: 1.0
 *
 */

package com.apex.vm.program.listener

import com.apex.vm.DataWord

import scala.collection.mutable.ListBuffer

class CompositeProgramListener extends ProgramListener {
  private val listeners = ListBuffer.empty[ProgramListener]

  override def onMemoryExtend(delta: Int): Unit = {
    for (listener <- listeners) {
      listener.onMemoryExtend(delta)
    }
  }

  override def onMemoryWrite(address: Int, data: Array[Byte], size: Int): Unit = {
    for (listener <- listeners) {
      listener.onMemoryWrite(address, data, size)
    }
  }

  override def onStackPop(): Unit = {
    for (listener <- listeners) {
      listener.onStackPop()
    }
  }

  override def onStackPush(value: DataWord): Unit = {
    for (listener <- listeners) {
      listener.onStackPush(value)
    }
  }

  override def onStackSwap(from: Int, to: Int): Unit = {
    for (listener <- listeners) {
      listener.onStackSwap(from, to)
    }
  }

  override def onStoragePut(key: DataWord, value: DataWord): Unit = {
    for (listener <- listeners) {
      listener.onStoragePut(key, value)
    }
  }

  override def onStorageClear(): Unit = {
    for (listener <- listeners) {
      listener.onStorageClear()
    }
  }

  def addListener(listener: ProgramListener): Unit = {
    listeners.append(listener)
  }

  def isEmpty: Boolean = listeners.isEmpty
}
