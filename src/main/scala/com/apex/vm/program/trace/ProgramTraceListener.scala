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
 * @author: ruixiao.xiao@chinapex.com: 18-11-28 下午2:05@version: 1.0
 *
 */

package com.apex.vm.program.trace

import com.apex.vm.DataWord
import com.apex.vm.program.listener.ProgramListenerAdaptor

class ProgramTraceListener(enabled: Boolean) extends ProgramListenerAdaptor {
  private var actions = new OpActions

  override def onMemoryExtend(delta: Int): Unit = {
    if (enabled) {
      actions.addMemoryExtend(delta)
    }
  }

  override def onMemoryWrite(address: Int, data: Array[Byte], size: Int): Unit = {
    if (enabled) {
      actions.addMemoryWrite(address, data, size)
    }
  }

  override def onStackPop(): Unit = {
    if (enabled) {
      actions.addStackPop
    }
  }

  override def onStackPush(value: DataWord): Unit = {
    if (enabled) {
      actions.addStackPush(value)
    }
  }

  override def onStackSwap(from: Int, to: Int): Unit = {
    if (enabled) {
      actions.addStackSwap(from, to)
    }
  }

  override def onStoragePut(key: DataWord, value: DataWord): Unit = {
    if (enabled) {
      if (value == DataWord.ZERO) {
        actions.addStorageRemove(key)
      } else {
        actions.addStoragePut(key, value)
      }
    }
  }

  override def onStorageClear(): Unit = {
    if (enabled) {
      actions.addStorageClear
    }
  }

  def resetActions(): OpActions = {
    val old = actions
    actions = new OpActions
    old
  }
}
