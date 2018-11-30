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
 * FileName: Stack.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-27 下午7:53@version: 1.0
 *
 */

package com.apex.vm

import com.apex.vm.program.listener.{ProgramListener, ProgramListenerAware}

class Stack extends java.util.Stack[DataWord] with ProgramListenerAware {
  private var programListener: ProgramListener = _

  override def pop(): DataWord = {
    if (programListener != null) {
      programListener.onStackPop()
    }
    super.pop()
  }

  override def push(item: DataWord): DataWord = {
    if (programListener != null) {
      programListener.onStackPush(item)
    }
    super.push(item)
  }

  def swap(from: Int, to: Int): Unit = {
    if (isAccessible(from) && isAccessible(to) && (from != to)) {
      if (programListener != null) {
        programListener.onStackSwap(from, to)
      }
      val tmp: DataWord = get(from)
      set(from, set(to, tmp))
    }
  }

  private def isAccessible(from: Int): Boolean = {
    from >= 0 && from < size
  }

  override def setProgramListener(listener: ProgramListener): Unit = {
    programListener = listener
  }
}
