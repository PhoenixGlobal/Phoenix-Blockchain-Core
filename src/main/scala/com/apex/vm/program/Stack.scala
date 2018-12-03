/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Stack.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 下午7:40@version: 1.0
 *
 */

package com.apex.vm.program

import com.apex.vm.DataWord
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
