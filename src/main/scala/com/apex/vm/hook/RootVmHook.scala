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
 * FileName: RootVmHook.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-29 上午11:47@version: 1.0
 *
 */

package com.apex.vm.hook

import com.apex.common.ApexLogging
import com.apex.vm.program.Program
import org.apex.vm.OpCode

/**
  * Primary VMHook implementation, that accepts other VM hook components to safely proxy all invocations to them.
  */
class RootVmHook(hooks: Array[VMHook] = Array.empty) extends VMHook with ApexLogging {
  override def startPlay(program: Program): Unit = {
    proxySafeToAll((hook: VMHook) => hook.startPlay(program))
  }

  override def step(program: Program, opcode: OpCode.Value): Unit = {
    proxySafeToAll((hook: VMHook) => hook.step(program, opcode))
  }

  override def stopPlay(program: Program): Unit = {
    proxySafeToAll((hook: VMHook) => hook.stopPlay(program))
  }

  override def isEmpty: Boolean = hooks.length == 0 || emptyHooksCount == hooks.length

  private def emptyHooksCount: Int = hooks.filterNot(_.isEmpty).length

  private def proxySafeToAll(action: VMHook => Unit): Unit = {
    def execute(hook: VMHook): Unit = try {
      action(hook)
    } catch {
      case t: Throwable => log.error("VM hook execution error: ", t)
    }

    hooks.filterNot(_.isEmpty).foreach(execute)
  }
}
