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
 * FileName: ProgramPrecompile.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 下午3:00@version: 1.0
 *
 */

package com.apex.vm.program

import java.io.{DataInputStream, DataOutputStream}

import org.apex.vm.OpCode

class ProgramPrecompile extends com.apex.common.Serializable {

  import ProgramPrecompile._

  private val jumpdest = collection.mutable.Set.empty[Int]

  def hasJumpDest(pc: Int): Boolean = jumpdest.contains(pc)

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeVarInt(version)
    os.writeVarInt(jumpdest.size)
    jumpdest.foreach(os.writeVarInt)
  }
}

object ProgramPrecompile {
  private val version = 1

  def deserialize(is: DataInputStream): ProgramPrecompile = {
    import com.apex.common.Serializable._
    val ver = is.readVarInt
    if (ver != version) {
      null
    } else {
      val ret = new ProgramPrecompile
      for (_ <- 1 to is.readVarInt) {
        ret.jumpdest.add(is.readVarInt)
      }
      ret
    }
  }

  def compile(ops: Array[Byte]): ProgramPrecompile = {
    val ret = new ProgramPrecompile
    var i = 0
    while (i < ops.length) {
      try {
        val op = OpCode(ops(i))
        if (op == OpCode.JUMPDEST) ret.jumpdest.add(i)
        if (op.value >= OpCode.PUSH1.value && op.value <= OpCode.PUSH32.value) {
          i += op.value - OpCode.PUSH1.value + 1
        }
      } catch {
        case _: NoSuchElementException =>
      }
      i += 1
    }
    ret
  }
}
