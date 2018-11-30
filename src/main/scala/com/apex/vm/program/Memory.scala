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
 * FileName: Memory.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-29 下午5:41@version: 1.0
 *
 */

package com.apex.vm.program

import com.apex.vm.DataWord
import com.apex.vm.program.listener.{ProgramListener, ProgramListenerAware}

import scala.collection.mutable.ListBuffer

class Memory extends ProgramListenerAware {
  private val CHUNK_SIZE = 1024
  private val WORD_SIZE = 32

  private val chunks = ListBuffer.empty[Array[Byte]]
  private var softSize = 0

  private var programListener: ProgramListener = _

  override def setProgramListener(listener: ProgramListener): Unit = {
    programListener = listener
  }

  def read(address: Int, size: Int): Array[Byte] = {
    if (size <= 0) {
      Array.empty
    } else {
      extend(address, size)
      val data = new Array[Byte](size)
      var chunkIndex = address / CHUNK_SIZE
      var chunkOffset = address % CHUNK_SIZE
      var toGrab = data.length
      var start = 0
      while ( {
        toGrab > 0
      }) {
        val copied = grabMax(chunkIndex, chunkOffset, toGrab, data, start)
        // read next chunk from the start
        chunkIndex += 1
        chunkOffset = 0
        // mark remind
        toGrab -= copied
        start += copied
      }
      data
    }
  }

  def write(address: Int, data: Array[Byte], dataSize: Int, limited: Boolean): Unit = {
    if (dataSize > 0) {
      val size = if (data.length < dataSize) data.length else data.length
      if (!limited) {
        extend(address, size)
      }

      var chunkIndex = address / CHUNK_SIZE
      var chunkOffset = address % CHUNK_SIZE
      var toCapture = 0
      if (limited) {
        toCapture = if (address + size > softSize) softSize - address else size
      } else {
        toCapture = size
      }

      var start = 0
      while (toCapture > 0) {
        val captured = captureMax(chunkIndex, chunkOffset, toCapture, data, start)
        // capture next chunk
        chunkIndex += 1
        chunkOffset = 0
        // mark remind
        toCapture -= captured
        start += captured
      }
      if (programListener != null) {
        programListener.onMemoryWrite(address, data, size)
      }
    }
  }

  def extendAndWrite(address: Int, allocSize: Int, data: Array[Byte]): Unit = {
    extend(address, allocSize)
    write(address, data, allocSize, false)
  }

  def extend(address: Int, size: Int): Unit = {
    if (size <= 0) return
    val newSize = address + size
    var toAllocate = newSize - internalSize
    if (toAllocate > 0) addChunks(math.ceil(toAllocate.toDouble / CHUNK_SIZE).toInt)
    toAllocate = newSize - softSize
    if (toAllocate > 0) {
      toAllocate = math.ceil(toAllocate.toDouble / WORD_SIZE).toInt * WORD_SIZE
      softSize += toAllocate
      if (programListener != null) programListener.onMemoryExtend(toAllocate)
    }
  }

  def readWord(address: Int): DataWord = {
    DataWord.of(read(address, 32))
  }

  // just access expecting all data valid
  def readByte(address: Int): Byte = {
    val chunkIndex = address / CHUNK_SIZE
    val chunkOffset = address % CHUNK_SIZE
    val chunk = chunks(chunkIndex)
    chunk(chunkOffset)
  }

  def size: Int = softSize

  def internalSize: Int = chunks.size * CHUNK_SIZE

  def getChunks = chunks.clone

  override def toString: String = {
    val memoryData = new StringBuilder
    val firstLine = new StringBuilder
    val secondLine = new StringBuilder
    var i = 0
    for (i <- 0 to softSize - 1) {
      val value = readByte(i)
      // Check if value is ASCII
      val character = if (0x20.toByte <= value && value <= 0x7e.toByte) new String(Array[Byte](value)) else "?"
      firstLine.append(character).append("")
      secondLine.append(value.toHexString).append(" ")
      if ((i + 1) % 8 == 0) {
        val tmp = Integer.toString(i - 7, 16).formatted("%4s").replace(" ", "0")
        memoryData.append("").append(tmp).append(" ")
        memoryData.append(firstLine).append(" ")
        memoryData.append(secondLine)
        if (i + 1 < softSize) memoryData.append("\n")
        firstLine.setLength(0)
        secondLine.setLength(0)
      }
    }
    memoryData.toString
  }

  private def captureMax(chunkIndex: Int, chunkOffset: Int, size: Int, src: Array[Byte], srcPos: Int) = {
    val chunk = chunks(chunkIndex)
    val toCapture = math.min(size, chunk.length - chunkOffset)
    System.arraycopy(src, srcPos, chunk, chunkOffset, toCapture)
    toCapture
  }

  private def grabMax(chunkIndex: Int, chunkOffset: Int, size: Int, dest: Array[Byte], destPos: Int) = {
    val chunk = chunks(chunkIndex)
    val toGrab = math.min(size, chunk.length - chunkOffset)
    System.arraycopy(chunk, chunkOffset, dest, destPos, toGrab)
    toGrab
  }

  private def addChunks(num: Int): Unit = {
    for (_ <- 1 to num) {
      chunks.append(new Array[Byte](CHUNK_SIZE))
    }
  }
}
