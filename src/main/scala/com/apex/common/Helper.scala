/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Helper.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-05 下午4:06@version: 1.0
 */

package com.apex.common

import java.io.{ByteArrayInputStream, IOException, InputStream, OutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}

import com.apex.crypto.BinaryData

object Helper {

  def uint8(input: InputStream): Int = input.read()

  def writeUInt8(input: Int, out: OutputStream): Unit = out.write(input & 0xff)

  def uint16(input: InputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Int = {
    val bin = new Array[Byte](2)
    input.read(bin)
    uint16(bin, order)
  }

  def uint16(input: BinaryData, order: ByteOrder): Int = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getShort & 0xFFFF
  }

  def writeUInt16(input: Int, out: OutputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit = out.write(writeUInt16(input, order))

  def writeUInt16(input: Int, order: ByteOrder): BinaryData = {
    val bin = new Array[Byte](2)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putShort(input.toShort)
    bin
  }

  def uint32(input: InputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Long = {
    val bin = new Array[Byte](4)
    input.read(bin)
    uint32(bin, order)
  }

  def uint32(input: BinaryData, order: ByteOrder): Long = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getInt() & 0xFFFFFFFFL
  }

  def writeUInt32(input: Long, out: OutputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit = out.write(writeUInt32(input, order))

  def writeUInt32(input: Long, order: ByteOrder): Array[Byte] = {
    val bin = new Array[Byte](4)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putInt((input & 0xffffffff).toInt)
    bin
  }

  def writeUInt32(input: Long): Array[Byte] = writeUInt32(input, ByteOrder.LITTLE_ENDIAN)

  def uint64(input: InputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Long = {
    val bin = new Array[Byte](8)
    input.read(bin)
    uint64(bin, order)
  }

  def uint64(input: BinaryData, order: ByteOrder): Long = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getLong()
  }

  def writeUInt64(input: Long, out: OutputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit = out.write(writeUInt64(input, order))

  def writeUInt64(input: Long, order: ByteOrder): Array[Byte] = {
    val bin = new Array[Byte](8)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putLong(input)
    bin
  }

  def varint(blob: Array[Byte]): Long = varint(new ByteArrayInputStream(blob))

  def varint(input: InputStream): Long = input.read() match {
    case value if value < 0xfd => value
    case 0xfd => uint16(input)
    case 0xfe => uint32(input)
    case 0xff => uint64(input)
  }

  def writeVarint(input: Int, out: OutputStream): Unit = writeVarint(input.toLong, out)

  def writeVarint(input: Long, out: OutputStream): Unit = {
    if (input < 0xfdL) writeUInt8(input.toInt, out)
    else if (input < 65535L) {
      writeUInt8(0xfd, out)
      writeUInt16(input.toInt, out)
    }
    else if (input < 1048576L) {
      writeUInt8(0xfe, out)
      writeUInt32(input.toInt, out)
    }
    else {
      writeUInt8(0xff, out)
      writeUInt64(input, out)
    }
  }

  def bytes(input: InputStream, size: Long): BinaryData = bytes(input, size.toInt)

  def bytes(input: InputStream, size: Int): BinaryData = {
    val blob = new Array[Byte](size)
    if (size > 0) {
      val count = input.read(blob)
      if (count < size) throw new IOException("not enough data to read from")
    }
    blob
  }

  def writeBytes(input: Array[Byte], out: OutputStream): Unit = out.write(input)

  def varstring(input: InputStream): String = {
    val length = varint(input)
    new String(bytes(input, length), "UTF-8")
  }

  def writeVarstring(input: String, out: OutputStream) = {
    writeVarint(input.length, out)
    writeBytes(input.getBytes("UTF-8"), out)
  }

  def hash(input: InputStream): BinaryData = bytes(input, 32) // a hash is always 256 bits

  def script(input: InputStream): BinaryData = {
    val length = varint(input) // read size
    bytes(input, length.toInt) // read bytes
  }

  def writeScript(input: Array[Byte], out: OutputStream): Unit = {
    writeVarint(input.length.toLong, out)
    writeBytes(input, out)
  }

  def weekNumOfYear(timeStamp: Long): Int = {
    val zonedDateTimeUtc = ZonedDateTime.ofInstant(Instant.ofEpochMilli(timeStamp), ZoneId.of("UTC"))
    val dateTimeFormatter = DateTimeFormatter.ofPattern("w") // DateTimeFormatter.ISO_OFFSET_DATE_TIME
    dateTimeFormatter.format(zonedDateTimeUtc).toInt
  }

}


