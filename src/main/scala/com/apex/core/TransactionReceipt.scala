/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: TransactionReceipt.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-12-24 上午11:31@version: 1.0
 *
 */

package com.apex.core
import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.{UInt160, UInt256}

case class TransactionReceipt(txId: UInt256, from: UInt160, to: UInt160, gasUsed: Long, totalGasUsed: Long, output: Array[Byte], status: Int) extends com.apex.common.Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(txId)
    os.write(from)
    os.write(to)
    os.writeLong(gasUsed)
    os.writeLong(totalGasUsed)
    os.writeByteArray(output)
    os.writeVarInt(status)
  }
}

object TransactionReceipt {
  def deserialize(is: DataInputStream): TransactionReceipt = {
    import com.apex.common.Serializable._
    TransactionReceipt(
      is.readObj[UInt256],
      is.readObj[UInt160],
      is.readObj[UInt160],
      is.readLong,
      is.readLong,
      is.readByteArray,
      is.readVarInt)
  }
}
