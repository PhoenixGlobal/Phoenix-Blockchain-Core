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

case class TransactionReceipt(txId: UInt256,
                              txType: TransactionType.Value,
                              from: UInt160,
                              to: UInt160,
                              gasUsed: BigInt,
                              totalGasUsed: BigInt,
                              output: Array[Byte],
                              status: Int) extends com.apex.common.Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(txId)
    os.writeByte(txType.toByte)
    os.write(from)
    os.write(to)
    os.writeByteArray(gasUsed.toByteArray)
    os.writeByteArray(totalGasUsed.toByteArray)
    os.writeByteArray(output)
    os.writeVarInt(status)
  }
}

object TransactionReceipt {
  def deserialize(is: DataInputStream): TransactionReceipt = {
    import com.apex.common.Serializable._
    TransactionReceipt(
      is.readObj[UInt256],
      TransactionType(is.readByte),
      is.readObj[UInt160],
      is.readObj[UInt160],
      BigInt(is.readByteArray),
      BigInt(is.readByteArray),
      is.readByteArray,
      is.readVarInt)
  }
}
