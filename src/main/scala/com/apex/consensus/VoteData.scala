/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * @author: fang.wu@chinapex.com: 2019-1-22 下午4:06@version: 1.0
 */
package com.apex.consensus

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.core.OperationType
import com.apex.crypto.{FixedNumber, UInt160}

case class WitnessVoteData(candidate: UInt160, voterCount: FixedNumber, operationType: OperationType.Value)
  extends Serializable{
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(candidate)
    os.write(voterCount)
    os.writeByte(operationType.toByte)
  }
}

object WitnessVoteData {

  def fromBytes(data: Array[Byte]): WitnessVoteData = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }

  def deserialize(is: DataInputStream): WitnessVoteData = {
    val candidate = UInt160.deserialize(is)
    val voterCount = FixedNumber.deserialize(is)
    val operationType = OperationType(is.readByte)
    new WitnessVoteData(candidate, voterCount,operationType)
  }
}
