/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Vote.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.{FixedNumber, UInt160}

class Vote( val voter: UInt160,
            val target: UInt160,
            val counts: FixedNumber,
            val version: Int = 0x01) extends com.apex.common.Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(voter)
    os.write(target)
    os.write(counts)
  }

}

object Vote {

  def deserialize(is: DataInputStream): Vote = {
    import com.apex.common.Serializable._

    val version = is.readInt
    val voter = UInt160.deserialize(is)
    val target = UInt160.deserialize(is)
    val counts = FixedNumber.deserialize(is)

    new Vote(voter, target, counts, version)
  }

}
