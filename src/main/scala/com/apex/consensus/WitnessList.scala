/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: WitnessList.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

class WitnessList(val witnesses: Array[WitnessInfo],
                  val generateInBlockNum: Long,
                  val generateInBlockTime: Long,
                  val version: Int = 0x01) extends com.apex.common.Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.writeSeq(witnesses)
    os.writeLong(generateInBlockNum)
    os.writeLong(generateInBlockTime)
  }

  def sortByLocation() = {
    witnesses.sortWith((w1, w2) => {
      if (w1.longitude > w2.longitude)
        true
      else
        false // TODO
    })
  }

  def sortByVote() = {
    witnesses.sortWith((w1, w2) => {
      if (w1.voteCounts > w2.voteCounts)
        true
      else
        false // TODO
    })
  }
}

object WitnessList {

  def deserialize(is: DataInputStream): WitnessList = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val witnesses = is.readSeq(WitnessInfo.deserialize)
    val generateInBlockNum = is.readLong()
    val generateInBlockTime = is.readLong()

    new WitnessList(witnesses.toArray, generateInBlockNum, generateInBlockTime, version)
  }
}
