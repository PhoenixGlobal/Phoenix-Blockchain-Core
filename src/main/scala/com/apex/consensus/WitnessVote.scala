/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Vote.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

import com.apex.core.Transaction
import com.apex.crypto.{FixedNumber, UInt160}
import play.api.libs.json.{JsValue, Json, Writes}

import scala.collection.mutable

case class WitnessVote(voter: UInt160,
                targetMap: mutable.Map[UInt160, FixedNumber] = scala.collection.mutable.Map[UInt160, FixedNumber](),
                version: Int = 0x01) extends com.apex.common.Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(voter)
    os.writeMap(targetMap.toMap)
  }

  def updateTargetCounter(addr: UInt160, counter: FixedNumber): WitnessVote = {
    if (targetMap.get(addr).isEmpty && counter.value < 0)
      return this
    val oldCounter = targetMap.getOrElse(addr, FixedNumber.Zero)
    if (oldCounter + counter >= FixedNumber.Zero)
      targetMap.update(addr, oldCounter + counter)
    this
  }

}

case class WitnessVoteInfo(voter: UInt160,
                           targetMap: mutable.Map[UInt160, FixedNumber],
                           pendingRefundTxs: Seq[Transaction],
                           version: Int = 0x01)

object WitnessVote {

  def deserialize(is: DataInputStream): WitnessVote = {
    import com.apex.common.Serializable._

    val version = is.readInt
    val voter = UInt160.deserialize(is)
    val target = is.readMap(UInt160.deserialize, FixedNumber.deserialize)

    new WitnessVote(voter, mutable.Map(target.toSeq: _*), version)
  }

  implicit val witnessVoteWrites = new Writes[WitnessVote] {
    override def writes(o: WitnessVote): JsValue = {
      Json.obj(
        "voter" -> o.voter.address,
        "target" -> o.targetMap.map(t => t._1.address +" - "+ t._2.toString()),
        "version" -> o.version
      )
    }
  }

  implicit val witnessVoteInfoWrites = new Writes[WitnessVoteInfo] {
    override def writes(o: WitnessVoteInfo): JsValue = {
      Json.obj(
        "voter" -> o.voter.address,
        "target" -> o.targetMap.map(t => t._1.address +" - "+ t._2.toString()),
        "version" -> o.version,
        "pendingRefundTxs" -> o.pendingRefundTxs
      )
    }
  }
}
