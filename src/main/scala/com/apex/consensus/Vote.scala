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
import play.api.libs.json.{JsValue, Json, Writes}

case class Vote(voter: UInt160,
                 target: UInt160,
                 count: FixedNumber,
                 version: Int = 0x01) extends com.apex.common.Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.write(voter)
    os.write(target)
    os.write(count)
  }

  def updateCounts(counts: FixedNumber): Vote ={
    this.copy(count = this.count + counts)
  }

}

object Vote {

  def deserialize(is: DataInputStream): Vote = {
    import com.apex.common.Serializable._

    val version = is.readInt
    val voter = UInt160.deserialize(is)
    val target = UInt160.deserialize(is)
    val count = FixedNumber.deserialize(is)

    new Vote(voter, target, count, version)
  }

  implicit val voteWrites = new Writes[Vote] {
    override def writes(o: Vote): JsValue = {
      Json.obj(
        "voter" -> o.voter.toString,
        "target" -> o.target.toString,
        "count" -> o.count.toString,
        "version" -> o.version
      )
    }
  }
}
