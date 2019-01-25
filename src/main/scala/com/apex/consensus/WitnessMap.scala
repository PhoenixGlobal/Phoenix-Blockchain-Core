/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: WitnessMap.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */


package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.{UInt160, UInt256}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

class WitnessMap(val witnesses: mutable.Map[UInt160, WitnessInfo],
                 val version: Int = 0x01) extends com.apex.common.Serializable {

  def set(witness: WitnessInfo) = {
    witnesses.update(witness.addr, witness)
  }

  def get(addr: UInt160): Option[WitnessInfo] = {
    witnesses.get(addr)
  }

  def delete(addr: UInt160) = {
    witnesses.remove(addr)
  }

  def getAll(): ArrayBuffer[WitnessInfo] = {
    val all = ArrayBuffer.empty[WitnessInfo]
    witnesses.foreach(f => all.append(f._2))
    all
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)
    os.writeMap(witnesses.toMap)
  }
}

object WitnessMap {

  def deserialize(is: DataInputStream): WitnessMap = {
    import com.apex.common.Serializable._

    val version = is.readInt()
    val witnesses = is.readMap(UInt160.deserialize, WitnessInfo.deserialize)

    new WitnessMap(mutable.Map(witnesses.toSeq: _*) , version)
  }

}
