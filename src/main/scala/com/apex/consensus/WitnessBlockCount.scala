package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.{UInt160, UInt256}
import scala.collection.{immutable, mutable}

class WitnessBlockCount(val blockCount: mutable.Map[UInt160, Int],
                        val version: Int = 0x01) extends com.apex.common.Serializable {

  def set(witness: UInt160, blockNum: Int) = {
    blockCount.update(witness, blockNum)
  }

  def get(addr: UInt160): Option[Int] = {
    blockCount.get(addr)
  }

  def delete(addr: UInt160) = {
    blockCount.remove(addr)
  }

  def increase(addr: UInt160) = {
    val old = blockCount.get(addr).getOrElse(0)
    blockCount.update(addr, old + 1)
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.writeInt(version)

    os.writeVarInt(blockCount.toMap.size)
    blockCount.toMap.foreach(o => {
      os.write(o._1)
      os.writeInt(o._2)
    })
  }
}

object WitnessBlockCount {

  def deserialize(is: DataInputStream): WitnessBlockCount = {
    import com.apex.common.Serializable._

    val version = is.readInt()

    val blockCount = (1 to is.readVarInt) map (_ => UInt160.deserialize(is) -> is.readInt()) toMap

    new WitnessBlockCount(mutable.Map(blockCount.toSeq: _*) , version)
  }

}
