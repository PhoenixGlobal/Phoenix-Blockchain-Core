package com.apex.network

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}
import com.apex.common.Serializable

class NodeInfo(val address: String, val port: Int) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(address)
    os.writeInt(port)
  }

  override def toString: String = {
    address + ":" + port
  }

}

object NodeInfo {

  def build(address: String, port: Int): NodeInfo = {
    new NodeInfo(address, port)
  }

  def deserialize(is: DataInputStream): NodeInfo = {
    import com.apex.common.Serializable._
    val address = is.readString()
    val port = is.readInt()
    new NodeInfo(address, port)
  }

  def fromBytes(data: Array[Byte]): NodeInfo = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}
