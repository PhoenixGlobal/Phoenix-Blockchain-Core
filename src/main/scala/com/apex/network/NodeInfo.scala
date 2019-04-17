package com.apex.network

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable

object NodeType extends Enumeration {
  val SEED = Value(0x00)
  val TRUST = Value(0x10)
  val UNKNOWN = Value(0x20)

  implicit class Extension(val value: NodeType.Value) {
    def toByte: Byte = value.id.toByte
  }

}

class NodeInfo(val address: String, val port: Int, val nodeType: NodeType.Value = NodeType.UNKNOWN) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(address)
    os.writeInt(port)
    os.write(nodeType.toByte)
  }

  override def toString: String = {
    address + ":" + port + ":" + nodeType
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
    val nodeType = is.readByte()
    new NodeInfo(address, port, NodeType(nodeType))
  }

  def fromBytes(data: Array[Byte]): NodeInfo = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}
