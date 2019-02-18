package com.apex.network

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}
import com.apex.common.Serializable

class InetSocketAddressSer(val address: String, val port: Int) extends Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(address)
    os.writeInt(port)
  }

  override def toString: String = {
    address + ":" + port
  }

}

object InetSocketAddressSer {

  def build(address: String, port: Int): InetSocketAddressSer = {
    new InetSocketAddressSer(address, port)
  }

  def deserialize(is: DataInputStream): InetSocketAddressSer = {
    import com.apex.common.Serializable._
    val address = is.readString()
    val port = is.readInt()
    new InetSocketAddressSer(address, port)
  }

  def fromBytes(data: Array[Byte]): InetSocketAddressSer = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }
}
