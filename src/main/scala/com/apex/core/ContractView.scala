package com.apex.core

import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.crypto.{Crypto, UInt160, UInt256}

class ContractView(val name: String,
                   val author: String,
                   val email: String,
                   val description: String,
                   val script: Array[Byte],
                   val version: Int = 0x01,
                   override protected var _id: UInt160 = null) extends Identifier[UInt160] {

  override def serialize(os: DataOutputStream): Unit = {
    serializeExcludeId(os)
    os.write(id)
  }

  override protected def genId(): UInt160 = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeExcludeId(os)
    UInt160.fromBytes(Crypto.hash160(bs.toByteArray))
  }

  private def serializeExcludeId(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.writeString(name)
    os.writeString(author)
    os.writeString(email)
    os.writeString(description)
    os.writeByteArray(script)
  }
}

object ContractView {
  def deserialize(is: DataInputStream): ContractView = {
    import com.apex.common.Serializable._
    val version = is.readInt
    new ContractView(
      name = is.readString(),
      author = is.readString(),
      email = is.readString(),
      description = is.readString(),
      script = is.readByteArray,
      version = version,
      is.readObj(UInt160.deserialize)
    )
  }
}