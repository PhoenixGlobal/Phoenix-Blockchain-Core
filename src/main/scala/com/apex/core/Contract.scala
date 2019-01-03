/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Contract.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-12-13 上午14:32@version: 1.0
 */

package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.Ecdsa.PublicKey
import com.apex.crypto.UInt160

case class Contract(address: UInt160,
                    code: Array[Byte],
                    name: String = "",
                    author: String = "",
                    email: String = "",
                    description: String = "") extends com.apex.common.Serializable {

  import Contract._

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(address)
    os.writeByteArray(code)
    os.writeString(name)
    os.writeString(author)
    os.writeString(email)
    os.writeString(description)
  }
}

object Contract {
  private val version: Int = 0x01

  def deserialize(is: DataInputStream): Contract = {
    import com.apex.common.Serializable._
    // TODO check version
    is.readInt
    Contract(
      address = is.readObj[UInt160],
      code = is.readByteArray,
      name = is.readString(),
      author = is.readString(),
      email = is.readString(),
      description = is.readString()
    )
  }
}

case class ContractState(owner: PublicKey, key: Array[Byte], value: Array[Byte]) extends com.apex.common.Serializable {
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.write(owner)
    os.writeByteArray(key)
    os.writeByteArray(value)
  }
}

object ContractState {
  def deserialize(is: DataInputStream): ContractState = {
    import com.apex.common.Serializable._
    ContractState(
      is.readObj[PublicKey],
      is.readByteArray,
      is.readByteArray
    )
  }
}


case class ContractCompile(name: String = "",
                   source: String = "") extends com.apex.common.Serializable {

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(name)
    os.writeString(source)
  }
}

object ContractCompile {

  def deserialize(is: DataInputStream): ContractCompile = {
    import com.apex.common.Serializable._
    ContractCompile(
      name = is.readString(),
      source = is.readString()
    )
  }
}