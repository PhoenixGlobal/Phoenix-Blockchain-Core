package com.apex.consensus

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable

case class RegisterInfo(location: String)extends Serializable{
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeString(location)
  }
}

object RegisterInfo{
  def deserialize(is: DataInputStream): RegisterInfo = {
    import com.apex.common.Serializable._
    RegisterInfo(is.readString())
  }
}
