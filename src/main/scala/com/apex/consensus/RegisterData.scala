/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * @author: fang.wu@chinapex.com: 2019-1-22 下午4:06@version: 1.0
 */
package com.apex.consensus

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.core.OperationType
import com.apex.crypto.UInt160
import play.api.libs.json.{JsValue, Json, Writes}

case class RegisterData(registerAccount: UInt160,
                        registerInfo: WitnessInfo = WitnessInfo(UInt160.Zero),
                        operationType: OperationType.Value)
  extends Serializable{
  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    os.write(registerAccount)
    os.write(registerInfo)
    os.writeByte(operationType.toByte)
  }
}

object RegisterData {

  def fromBytes(data: Array[Byte]): RegisterData = {
    val bs = new ByteArrayInputStream(data)
    val is = new DataInputStream(bs)
    deserialize(is)
  }

  def deserialize(is: DataInputStream): RegisterData = {
    val registerAccount = UInt160.deserialize(is)
    val registerInfo = WitnessInfo.deserialize(is)
    val operationType = OperationType(is.readByte)
    RegisterData(registerAccount, registerInfo, operationType)
  }

  implicit val registerData = new Writes[RegisterData] {
    override def writes(o: RegisterData): JsValue = {
      Json.obj(
        "registerAccount" -> o.registerAccount.address,
        "registerAccountInWitnessInfoClass" -> o.registerInfo.addr.address,
      )
    }
  }
}

