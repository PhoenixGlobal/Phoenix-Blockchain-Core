/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcCommands.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-09 下午4:06@version: 1.0
 */

package com.apex.rpc

import java.io.{ByteArrayInputStream, DataInputStream}
import com.apex.core.Transaction
import com.apex.crypto.{BinaryData, UInt160, UInt256}
import com.apex.crypto.Ecdsa.PublicKeyHash
import play.api.libs.json.Reads._
import play.api.libs.json._

object Validators {
  def uint256Validator = Reads.StringReads.filter(JsonValidationError("invalid UInt256"))(UInt256.parse(_).isDefined)

  def amountValidator = Reads.StringReads.filter(JsonValidationError("invalid amount"))(d => BigDecimal(d).signum > 0)

  def addressValidator = Reads.StringReads.filter(JsonValidationError("invalid Address"))(PublicKeyHash.fromAddress(_).isDefined)

  // TODO
  def HexValidator = Reads.StringReads.filter(JsonValidationError("invalid Address"))(d => true)
}

trait RPCCommand

case class GetBlocksCmd() extends RPCCommand

case class GetBlockCountCmd() extends RPCCommand

case class GetBlockCountResult(count: Long)

object GetBlockCountResult {
  implicit val writes = new Writes[GetBlockCountResult] {
    override def writes(o: GetBlockCountResult): JsValue = Json.obj(
      "count" -> o.count.toLong
    )
  }
}

case class GetLatesBlockInfoCmd() extends RPCCommand

case class GetAverageCmd() extends RPCCommand

case class GetAccountCmd(address: UInt160) extends RPCCommand

object GetAccountCmd {
  implicit val testWrites = new Writes[GetAccountCmd] {
    override def writes(o: GetAccountCmd): JsValue = Json.obj(
      "address" -> o.address.toString
    )
  }
  implicit val testReads: Reads[GetAccountCmd] = (
    (__ \ "address").read[String](Validators.addressValidator).map(c => PublicKeyHash.fromAddress(c).get)
    ) map (GetAccountCmd.apply _)
}

case class SendRawTransactionCmd(rawTx: Transaction) extends RPCCommand

object SendRawTransactionCmd {
  implicit val testWrites = new Writes[SendRawTransactionCmd] {
    override def writes(o: SendRawTransactionCmd): JsValue = Json.obj(
      "rawTx" -> BinaryData(o.rawTx.toBytes).toString
    )
  }
  implicit val testReads: Reads[SendRawTransactionCmd] = (
    (__ \ "rawTx").read[String](Validators.HexValidator).map(c => {
      val is = new DataInputStream(new ByteArrayInputStream(BinaryData(c)))
      Transaction.deserialize(is)
    })
    ) map (SendRawTransactionCmd.apply _)
}

case class GetBlockByHeightCmd(height: Int) extends RPCCommand

//{
//  def run(): JsValue = {
//    val block = Blockchain.Current.getBlock(height)
//    if (block != None) {
//      Json.toJson(block.get)
//    }
//    else {
//      Json.parse( """  {  "result": "Error"  }""")
//    }
//  }
//}

object GetBlockByHeightCmd {
  implicit val testWrites = new Writes[GetBlockByHeightCmd] {
    override def writes(o: GetBlockByHeightCmd): JsValue = Json.obj(
      "height" -> o.height
    )
  }
  implicit val testReads: Reads[GetBlockByHeightCmd] = (
    (JsPath \ "height").read[Int]

    ) map (GetBlockByHeightCmd.apply _)
}

case class GetBlockByIdCmd(id: UInt256) extends RPCCommand

//{
//  def run(): JsValue = {
//    val block = Blockchain.Current.getBlock(id)
//    if (block != None) {
//      Json.toJson(block.get)
//    }
//    else {
//      Json.parse( """  {  "result": "Error"  }""")
//    }
//  }
//}

object GetBlockByIdCmd {
  implicit val testWrites = new Writes[GetBlockByIdCmd] {
    override def writes(o: GetBlockByIdCmd): JsValue = Json.obj(
      "hash" -> o.id.toString
    )
  }
  implicit val testReads: Reads[GetBlockByIdCmd] = (
    (__ \ "hash").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get)
    ) map (GetBlockByIdCmd.apply _)
}


case class GetContractByIdCmd(id: UInt256) extends RPCCommand

object GetContractByIdCmd {
  implicit val testWrites = new Writes[GetContractByIdCmd] {
    override def writes(o: GetContractByIdCmd): JsValue = Json.obj(
      "id" -> o.id.toString
    )
  }
  implicit val testReads: Reads[GetContractByIdCmd] = (
    (__ \ "id").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get)
    ) map (GetContractByIdCmd.apply _)
}

case class GetProducerCmd(address: UInt160) extends RPCCommand

object GetProducerCmd {
  implicit val testWrites = new Writes[GetProducerCmd] {
    override def writes(o: GetProducerCmd): JsValue = Json.obj(
      "address" -> o.address.toString
    )
  }
  implicit val testReads: Reads[GetProducerCmd] = (
    (__ \ "address").read[String](Validators.addressValidator).map(c => PublicKeyHash.fromAddress(c).get)
    ) map (GetProducerCmd.apply _)
}

case class GetVotesCmd(address: UInt160) extends RPCCommand

object GetVotesCmd {
  implicit val testWrites = new Writes[GetVotesCmd] {
    override def writes(o: GetVotesCmd): JsValue = Json.obj(
      "address" -> o.address.toString
    )
  }
  implicit val testReads: Reads[GetVotesCmd] = (
    (__ \ "address").read[String](Validators.addressValidator).map(c => PublicKeyHash.fromAddress(c).get)
    ) map (GetVotesCmd.apply _)
}

case class GetProducersCmd(listType: String) extends RPCCommand

object GetProducersCmd {
  implicit val testWrites = new Writes[GetProducersCmd] {
    override def writes(o: GetProducersCmd): JsValue = Json.obj(
      "listType" -> o.listType
    )
  }
  implicit val testReads: Reads[GetProducersCmd] = (
    (JsPath \ "listType").read[String]
    ) map (GetProducersCmd.apply _)
}

case class ExecResult(var succeed: Boolean = true, var status: Int = 200, var message: String = "", var result: String = "")

object ExecResult {
  implicit val resultWrites = new Writes[ExecResult] {
    override def writes(o: ExecResult): JsValue = Json.obj(
      "succeed" -> o.succeed,
      "status" -> o.status,
      "message" -> o.message,
      "result" -> o.result
    )
  }
}
