/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcCommands.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-09 下午4:06@version: 1.0
 */

package com.apex.network.rpc

import com.apex.core.Block
import com.apex.crypto.{UInt160, UInt256}
import com.apex.crypto.Ecdsa.PublicKeyHash
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.mutable.ArrayBuffer

object Validators {
  def uint256Validator = Reads.StringReads.filter(JsonValidationError("invalid UInt256"))(UInt256.parse(_).isDefined)

  def amountValidator = Reads.StringReads.filter(JsonValidationError("invalid amount"))(d => BigDecimal(d).signum > 0)

  def addressValidator = Reads.StringReads.filter(JsonValidationError("invalid Address"))(PublicKeyHash.fromAddress(_).isDefined)
}

trait RPCCommand

case class GetBlocks() extends RPCCommand

case class GetBlocksResult(blocks: ArrayBuffer[Block])

object GetBlocksResult {
  implicit val writes = new Writes[GetBlocksResult] {
    override def writes(o: GetBlocksResult): JsValue = Json.obj(
      "blocks" -> o.blocks
    )
  }
}

case class GetBlockCountCmd() extends RPCCommand

//object GetBlockCountCmd extends RPCCommand

case class GetBlockCountResult(count: Int)

object GetBlockCountResult {
  implicit val writes = new Writes[GetBlockCountResult] {
    override def writes(o: GetBlockCountResult): JsValue = Json.obj(
      "count" -> o.count
    )
  }
}

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
      "id" -> o.id.toString
    )
  }
  implicit val testReads: Reads[GetBlockByIdCmd] = (
    (__ \ "id").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get)
    ) map (GetBlockByIdCmd.apply _)
}


case class SendCmd(address: String, assetId: UInt256, amount: String) extends RPCCommand
//{
//  def run(): JsValue = {
//    val tx = Wallet.makeTransaction(address, assetId, Fixed8.fromDecimal(BigDecimal(amount)))
//    if (tx != None) {
////TODO:      LocalNode.default.addTransaction(tx.get)
//      val txid = tx.get.id.toString
//      Json.parse( s"""  { "result": "OK", "txid":"$txid"  }""")
//    }
//    else {
//      Json.parse( """  {  "result": "Error"  }""")
//    }
//  }
//}

object SendCmd {
  implicit val testWrites = new Writes[SendCmd] {
    override def writes(o: SendCmd): JsValue = Json.obj(
      "address" -> o.address,
      "assetId" -> o.assetId.toString,
      "amount" -> o.amount
    )
  }
  implicit val testReads: Reads[SendCmd] = (
    (JsPath \ "address").read[String] and
      (JsPath \ "assetId").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get) and
      (JsPath \ "amount").read[String](Validators.amountValidator)
    ) (SendCmd.apply _)
}

case class ImportPrivKeyCmd(key: String) extends RPCCommand
//{
//  def run(): JsValue = {
//    if (Wallet.importPrivKeyFromWIF(key)) {
//      Json.parse( """  {  "result": "OK"  }""")
//    }
//    else {
//      Json.parse( """  {  "result": "Error"  }""")
//    }
//  }
//}

object ImportPrivKeyCmd {
  implicit val testWrites = new Writes[ImportPrivKeyCmd] {
    override def writes(o: ImportPrivKeyCmd): JsValue = Json.obj(
      "key" -> o.key
    )
  }
  implicit val testReads: Reads[ImportPrivKeyCmd] = (
    (__ \ "key").read[String]
    ) map (ImportPrivKeyCmd.apply _)
}

case class GetBalanceCmd(assetId: UInt256) extends RPCCommand
//{
//  def run(): JsValue = {
//    val balance = Wallet.getBalance(assetId).toString
//    Json.parse( s"""  {  "balance": $balance  }""")
//  }
//}

object GetBalanceCmd {
  implicit val testWrites = new Writes[GetBalanceCmd] {
    override def writes(o: GetBalanceCmd): JsValue = Json.obj(
      "assetId" -> o.assetId.toString
    )
  }
  implicit val testReads: Reads[GetBalanceCmd] = (
    (__ \ "assetId").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get)
    ) map (GetBalanceCmd.apply _)
}
