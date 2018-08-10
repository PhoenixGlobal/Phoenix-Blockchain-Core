/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcCommands.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-09 下午4:06@version: 1.0
 */

package com.apex.network.rpc

import com.apex.core.Blockchain
import com.apex.crypto.{Fixed8, UInt256}
import com.apex.network.LocalNode
import com.apex.wallets.Wallet
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

object Validators {
  def uint256Validator = Reads.StringReads.filter(JsonValidationError("invalid UInt256"))(UInt256.parse(_).isDefined)

  def amountValidator = Reads.StringReads.filter(JsonValidationError("invalid amount"))(d => BigDecimal(d).signum > 0)
}

case class GetBlockByHeightCmd(height: Int) {
  def run(): JsValue = {
    val block = Blockchain.Current.getBlock(height)
    if (block != None) {
      Json.toJson(block.get)
    }
    else {
      Json.parse( """  {  "result": "Error"  }""")
    }
  }
}

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

case class GetBlockByIdCmd(id: UInt256) {
  def run(): JsValue = {
    val block = Blockchain.Current.getBlock(id)
    if (block != None) {
      Json.toJson(block.get)
    }
    else {
      Json.parse( """  {  "result": "Error"  }""")
    }
  }
}

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


case class SendCmd(address: String, assetId: UInt256, amount: String) {
  def run(): JsValue = {
    val tx = Wallet.makeTransaction(address, assetId, Fixed8.fromDecimal(BigDecimal(amount)))
    if (tx != None) {
      LocalNode.default.addTransaction(tx.get)
      val txid = tx.get.id.toString
      Json.parse( s"""  { "result": "OK", "txid":"$txid"  }""")
    }
    else {
      Json.parse( """  {  "result": "Error"  }""")
    }
  }
}

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


