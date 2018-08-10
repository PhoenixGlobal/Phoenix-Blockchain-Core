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

}

case class GetBlockCmd(index: Int, hash: UInt256) {
  def run(): JsValue = {
    val block = Blockchain.Current.getBlock(index).get
    Json.toJson(block)
  }
}
object GetBlockCmd {
  implicit val testWrites = new Writes[GetBlockCmd] {
    override def writes(o: GetBlockCmd): JsValue = Json.obj(
      "index" -> o.index,
      "hash" -> o.hash.toString
    )
  }
  implicit val testReads: Reads[GetBlockCmd] = (
    (JsPath \ "index").read[Int] and
    (JsPath \ "hash").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get)
    ) (GetBlockCmd.apply _)
}


case class SendCmd(address: String, assetId: UInt256, value: String) {
  def run(): JsValue = {
    val tx = Wallet.makeTransaction(address, assetId, Fixed8.fromDecimal(BigDecimal(value)))
    if (tx != None) {
      LocalNode.default.addTransaction(tx.get)
      Json.parse( """  { "result": "OK"  }""")
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
      "value" -> o.value
    )
  }
  implicit val testReads: Reads[SendCmd] = (
      (JsPath \ "address").read[String] and
      (JsPath \ "assetId").read[String](Validators.uint256Validator).map(c => UInt256.parse(c).get) and
      (JsPath \ "value").read[String]
    ) (SendCmd.apply _)
}


