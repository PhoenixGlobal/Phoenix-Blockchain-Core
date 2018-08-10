/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: RpcCommands.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-09 下午4:06@version: 1.0
 */

package com.apex.network.rpc

import com.apex.core.Blockchain
import com.apex.crypto.UInt256
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class GetBlockCmd(index: Int, b: Int, c: UInt256) {
  def run(): JsValue = {
    val block = Blockchain.Current.getBlock(index).get
    Json.toJson(block)
  }
}
object GetBlockCmd {
  def uint256Validator = Reads.StringReads.filter(JsonValidationError("invalid UInt256"))(UInt256.parse(_).isDefined)

  implicit val testWrites = new Writes[GetBlockCmd] {
    override def writes(o: GetBlockCmd): JsValue = Json.obj(
      "index" -> o.index,
      "b" -> o.b,
      "c" -> o.c.toString
    )
  }
  implicit val testReads: Reads[GetBlockCmd] = (
    (JsPath \ "index").read[Int] and
      (JsPath \ "b").read[Int] and
      (JsPath \ "c").read[String](uint256Validator).map(c => UInt256.parse(c).get)
    ) (GetBlockCmd.apply _)
}