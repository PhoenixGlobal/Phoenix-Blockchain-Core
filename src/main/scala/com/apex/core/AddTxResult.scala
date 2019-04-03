package com.apex.core

import play.api.libs.json.{JsValue, Json, Writes}

class AddTxResult(val added: Boolean, val result: String, val txId: String = "")

object AddTxResult {
  implicit val resultWrites = new Writes[AddTxResult] {
    override def writes(o: AddTxResult): JsValue = Json.obj(
      "txId" -> o.txId,
      "added" -> o.added,
      "result" -> o.result
    )
  }
}

case class InvalidNonce(expected: Long, actual: Long) extends
  AddTxResult(false, s"Invalid nonce, expected：$expected but actual：$actual")

/*case class NonceTooBig(reqNonce: Long, txNonce: Long) extends
  AddTxResult(false, s"Invalid nonce: nonce too big, required: $reqNonce, tx.nonce: $txNonce")*/

case class HeighGasLimit(txAcceptGasLimit: Long) extends
  AddTxResult(false, s"Set too heigh gas-limit, it should not above ${txAcceptGasLimit}")

case class ExecuteError(error: String) extends
  AddTxResult(false, s"Executor error: ${error}")

object AddTxSucceed extends AddTxResult(true, "Succeed")

object AddTxError extends AddTxResult(false, "Error")

object SignatureFail extends AddTxResult(false, "Verify signature unsuccess")

object RefundTxError extends AddTxResult(false, "ApplyRefundTransaction error")

object InvalidType extends AddTxResult(false, "Tx type invalid")

object Added extends AddTxResult(true, "Added to mempool, pending process")

object MempoolFull extends AddTxResult(false, "Mempool full")

object SameTx extends AddTxResult(false, "Same tx already exist in mempool")

object ScheduleFeeNotEnough extends AddTxResult(false, "Schedule fee not enough")

object ExecuteTxTimeout extends AddTxResult(false, "Execute transaction Timeout")

object ExecutorTimeout extends AddTxResult(false, "Executor time out")