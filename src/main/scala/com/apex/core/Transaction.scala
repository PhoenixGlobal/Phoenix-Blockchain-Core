package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, UInt160, UInt256}
import com.apex.vm.GasCost
import play.api.libs.json.{JsValue, Json, Writes}

class Transaction(val txType: TransactionType.Value,
                  val from: UInt160,
                  val toPubKeyHash: UInt160,
                  val amount: FixedNumber,
                  val nonce: Long,
                  val data: BinaryData,
                  val gasPrice: FixedNumber,
                  val gasLimit: BigInt,
                  var signature: BinaryData,
                  val version: Int = 0x01,
                  val executeTime: Long = 0) extends Identifier[UInt256] with Serializable {

  def sender(): UInt160 = from

  def toAddress(): String = {
    toPubKeyHash.address
  }

  def isContractCreation(): Boolean = (txType == TransactionType.Deploy && toPubKeyHash == UInt160.Zero)

  def getContractAddress(): Option[UInt160] = {
    if (isContractCreation()) {
      Some(Crypto.calcNewAddr(from, BigInt(nonce).toByteArray))
    }
    else
      None
  }

  def transactionCost(): Long = {
    var cost: BigInt = 0
    if (isContractCreation())
      cost = GasCost.TRANSACTION_CREATE_CONTRACT
    else
      cost = GasCost.TRANSACTION
    cost += (zeroDataBytes() * GasCost.TX_ZERO_DATA)
    cost += (nonZeroDataBytes() * GasCost.TX_NO_ZERO_DATA)
    cost.longValue()
  }

  def zeroDataBytes(): Long = {
    var counter = 0
    data.data.foreach(d => if (d == 0) counter += 1)
    counter
  }

  def nonZeroDataBytes(): Long = {
    var counter = 0
    data.data.foreach(d => if (d != 0) counter += 1)
    counter
  }

  override protected def genId(): UInt256 = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serialize(os)
    UInt256.fromBytes(Crypto.hash256(bs.toByteArray))
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._

    serializeForSign(os)

    os.writeByteArray(signature)
  }

  def serializeForSign(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.writeByte(txType.toByte)
    os.write(from)
    os.write(toPubKeyHash)
    os.write(amount)
    os.writeLong(nonce)
    os.writeByteArray(data)
    os.write(gasPrice)
    os.writeByteArray(gasLimit.toByteArray)
    os.writeLong(executeTime)

    // skip signature

  }

  def dataForSigning(): Array[Byte] = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeForSign(os)
    bs.toByteArray
  }

  def sign(privateKey: Ecdsa.PrivateKey) = {
    signature = Crypto.sign(dataForSigning(), privateKey.toBin)
  }

  def verifySignature(): Boolean = {
    Crypto.verifySignature(dataForSigning(), signature, from)
  }

}

object Transaction {
  implicit val transactionWrites = new Writes[Transaction] {
    override def writes(o: Transaction): JsValue = {
      Json.obj(
            "hash" -> o.id.toString,
            "type" -> o.txType.toString,
            "from" -> { if (o.txType == TransactionType.Miner) "" else o.from.address },
            "to" ->  o.toAddress,
            "amount" -> o.amount.toString,
            "nonce" -> o.nonce.toString,
            "data" -> o.data.toString,
            "gasPrice" -> o.gasPrice.toString,
            "gasLimit" -> o.gasLimit.longValue(),
            "signature" -> o.signature.toString,
            "version" -> o.version,
            "executeTime" -> o.executeTime.toString
          )
    }
  }

  def deserialize(is: DataInputStream): Transaction = {
    import com.apex.common.Serializable._

    val version = is.readInt
    val txType = TransactionType(is.readByte)
    val from = UInt160.deserialize(is)
    val toPubKeyHash = UInt160.deserialize(is)
    val amount = FixedNumber.deserialize(is)
    val nonce = is.readLong
    val data = is.readByteArray
    val gasPrice = FixedNumber.deserialize(is)
    val gasLimit = BigInt(is.readByteArray)
    val executeTime = is.readLong()
    val signature = is.readByteArray

    new Transaction(txType, from, toPubKeyHash, amount, nonce, data, gasPrice, gasLimit, signature, version, executeTime)
  }
}