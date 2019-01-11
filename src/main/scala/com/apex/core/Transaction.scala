package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, UInt160, UInt256}
import com.apex.vm.GasCost
import play.api.libs.json.{JsValue, Json, Writes}

class Transaction(val txType: TransactionType.Value,
                  val from: UInt160,
                  val toPubKeyHash: UInt160,
                  val toName: String,
                  val amount: FixedNumber,
                  val nonce: Long,
                  val data: BinaryData,
                  val gasPrice: FixedNumber,
                  val gasLimit: BigInt,
                  var signature: BinaryData,
                  val version: Int = 0x01) extends Identifier[UInt256] with Serializable {

  //TODO: read settings
  def fee: FixedNumber = FixedNumber.Zero

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
    os.writeByte(txType.toByte)
    os.writeInt(version)
    os.write(from)
    os.write(toPubKeyHash)
    os.writeString(toName)
    os.write(amount)
    os.writeLong(nonce)
    os.writeByteArray(data)
    os.write(gasPrice)
    os.writeByteArray(gasLimit.toByteArray)

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
    val message = dataForSigning()
    if (Crypto.verifySignature(message, signature)) {
      val (pub1, pub2) = Crypto.recoverPublicKey(signature, message)
      val from1 = pub1.pubKeyHash
      val from2 = pub2.pubKeyHash
      if (from1 == from || from2 == from)
        true
      else
        false
    }
    else
      false
  }

}

object Transaction {
  implicit val transactionWrites = new Writes[Transaction] {
    override def writes(o: Transaction): JsValue = {
      Json.obj(
            "id" -> o.id.toString,
            "type" -> o.txType.toString,
            "from" -> { if (o.txType == TransactionType.Miner) "" else o.from.address },
            "to" ->  o.toAddress,
            "toName" -> o.toName,
            "amount" -> o.amount.toString,
            "nonce" -> o.nonce.toString,
            "data" -> o.data.toString,
            "gasPrice" -> o.gasPrice.toString,
            "gasLimit" -> o.gasLimit.longValue(),
            "signature" -> o.signature.toString,
            "version" -> o.version
          )
    }
  }

  def deserialize(is: DataInputStream): Transaction = {
    import com.apex.common.Serializable._

    val txType = TransactionType(is.readByte)
    val version = is.readInt
    val from = UInt160.deserialize(is)
    val toPubKeyHash = UInt160.deserialize(is)
    val toName = is.readString
    val amount = FixedNumber.deserialize(is)
    val nonce = is.readLong
    val data = is.readByteArray
    val gasPrice = FixedNumber.deserialize(is)
    val gasLimit = BigInt(is.readByteArray)
    val signature = is.readByteArray

    new Transaction(txType, from, toPubKeyHash, toName, amount, nonce, data, gasPrice, gasLimit, signature, version)
  }
}