package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, FixedNumber, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

class Transaction(val txType: TransactionType.Value,
                  val from: Ecdsa.PublicKey, // 33 bytes pub key
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

  def fromPubKeyHash() : UInt160 = {
    from.pubKeyHash
  }

  def sender(): UInt160 = fromPubKeyHash

  def fromAddress(): String = {
    from.address
  }

  def toAddress(): String = {
    Ecdsa.PublicKeyHash.toAddress(toPubKeyHash.data)
  }

  def isContractCreation(): Boolean = (txType == TransactionType.Deploy && toPubKeyHash == UInt160.Zero)

  def getContractAddress(): Option[UInt160] = {
    if (isContractCreation()) {
      Some(Crypto.calcNewAddr(fromPubKeyHash, BigInt(nonce).toByteArray))
    }
    else
      None
  }

  def transactionCost(block: Block): Long = {
    // TODO
    0
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
    Crypto.verifySignature(dataForSigning(), signature, from.toBin)
  }

}

object Transaction {
  implicit val transactionWrites = new Writes[Transaction] {
    override def writes(o: Transaction): JsValue = {
      Json.obj(
            "id" -> o.id.toString,
            "type" -> o.txType.toString,
            "from" -> { if (o.txType == TransactionType.Miner) "" else o.fromAddress },
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
    val from = Ecdsa.PublicKey.deserialize(is)
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