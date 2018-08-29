package com.apex.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{BinaryData, Crypto, Ecdsa, Fixed8, UInt160, UInt256}
import play.api.libs.json.{JsValue, Json, Writes}

class Transaction(val txType: TransactionType.Value,
                           val from: BinaryData,   // 33 bytes pub key
                           val toPubKeyHash: UInt160,
                           val toName: String,
                           val amount: Fixed8,
                           val assetId: UInt256,
                           val nonce: Long,
                           val data: BinaryData,
                           var signature: BinaryData,
                           //TODO: gas price and gas limit
                           val version: Int = 0x01,
                           override protected var _id: UInt256 = null)
  extends Identifier[UInt256] with Serializable {

  //TODO: read settings
  def fee: Fixed8 = Fixed8.Zero

  def fromPubKeyHash() : UInt160 = {
    UInt160.fromBytes(Ecdsa.PublicKey(from).hash160)
  }

  def fromAddress(): String = {
    Ecdsa.PublicKey(from).toAddress
  }

  def toAddress(): String = {
    Ecdsa.PublicKeyHash.toAddress(toPubKeyHash.data)
  }

  override def serialize(os: DataOutputStream): Unit = {
    serializeExcludeId(os)
    os.write(id)
  }

  override protected def genId(): UInt256 = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeExcludeId(os)
    UInt256.fromBytes(Crypto.hash256(bs.toByteArray))
  }

  private def serializeExcludeId(os: DataOutputStream) = {
    import com.apex.common.Serializable._

    serializeForSign(os)

    os.writeByteArray(signature)

    //serializeExtraData(os)
  }

  //protected def serializeExtraData(os: DataOutputStream): Unit

  def serializeForSign(os: DataOutputStream) = {
    import com.apex.common.Serializable._
    os.writeByte(txType.toByte)
    os.writeInt(version)
    os.writeByteArray(from)
    os.write(toPubKeyHash)
    os.writeString(toName)
    os.write(amount)
    os.write(assetId)
    os.writeLong(nonce)
    os.writeByteArray(data)
    // skip signature

    //serializeExtraData(os)
  }

  def dataForSigning(): Array[Byte] = {
    val bs = new ByteArrayOutputStream()
    val os = new DataOutputStream(bs)
    serializeForSign(os)
    bs.toByteArray
  }

  //  def signInput(inputIndex: Int, sigHashType: Int, signatureVersion: Int, privateKey: Ecdsa.PrivateKey) = {
  //    val sig = Crypto.sign(dataForSigning(sigHashType), privateKey.toBin)
  //    inputs(inputIndex).signatureScript = Script.write(OP_PUSHDATA(sig) :: OP_PUSHDATA(privateKey.publicKey) :: Nil)
  //  }

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
            "id" -> o.id.toString,
            "type" -> o.txType.toString,
            "from" -> Ecdsa.PublicKey(o.from).toAddress,
            "to" ->  o.toAddress,
            "toName" -> o.toName,
            "amount" -> o.amount.toString,
            "assetId" -> o.assetId.toString,
            "nonce" -> o.nonce.toString,
            "data" -> o.data.toString,
            "signature" -> o.signature.toString,
            "version" -> o.version
          )
    }
  }

  def deserialize(is: DataInputStream): Transaction = {
    import com.apex.common.Serializable._

    val txType = TransactionType(is.readByte)
    val version = is.readInt
    val from = is.readByteArray
    val toPubKeyHash = UInt160.deserialize(is)
    val toName = is.readString
    val amount = Fixed8.deserialize(is)
    val assetId = UInt256.deserialize(is)
    val nonce = is.readLong
    val data = is.readByteArray
    val signature = is.readByteArray

    val id = is.readObj(UInt256.deserialize)

    new Transaction(txType, from, toPubKeyHash, toName, amount, assetId, nonce, data, signature, version, id)
  }
}