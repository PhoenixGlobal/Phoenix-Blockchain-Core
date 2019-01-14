package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.crypto.{Ecdsa, FixedNumber, UInt160}
import org.bouncycastle.util.encoders.Hex
import play.api.libs.json.{JsValue, Json, Writes}

class Account(val pubKeyHash: UInt160,
              val active: Boolean,
              val name: String,
              val balance: FixedNumber,
              val nextNonce: Long,
              val codeHash: Array[Byte] = Array.empty,
              val version: Int = 0x01) extends com.apex.common.Serializable {

  //TODO check balance and code
  def isEmpty: Boolean = balance.isZero

  def address: String = Ecdsa.PublicKeyHash.toAddress(pubKeyHash.data)

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeInt(version)
    os.write(pubKeyHash)
    os.writeBoolean(active)
    os.writeString(name)
    os.write(balance)
    os.writeLong(nextNonce)
    os.writeByteArray(codeHash)
  }
}

object Account {
  implicit class Extension(account: Account) {
    def increaseNonce(): Account = {
      new Account(account.pubKeyHash, account.active, account.name, account.balance, account.nextNonce + 1, account.codeHash, account.version)
    }

    def addBalance(value: FixedNumber): Account = {
      new Account(account.pubKeyHash, account.active, account.name, account.balance + value, account.nextNonce, account.codeHash, account.version)
    }
  }

  def newAccount(pubKeyHash: UInt160): Account =
    new Account(pubKeyHash, true, "", FixedNumber.Zero, 0)

//  def increaseNonce(acct: Account): Account =
//    new Account(acct.pubKeyHash, acct.active, acct.name, acct.balance, acct.nextNonce + 1, acct.codeHash, acct.version)
//
//  def addBalance(acct: Account, value: FixedNumber): Account =
//    new Account(acct.pubKeyHash, acct.active, acct.name, acct.balance + value, acct.nextNonce, acct.codeHash, acct.version)

  def deserialize(is: DataInputStream): Account = {
    import com.apex.common.Serializable._
    val version = is.readInt
    val pubKeyHash = is.readObj[UInt160]
    val active = is.readBoolean
    val name = is.readString
    val balance = is.readObj[FixedNumber]
    val nextNonce = is.readLong
    val codeHash = is.readByteArray

    new Account(
      pubKeyHash = pubKeyHash,
      active = active,
      name = name,
      balance = balance,
      nextNonce = nextNonce,
      codeHash = codeHash,
      version = version
    )
  }

  implicit val accountWrites = new Writes[Account] {
    override def writes(o: Account): JsValue = {
      Json.obj(
        "address" -> o.address,
        "active" -> o.active,
        "name" -> o.name,
        "balance" -> o.balance.toString,
        "nextNonce" -> o.nextNonce,
        "codeHash" -> Hex.toHexString(o.codeHash),
        "version" -> o.version
      )
    }
  }
}