package com.apex.core

import java.io.{DataInputStream, DataOutputStream}

import com.apex.common.Serializable
import com.apex.crypto.{Crypto, UInt160}

class Witness(val invocationScript: Array[Byte], val verificationScript: Array[Byte]) extends Serializable {
  private var _hash: UInt160 = null

  def hash: UInt160 = {
    if (_hash == null) {
      _hash = UInt160.fromBytes(Crypto.hash160(verificationScript))
    }
    _hash
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Witness =>
        invocationScript.sameElements(that.invocationScript) &&
          verificationScript.sameElements(that.verificationScript)
      case _ => false
    }
  }

  override def serialize(os: DataOutputStream): Unit = {
    import com.apex.common.Serializable._
    os.writeByteArray(invocationScript)
    os.writeByteArray(verificationScript)
  }
}

object Witness {
  def deserialize(is: DataInputStream): Witness = {
    import com.apex.common.Serializable._
    new Witness(
      invocationScript = is.readByteArray,
      verificationScript = is.readByteArray
    )
  }
}
