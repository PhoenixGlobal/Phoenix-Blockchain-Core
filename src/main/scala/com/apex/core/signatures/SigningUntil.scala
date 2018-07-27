package com.apex.core.signatures

import java.security.SecureRandom

trait SigningUntil {

  val SignatureLength: Int
  val KeyLength: Int

  def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey)

  def createKeyPair: (PrivateKey, PublicKey) = {
    val seed = new Array[Byte](KeyLength)
    new SecureRandom().nextBytes(seed) // modifies seed
    createKeyPair(seed)
  }

  def sign(privateKey: PrivateKey, message: MessageToSign): Signature

  def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean

  def createSharedSecret(privateKey: PrivateKey, publicKey: PublicKey): SharedSecret
}
