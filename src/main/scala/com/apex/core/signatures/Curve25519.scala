package com.apex.core.signatures

import java.lang.reflect.Constructor

import com.apex.crypto.Crypto
import org.slf4j.LoggerFactory
import org.whispersystems.curve25519.OpportunisticCurve25519Provider

import scala.util.{Failure, Try}

object Curve25519 extends EllipticCurveSignatureScheme {

  val SignatureLength25519 = 64
  val KeyLength25519 = 32

  override val SignatureLength = SignatureLength25519
  override val KeyLength = KeyLength25519

  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider]
      .getDeclaredConstructors
      .head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val hashedSeed = Crypto.sha256(seed)
    val privateKey = PrivateKey @@ provider.generatePrivateKey(hashedSeed)
    privateKey -> PublicKey @@ provider.generatePublicKey(privateKey)
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.length == KeyLength)
    Signature @@ provider.calculateSignature(provider.getRandom(SignatureLength), privateKey, message)
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean = Try {
    require(signature.length == SignatureLength)
    require(publicKey.length == KeyLength)
    provider.verifySignature(publicKey, message, signature)
  }.recoverWith { case e =>
    log.debug("Error while message signature verification", e)
    Failure(e)
  }.getOrElse(false)

  override def createSharedSecret(privateKey: PrivateKey, publicKey: PublicKey): SharedSecret = {
    SharedSecret @@ provider.calculateAgreement(privateKey, publicKey)
  }

  protected lazy val log = LoggerFactory.getLogger(this.getClass)
}