package com.apex.crypto

import akka.util.ByteString
import org.bouncycastle.asn1.x9.X9IntegerConverter
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{ECPublicKeyParameters, ECDomainParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.{ECCurve, ECPoint}

object ECDSASignature {

  val curveParams: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val curve: ECDomainParameters = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)

  val SLength = 32
  val RLength = 32
  val VLength = 1
  val EncodedLength: Int = RLength + SLength + VLength

  val uncompressedIndicator:Byte = 0x04

  val negativePointSign: Byte = 27
  val newNegativePointSign: Byte = 35
  val positivePointSign: Byte = 28
  val newPositivePointSign: Byte = 36

  val allowedPointSigns = Set(negativePointSign, positivePointSign)

  def apply(r: ByteString, s: ByteString, v: Byte): ECDSASignature = {
    ECDSASignature(BigInt(1, r.toArray), BigInt(1, s.toArray), v)
  }

  def sign(message: Array[Byte], keyPair: AsymmetricCipherKeyPair, chainId: Option[Byte] = None): ECDSASignature = {
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
    signer.init(true, keyPair.getPrivate)
    val components = signer.generateSignature(message)
    val r = components(0)
    val s = ECDSASignature.canonicalise(components(1))
    val v = ECDSASignature
      .calculateV(r, s, keyPair, message)
      .getOrElse(throw new RuntimeException("Failed to calculate signature rec id"))

    val pointSign = chainId match {
      case Some(id) if v == negativePointSign => (id * 2 + newNegativePointSign).toByte
      case Some(id) if v == positivePointSign => (id * 2 + newPositivePointSign).toByte
      case None => v
    }

    ECDSASignature(r, s, pointSign)
  }

  private def getRecoveredPointSign(pointSign: Byte, chainId: Option[Byte]): Option[Byte] = {
    (chainId match {
      case Some(id) =>
        if (pointSign == negativePointSign || pointSign == (id * 2 + newNegativePointSign).toByte) {
          Some(negativePointSign)
        } else if (pointSign == positivePointSign || pointSign == (id * 2 + newPositivePointSign).toByte) {
          Some(positivePointSign)
        } else {
          None
        }
      case None => Some(pointSign)
    }).filter(pointSign => allowedPointSigns.contains(pointSign))
  }

  private def canonicalise(s: BigInt): BigInt = {
    val halfCurveOrder: BigInt = curveParams.getN.shiftRight(1)
    if (s > halfCurveOrder) BigInt(curve.getN) - s
    else s
  }

  private def calculateV(r: BigInt, s: BigInt, key: AsymmetricCipherKeyPair, message: Array[Byte]): Option[Byte] = {

    val pubKey = key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    val recIdOpt = Seq(positivePointSign, negativePointSign).find { i =>
      recoverPubBytes(r, s, i, None, message).exists(java.util.Arrays.equals(_, pubKey))
    }
    recIdOpt
  }

  private def recoverPubBytes(r: BigInt, s: BigInt, recId: Byte, chainId: Option[Byte], message: Array[Byte]): Option[Array[Byte]] = {
    val order = curve.getCurve.getOrder

    val xCoordinate = r
    val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
    val prime = curveFp.getQ

    getRecoveredPointSign(recId, chainId).flatMap { recovery =>
      if (xCoordinate.compareTo(prime) < 0) {
        val R = constructPoint(xCoordinate, recovery)
        if (R.multiply(order).isInfinity) {
          val e = BigInt(1, message)
          val rInv = r.modInverse(order)
          val q = R.multiply(s.bigInteger).subtract(curve.getG.multiply(e.bigInteger)).multiply(rInv.bigInteger)
          Some(q.getEncoded(false).tail)
        } else None
      } else None
    }
  }

  private def constructPoint(xCoordinate: BigInt, recId: Int): ECPoint = {
    val x9 = new X9IntegerConverter
    val compEnc = x9.integerToBytes(xCoordinate.bigInteger, 1 + x9.getByteLength(curve.getCurve))
    compEnc(0) = if (recId == ECDSASignature.positivePointSign) 3.toByte else 2.toByte
    curve.getCurve.decodePoint(compEnc)
  }
}

case class ECDSASignature(r: BigInt, s: BigInt, v: Byte) {

  def publicKey(message: Array[Byte], chainId: Option[Byte] = None): Option[Array[Byte]] =
    ECDSASignature.recoverPubBytes(r, s, v, chainId, message)

  def publicKey(message: ByteString): Option[ByteString] =
    ECDSASignature.recoverPubBytes(r, s, v, None, message.toArray[Byte]).map(ByteString(_))
}
