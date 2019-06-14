/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Ecdsa.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.crypto

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.math.BigInteger

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.{ASN1Integer, DERSequenceGenerator}
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA1Digest, SHA256Digest, SHA512Digest}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters, ECPublicKeyParameters, KeyParameter}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.ECPoint
import com.apex.common.Serializable

object BinaryData {
  def apply(hex: String): BinaryData = hex

  val empty: BinaryData = Seq.empty[Byte]
}

case class BinaryData(data: Seq[Byte]) {
  def length = data.length

  override def toString = toHexString(data)
}

object Ecdsa {
  val params = SECNamedCurves.getByName("secp256k1")  // secp256r1
  val curve = new ECDomainParameters(params.getCurve, params.getG, params.getN, params.getH)
  val halfCurveOrder = params.getN().shiftRight(1)
  val zero = BigInteger.valueOf(0)
  val one = BigInteger.valueOf(1)

  def fixSize(data: BinaryData): BinaryData = data.length match {
    case 32 => data
    case length if length < 32 => Array.fill(32 - length)(0.toByte) ++ data
  }

  case class Scalar(value: BigInteger) {
    def add(scalar: Scalar): Scalar = Scalar(value.add(scalar.value)).mod(Ecdsa.curve.getN)

    def substract(scalar: Scalar): Scalar = Scalar(value.subtract(scalar.value)).mod(Ecdsa.curve.getN)

    def multiply(scalar: Scalar): Scalar = Scalar(value.multiply(scalar.value).mod(Ecdsa.curve.getN))

    def +(that: Scalar): Scalar = add(that)

    def -(that: Scalar): Scalar = substract(that)

    def *(that: Scalar): Scalar = multiply(that)

    def isZero: Boolean = value == BigInteger.ZERO

    def toBin: BinaryData = fixSize(value.toByteArray.dropWhile(_ == 0))

    def toPoint: Point = Point(params.getG() * value)

    override def toString = this.toBin.toString
  }

  object Scalar {
    def apply(data: BinaryData): Scalar = {
      require(data.length == 32, "scalar must be initialized with a 32 bytes value")
      new Scalar(new BigInteger(1, data))
    }
  }

  implicit def scalar2biginteger(scalar: Scalar): BigInteger = scalar.value

  implicit def biginteger2scalar(value: BigInteger): Scalar = Scalar(value)

  implicit def bin2scalar(value: BinaryData): Scalar = Scalar(value)

  implicit def scalar2bin(scalar: Scalar): BinaryData = scalar.toBin

  object PrivateKey {
    def apply(data: BinaryData): PrivateKey = data.length match {
      case 32 => new PrivateKey(Scalar(data), compressed = true)
      case 33 if data.last == 1 => new PrivateKey(Scalar(data.take(32)), compressed = true)
    }

    def toWIF(privateKey: Array[Byte]): String = {
      // always treat as compressed key, do NOT use uncompressed key
      assert(privateKey.length == 32)
      var data = new Array[Byte](34)
      // 0x80: mainnet
      data(0) = 0x80.toByte
      Array.copy(privateKey, 0, data, 1, 32)
      // 0x01: compressed
      data(33) = 0x01.toByte
      Base58Check.encode(data)
    }

    def fromWIF(wif: String): Option[PrivateKey] = {
      var privateKey: Option[PrivateKey] = None
      val decode = Base58Check.decode(wif).getOrElse(Array[Byte]())
      if (decode.length == 34) {
        // 1 bytes prefix + 32 bytes data + 1 byte 0x01 (+ 4 bytes checksum)
        if (decode(33) == 0x01.toByte)
          privateKey = Some(PrivateKey(decode.slice(1, 33)))
      }
      privateKey
    }

    def deserialize(is: DataInputStream): PrivateKey = {
      import com.apex.common.Serializable._
      PrivateKey(is.readByteArray)
    }
  }

  case class PrivateKey(value: Scalar, compressed: Boolean = true) extends com.apex.common.Serializable {

    def publicKey: PublicKey = PublicKey(value.toPoint, compressed)

    // 32 or 33 bytes
    //def toBin: BinaryData = if (compressed) value.toBin :+ 1.toByte else value.toBin

    def toBin: BinaryData = value.toBin // treat all as 32 bytes, compressed

    def toWIF: String = {
      // always treat as compressed key, do NOT use uncompressed key
      PrivateKey.toWIF(value.toBin)
    }

    override def serialize(os: DataOutputStream): Unit = {
      import com.apex.common.Serializable._
      os.writeByteArray(toBin)
    }

    override def toString = toBin.toString
  }

  implicit def privatekey2scalar(priv: PrivateKey): Scalar = priv.value

  case class Point(value: ECPoint) extends com.apex.common.Serializable {
    def add(point: Point): Point = Point(value.add(point.value))

    def substract(point: Point): Point = Point(value.subtract(point.value))

    def multiply(scalar: Scalar): Point = Point(value.multiply(scalar.value))

    def normalize = Point(value.normalize())

    def +(that: Point): Point = add(that)

    def -(that: Point): Point = substract(that)

    def *(that: Scalar): Point = multiply(that)

    def toBin(compressed: Boolean): BinaryData = value.getEncoded(compressed)

    //protected def writeReplace: Object = PointProxy(toBin(true))

    override def toString = toBin(true).toString

    override def serialize(os: DataOutputStream): Unit = {
      val data = toBin(true)
      os.write(data.length)
      os.write(data)
    }

  }

  //  case class PointProxy(bin: BinaryData) {
  //    def readResolve: Object = Point(bin)
  //  }

  object Point {
    def apply(data: BinaryData): Point = Point(curve.getCurve.decodePoint(data))

    def deserialize(is: DataInputStream): Point = {
      val data = Array.fill(is.readInt)(0.toByte)
      is.read(data, 0, data.length)
      Point(data)
    }
  }

  implicit def point2ecpoint(point: Point): ECPoint = point.value

  implicit def ecpoint2point(value: ECPoint): Point = Point(value)

  object PublicKey {
    implicit val deserializer: DataInputStream => PublicKey = deserialize

    def apply(data: BinaryData): PublicKey = data.length match {
      //case 65 if data.head == 4 => new PublicKey(Point(data), false)
      //case 65 if data.head == 6 || data.head == 7 => new PublicKey(Point(data), false)
      case 33 if data.head == 2 || data.head == 3 => new PublicKey(Point(data), true)
    }

    def deserialize(is: DataInputStream): PublicKey = {
      import com.apex.common.Serializable._
      PublicKey(is.readByteArray)
    }
  }

  case class PublicKey(value: Point, compressed: Boolean = true) extends com.apex.common.Serializable {
    def toBin: BinaryData = value.toBin(compressed)

    //def hash160: BinaryData = Ecdsa.hash160(toBin)

    def pubKeyHash: UInt160 = UInt160.fromBytes(Ecdsa.hash160(toBin))

    override def toString = toBin.toString

    def address: String = {
      PublicKeyHash.toAddress(pubKeyHash.data)
    }

    override def serialize(os: DataOutputStream): Unit = {
      import com.apex.common.Serializable._
      os.writeByteArray(toBin)
    }
  }

  implicit def publickey2point(pub: PublicKey): Point = pub.value

  implicit def publickey2bin(pub: PublicKey): BinaryData = pub.toBin

  object PublicKeyHash {

    private val prefixString = "AP"
    private val prefixBin = BinaryData("0548") // "0548" is for the "AP" prefix

    def toAddress(hash: Array[Byte]): String = {
      assert(hash.length == 20)
      Base58Check.encode(prefixBin, hash)
    }

    def fromAddress(address: String): Option[UInt160] = {
      var publicKeyHash: Option[UInt160] = None
      if (address.startsWith(prefixString) && address.length == 35) {
        val decode = Base58Check.decode(address).getOrElse(Array[Byte]())
        // 2 bytes prefix + 20 bytes data (+ 4 bytes checksum)
        if (decode.length == 22 && BinaryData(decode.slice(0, 2)) == prefixBin)
          publicKeyHash = Some(UInt160.fromBytes(decode.slice(2, 22)))
      }
      publicKeyHash
    }
  }

  def hash(digest: Digest)(input: Seq[Byte]): BinaryData = {
    digest.update(input.toArray, 0, input.length)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    out
  }

  def sha1 = hash(new SHA1Digest) _

  def sha256 = hash(new SHA256Digest) _

  def ripemd160 = hash(new RIPEMD160Digest) _

  def hash160(input: Seq[Byte]) = ripemd160(sha256(input))

  def hash256(input: Seq[Byte]) = sha256(sha256(input))

  def encodeSignature(r: BigInteger, s: BigInteger): BinaryData = {

    val bos = new ByteArrayOutputStream(72)
    val seq = new DERSequenceGenerator(bos)
    seq.addObject(new ASN1Integer(r))
    seq.addObject(new ASN1Integer(s))
    seq.close()
    bos.toByteArray
  }

  def encodeSignature(t: (BigInteger, BigInteger)): BinaryData = encodeSignature(t._1, t._2)

  //  def normalizeSignature(r: BigInteger, s: BigInteger): (BigInteger, BigInteger) = {
  //    val s1 = if (s.compareTo(halfCurveOrder) > 0) curve.getN().subtract(s) else s
  //    (r, s1)
  //  }

  //  def normalizeSignature(sig: BinaryData): BinaryData = {
  //    val (r, s) = decodeSignature(sig)
  //    encodeSignature(normalizeSignature(r, s))
  //  }

  // we use only the 33 bytes compressed pub key, don not use uncompressed key
  def isPubKeyValid(key: Seq[Byte]): Boolean = key.length match {
    //case 65 if key(0) == 4 || key(0) == 6 || key(0) == 7 => true
    case 33 if key(0) == 2 || key(0) == 3 => true
    case _ => false
  }

  //  def isPubKeyCompressedOrUncompressed(key: Seq[Byte]): Boolean = key.length match {
  //    case 65 if key(0) == 4 => true
  //    case 33 if key(0) == 2 || key(0) == 3 => true
  //    case _ => false
  //  }

  //  def isPubKeyCompressed(key: Seq[Byte]): Boolean = key.length match {
  //    case 33 if key(0) == 2 || key(0) == 3 => true
  //    case _ => false
  //  }

  def isPrivateKeyCompressed(key: PrivateKey): Boolean = key.compressed

  def decodeSignature(blob: Seq[Byte]): (BigInteger, BigInteger) = {
    decodeSignatureLax(blob)
  }

  def decodeSignatureLax(input: ByteArrayInputStream): (BigInteger, BigInteger) = {
    require(input.read() == 0x30)

    def readLength: Int = {
      val len = input.read()
      if ((len & 0x80) == 0) len else {
        var n = len - 0x80
        var len1 = 0
        while (n > 0) {
          len1 = (len1 << 8) + input.read()
          n = n - 1
        }
        len1
      }
    }

    readLength
    require(input.read() == 0x02)
    val lenR = readLength
    val r = new Array[Byte](lenR)
    input.read(r)
    require(input.read() == 0x02)
    val lenS = readLength
    val s = new Array[Byte](lenS)
    input.read(s)
    (new BigInteger(1, r), new BigInteger(1, s))
  }

  def decodeSignatureLax(input: BinaryData): (BigInteger, BigInteger) = decodeSignatureLax(new ByteArrayInputStream(input))

  def verifySignature(data: Seq[Byte], signature: (BigInteger, BigInteger), publicKey: PublicKey): Boolean =
    verifySignature(data, encodeSignature(signature), publicKey)

  def verifySignature(data: BinaryData, signature: BinaryData, publicKey: PublicKey): Boolean = {
    val (r, s) = decodeSignature(signature)
    require(r.compareTo(one) >= 0, "r must be >= 1")
    require(r.compareTo(curve.getN) < 0, "r must be < N")
    require(s.compareTo(one) >= 0, "s must be >= 1")
    require(s.compareTo(curve.getN) < 0, "s must be < N")

    val signer = new ECDSASigner
    val params = new ECPublicKeyParameters(publicKey.value, curve)
    signer.init(false, params)
    signer.verifySignature(data.toArray, r, s)
  }

  //def publicKeyFromPrivateKey(privateKey: BinaryData) = PrivateKey(privateKey).publicKey

  def sign(data: BinaryData, privateKey: PrivateKey): (BigInteger, BigInteger) = {
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
    val privateKeyParameters = new ECPrivateKeyParameters(privateKey.value, curve)
    signer.init(true, privateKeyParameters)
    val Array(r, s) = signer.generateSignature(data.toArray)

    if (s.compareTo(halfCurveOrder) > 0) {
      (r, curve.getN().subtract(s)) // if s > N/2 then s = N - s
    } else {
      (r, s)
    }
  }

  private def recoverPoint(x: BigInteger): (Point, Point) = {
    val x1 = Ecdsa.curve.getCurve.fromBigInteger(x)
    val square = x1.square().add(Ecdsa.curve.getCurve.getA).multiply(x1).add(Ecdsa.curve.getCurve.getB)
    val y1 = square.sqrt()
    val y2 = y1.negate()
    val R1 = Ecdsa.curve.getCurve.createPoint(x1.toBigInteger, y1.toBigInteger).normalize()
    val R2 = Ecdsa.curve.getCurve.createPoint(x1.toBigInteger, y2.toBigInteger).normalize()
    if (y1.testBitZero()) (R2, R1) else (R1, R2)
  }

  def recoverPublicKey(t: (BigInteger, BigInteger), message: BinaryData): (PublicKey, PublicKey) = {
    val (r, s) = t
    val m = new BigInteger(1, message)

    val (p1, p2) = recoverPoint(r)
    val Q1 = (p1.multiply(s).subtract(Ecdsa.curve.getG.multiply(m))).multiply(r.modInverse(Ecdsa.curve.getN))
    val Q2 = (p2.multiply(s).subtract(Ecdsa.curve.getG.multiply(m))).multiply(r.modInverse(Ecdsa.curve.getN))
    (PublicKey(Q1), PublicKey(Q2))
  }

  def recoverPublicKey(sig: BinaryData, message: BinaryData): (PublicKey, PublicKey) = {
    recoverPublicKey(Ecdsa.decodeSignature(sig), message)
  }
}
