package com.apex.common

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import com.apex.core.ModifierId
import com.apex.core.serialization.Serializer
import com.apex.core.structure.BoxStructure
import com.apex.core.structure.account.PublicKeyNoncedBox
import com.apex.core.structure.box.BoxUnlocker
import com.apex.core.structure.box.proposition.PublicKey25519Proposition
import com.apex.core.structure.proof.{Proof, Signature25519}
import com.apex.core.structure.state.{PrivateKey25519, PrivateKey25519Companion}
import com.apex.core.utils.ApexEncoding
import com.apex.core.hash.Blake2b256
import com.apex.core.signatures.{Curve25519, PublicKey, Signature}

import scala.util.Try

case class SimpleBoxStructure(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                to: IndexedSeq[(PublicKey25519Proposition, Value)],
                                signatures: IndexedSeq[Signature25519],
                                override val fee: Long,
                                override val timestamp: Long) extends
  BoxStructure[PublicKey25519Proposition, PublicKey25519NoncedBox] {

  override type M = SimpleBoxStructure

  lazy val boxIdsToOpen: IndexedSeq[ModifierId] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: ModifierId = boxId
        override val boxKey: Proof[PublicKey25519Proposition] = signature
      }
  }

  lazy val hashNoNonces = Blake2b256(
    Bytes.concat(com.apex.core.utils.concatFixLengthBytes(to.map(_._1.pubKeyBytes)),
      com.apex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  )

  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map { case ((prop, value), idx) =>
    val nonce = SimpleBoxStructure.nonceFromDigest(Blake2b256(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    PublicKey25519NoncedBox(prop, nonce, value)
  }

  override lazy val serializer = SimpleBoxStructureCompanion

  override def toString: String = s"SimpleBoxStructure(${this.asJson.noSpaces})"

  lazy val semanticValidity: Try[Unit] = Try {
    require(from.size == signatures.size)
    require(to.forall(_._2 >= 0))
    require(fee >= 0)
    require(timestamp >= 0)
    require(boxIdsToOpen.map(to => ByteArrayWrapper(to)).distinct.size == boxIdsToOpen.size)
    require(from.zip(signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, messageToSign)
    })
  }
}


object SimpleBoxStructure extends ApexEncoding {

  implicit val simpleBoxEncoder: Encoder[SimpleBoxStructure] = (sbe: SimpleBoxStructure) =>
    Map(
      "id" -> encoder.encode(sbe.id).asJson,
      "newBoxes" -> sbe.newBoxes.map(b => encoder.encode(b.id).asJson).toSeq.asJson,
      "boxesToRemove" -> sbe.boxIdsToOpen.map(id => encoder.encode(id).asJson).asJson,
      "from" -> sbe.from.map { s =>
        Map(
          "proposition" -> encoder.encode(s._1.pubKeyBytes).asJson,
          "nonce" -> s._2.toLong.asJson
        ).asJson
      }.asJson,
      "to" -> sbe.to.map { s =>
        Map(
          "proposition" -> encoder.encode(s._1.pubKeyBytes).asJson,
          "value" -> s._2.toLong.asJson
        ).asJson
      }.asJson,
      "signatures" -> sbe.signatures.map(s => encoder.encode(s.signature).asJson).asJson,
      "fee" -> sbe.fee.asJson,
      "timestamp" -> sbe.timestamp.asJson
    ).asJson

  def nonceFromDigest(digest: Array[Byte]): Nonce = Nonce @@ Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): SimpleBoxStructure = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Signature @@ Array[Byte]()))

    val undersigned = SimpleBoxStructure(fromPub, to, fakeSigs, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new SimpleBoxStructure(fromPub, to, sigs, fee, timestamp)
  }
}


object SimpleBoxStructureCompanion extends Serializer[SimpleBoxStructure] {

  override def toBytes(m: SimpleBoxStructure): Array[Byte] = {
    Bytes.concat(Longs.toByteArray(m.fee),
      Longs.toByteArray(m.timestamp),
      Ints.toByteArray(m.signatures.length),
      Ints.toByteArray(m.from.length),
      Ints.toByteArray(m.to.length),
      m.signatures.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b.bytes)),
      m.from.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.bytes, Longs.toByteArray(b._2))),
      m.to.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.bytes, Longs.toByteArray(b._2)))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[SimpleBoxStructure] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, 8))
    val timestamp = Longs.fromByteArray(bytes.slice(8, 16))
    val sigLength = Ints.fromByteArray(bytes.slice(16, 20))
    val fromLength = Ints.fromByteArray(bytes.slice(20, 24))
    val toLength = Ints.fromByteArray(bytes.slice(24, 28))
    val signatures = (0 until sigLength) map { i =>
      Signature25519(Signature @@ bytes.slice(28 + i * Curve25519.SignatureLength, 28 + (i + 1) * Curve25519.SignatureLength))
    }
    val s = 28 + sigLength * Curve25519.SignatureLength
    val elementLength = 8 + Curve25519.KeyLength
    val from = (0 until fromLength) map { i =>
      val pk = PublicKey @@ bytes.slice(s + i * elementLength, s + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s + (i + 1) * elementLength - 8, s + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), Nonce @@ v)
    }
    val s2 = s + fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = PublicKey @@ bytes.slice(s2 + i * elementLength, s2 + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s2 + (i + 1) * elementLength - 8, s2 + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), Value @@ v)
    }
    SimpleBoxStructure(from, to, signatures, fee, timestamp)
  }
}