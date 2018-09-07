package com.apex.network

import java.net.{InetAddress, InetSocketAddress}

import com.apex.common.{BytesSerializable, Serializer}
import com.google.common.primitives.{Ints, Longs}
import com.apex.utils.{ApplicationVersionSerializer, Version}

import scala.util.Try


case class Handshake(applicationName: String,
                     protocolVersion: Version,
                     nodeName: String,
                     declaredAddress: Option[InetSocketAddress],
                     chainId: String,
                     headerNum: String,
                     time: Long) extends BytesSerializable {

  require(Option(applicationName).isDefined)
  require(Option(protocolVersion).isDefined)



  override type M = Handshake

  override def serializer: Serializer[Handshake] = HandshakeSerializer
}

object HandshakeSerializer extends Serializer[Handshake] {

  override def toBytes(obj: Handshake): Array[Byte] = {
    val anb = obj.applicationName.getBytes

    val fab = obj.declaredAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    val nodeNameBytes = obj.nodeName.getBytes

    val chainIdBytes = obj.chainId.getBytes

    val header_num = obj.headerNum.getBytes

    Array(anb.size.toByte) ++ anb ++
      obj.protocolVersion.bytes ++
      Array(nodeNameBytes.size.toByte) ++ nodeNameBytes ++ Array(chainIdBytes.size.toByte) ++ chainIdBytes ++
      Array(header_num.size.toByte) ++ header_num ++
      Ints.toByteArray(fab.length) ++ fab ++
      Longs.toByteArray(obj.time)

  }

  override def parseBytes(bytes: Array[Byte]): Try[Handshake] = Try {
    var position = 0
    val appNameSize = bytes.head
    require(appNameSize > 0)

    position += 1

    val an = new String(bytes.slice(position, position + appNameSize))
    position += appNameSize

    val av = ApplicationVersionSerializer.parseBytes(
      bytes.slice(position, position + ApplicationVersionSerializer.SerializedVersionLength)).get
    position += ApplicationVersionSerializer.SerializedVersionLength

    val nodeNameSize = bytes.slice(position, position + 1).head
    position += 1

    val nodeName = new String(bytes.slice(position, position + nodeNameSize))
    position += nodeNameSize

    val chainIdSize = bytes.slice(position, position + 1).head
    position += 1

    val chainId = new String(bytes.slice(position, position + chainIdSize))
    position += chainIdSize

    val headerNumSize = bytes.slice(position, position + 1).head
    position += 1

    val headerNum = new String(bytes.slice(position, position + headerNumSize))
    position += headerNumSize

    val fas = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt = if (fas > 0) {
      val fa = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, nodeName, isaOpt,chainId, headerNum, time)
  }
}