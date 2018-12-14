package com.apex.common

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.fasterxml.jackson.databind.{ObjectMapper, SerializationFeature}
import javax.swing.text.AbstractDocument.Content

import scala.util.Try

trait Serializable {
  def serialize(os: DataOutputStream): Unit

  //  def deserialize[T <: Serializable](is: DataInputStream): T
}

object Serializable {

  var mapper: ObjectMapper = new ObjectMapper()
  mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false)

  def JsonMapperTo(any: Any): String = mapper.writeValueAsString(any)

  def JsonMapperFrom[T](content: String, valueType: Class[T]): T = {
    mapper.readValue(content, valueType)
  }


  implicit class Writer(val obj: Serializable) {
    def toBytes: Array[Byte] = {
      val bs = new ByteArrayOutputStream()
      val os = new DataOutputStream(bs)
      obj.serialize(os)
      bs.toByteArray
    }
  }

  implicit class Reader(val bytes: Array[Byte]) {
    def toInstance[A <: Serializable](deserializer: DataInputStream => A): A = {
      val bs = new ByteArrayInputStream(bytes)
      val is = new DataInputStream(bs)
      deserializer(is)
    }

    def toInstances[A <: Serializable](deserializer: DataInputStream => A): Seq[A] = {
      val bs = new ByteArrayInputStream(bytes)
      val is = new DataInputStream(bs)
      (1 to is.readVarInt) map (_ => deserializer(is))
    }
  }

  implicit class DataOutputStreamExtension(val os: DataOutputStream) {
    def writeVarInt(i: Int) = {
      Helper.writeVarint(i, os)
    }

    def writeByteArray(bytes: Array[Byte]) = {
      os.writeVarInt(bytes.size)
      os.write(bytes)
    }

    def writeString(str: String) = {
      os.writeByteArray(str.getBytes("UTF-8"))
    }

    def write[A <: Serializable](value: A) = {
      value.serialize(os)
    }

    def writeSeq[A <: Serializable](arr: Seq[A]) = {
      os.writeVarInt(arr.size)
      arr.foreach(_.serialize(os))
    }

    def writeMap[K <: Serializable, V <: Serializable](map: Map[K, V]) = {
      os.writeVarInt(map.size)
      map.foreach(o => {
        o._1.serialize(os)
        o._2.serialize(os)
      })
    }

    def writeMap[K, V](map: Map[K, V])(implicit kSerializer: (K, DataOutputStream) => Unit, vSerializer: (V, DataOutputStream) => Unit): Unit = {
      os.writeVarInt(map.size)
      map.foreach(o => {
        kSerializer(o._1, os)
        vSerializer(o._2, os)
      })
    }
  }

  implicit class DataInputStreamExtension(val is: DataInputStream) {
    def readVarInt(): Int = {
      Helper.varint(is).toInt
    }

    def readByteArray(): Array[Byte] = {
      val bytes = Array.fill(is.readVarInt)(0.toByte)
      is.read(bytes, 0, bytes.length)
      bytes
    }

    def readSeq[A](deserializer: DataInputStream => A): Seq[A] = {
      (1 to is.readVarInt) map (_ => deserializer(is))
    }

    def readMap[K, V](kDeserializer: DataInputStream => K,
                      vDeserializer: DataInputStream => V): Map[K, V] = {
      (1 to is.readVarInt) map (_ => kDeserializer(is) -> vDeserializer(is)) toMap
    }

    def readMap(): Map[Array[Byte], Array[Byte]] = {
      (1 to is.readVarInt) map (_ => is.readByteArray -> is.readByteArray) toMap
    }

//    def readObj[A](deserializer: DataInputStream => A): A = {
//      deserializer(is)
//    }

    def readObj[A](implicit deserializer: DataInputStream => A): A = {
      deserializer(is)
    }

    def readString(): String = {
      new String(is.readByteArray, "UTF-8")
    }
  }
}

trait BytesSerializable extends java.io.Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  def serializer: Serializer[M]
}

trait Serializer[M] {
  def toBytes(obj: M): Array[Byte]

  def parseBytes(bytes: Array[Byte]): Try[M]
}
