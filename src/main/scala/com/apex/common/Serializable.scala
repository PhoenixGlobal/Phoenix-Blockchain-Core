package com.apex.common

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import scala.util.Try

trait Serializable {
  def serialize(os: DataOutputStream): Unit

  //  def deserialize[T <: Serializable](is: DataInputStream): T
}

object Serializable {

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
      (1 to is.readInt) map (_ => deserializer(is))
    }
  }

  implicit class DataOutputStreamExtension(val os: DataOutputStream) {
    def writeByteArray(bytes: Array[Byte]) = {
      os.writeInt(bytes.length)
      os.write(bytes)
    }

    def writeString(str: String) = {
      os.writeByteArray(str.getBytes("UTF-8"))
    }

    def write[A <: Serializable](value: A) = {
      value.serialize(os)
    }

    def writeSeq[A <: Serializable](arr: Seq[A]) = {
      os.writeInt(arr.length)
      arr.foreach(_.serialize(os))
    }

    def writeMap[K <: Serializable, V <: Serializable](map: Map[K, V]) = {
      os.writeInt(map.size)
      map.foreach(o => {
        o._1.serialize(os)
        o._2.serialize(os)
      })
    }
  }

  implicit class DataInputStreamExtension(val is: DataInputStream) {
    def readByteArray(): Array[Byte] = {
      val bytes = Array.fill(is.readInt())(0.toByte)
      is.read(bytes, 0, bytes.length)
      bytes
    }

    def readSeq[A <: Serializable](deserializer: DataInputStream => A): Seq[A] = {
      (1 to is.readInt) map (_ => deserializer(is))
    }

    def readMap[K <: Serializable, V <: Serializable](kDeserializer: DataInputStream => K,
                                                      vDeserializer: DataInputStream => V): Map[K, V] = {
      (1 to is.readInt) map (_ => kDeserializer(is) -> vDeserializer(is)) toMap
    }

    def readObj[A <: Serializable](deserializer: DataInputStream => A): A = {
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
