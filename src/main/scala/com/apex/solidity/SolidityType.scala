package com.apex.solidity

import com.apex.utils.ByteUtil
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.annotation.JsonValue
import com.apex.vm.DataWord
//import java.lang.reflect.Array
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import scala.collection.JavaConversions._

object SolidityType {

  @JsonCreator
  def getType(typeName: String): SolidityType = {
    if (typeName.contains("[")) return ArrayType.getType(typeName)
    if ("bool" == typeName) return new BoolType()
    if (typeName.startsWith("int")) return new IntType(typeName)
    if (typeName.startsWith("uint")) return new UnsignedIntType(typeName)
    if ("address" == typeName) return new AddressType()
    if ("string" == typeName) return new StringType()
    if ("bytes" == typeName) return new BytesType()
    if ("function" == typeName) return new FunctionType()
    if (typeName.startsWith("bytes")) return new Bytes32Type(typeName)

    throw new RuntimeException("Unknown type: " + typeName)
  }

  object ArrayType {

    def getType(typeName: String): ArrayType = {
      val idx1: Int = typeName.indexOf("[")
      val idx2: Int = typeName.indexOf("]", idx1)
      if (idx1 + 1 == idx2) {
        new DynamicArrayType(typeName)
      } else {
        new StaticArrayType(typeName)
      }
    }

  }

  abstract class ArrayType(name: String) extends SolidityType(name) {

    var elementType: SolidityType = SolidityType.getType(st + subDim)

    val idx: Int = name.indexOf("[")

    val st: String = name.substring(0, idx)

    val idx2: Int = name.indexOf("]", idx)

    val subDim: String =
      if (idx2 + 1 == name.length) "" else name.substring(idx2 + 1)

    override def encode(value: AnyRef): Array[Byte] =
      if (value.getClass.isArray) {
        val elems: List[Any] = new ArrayList[Any]()
        for (i <- 0 until java.lang.reflect.Array.getLength(value)) {
          elems.add(java.lang.reflect.Array.get(value, i))
        }
        encodeList(elems)
      } else if (value.isInstanceOf[List[_]]) {
        encodeList(value.asInstanceOf[List[_]])
      } else {
        throw new RuntimeException("List value expected for type " + getName)
      }

    override def getCanonicalName(): String = getArrayCanonicalName("")

    def getArrayCanonicalName(parentDimStr: String): String = {
      val myDimStr: String = parentDimStr + getCanonicalDimension
      if (getElementType.isInstanceOf[ArrayType]) {
        getElementType.asInstanceOf[ArrayType].getArrayCanonicalName(myDimStr)
      } else {
        getElementType.getCanonicalName + myDimStr
      }
    }

    protected def getCanonicalDimension(): String

    def getElementType(): SolidityType = elementType

    def encodeList(l: List[_]): Array[Byte]

  }

  class StaticArrayType(name: String) extends ArrayType(name) {

    var size: Int = java.lang.Integer.parseInt(dim)

    val idx1: Int = name.indexOf("[")

    val idx2_StaticArrayType: Int = name.indexOf("]", idx1)

    val dim: String = name.substring(idx1 + 1, idx2_StaticArrayType)

    override def getCanonicalName(): String =
      if (elementType.isInstanceOf[ArrayType]) {
        val elementTypeName: String = elementType.getCanonicalName
        val idx1: Int = elementTypeName.indexOf("[")
        elementTypeName.substring(0, idx1) + "[" + size + "]" +
          elementTypeName.substring(idx1)
      } else {
        elementType.getCanonicalName + "[" + size + "]"
      }

    protected override def getCanonicalDimension(): String = "[" + size + "]"

    override def encodeList(l: List[_]): Array[Byte] = {
      if (l.size != size)
        throw new RuntimeException("List size (" + l.size + ") != " + size + " for type " + getName)
      val elems: Array[Array[Byte]] = Array.ofDim[Array[Byte]](size)
      for (i <- 0 until l.size) {
        elems(i) = elementType.encode(l.get(i).asInstanceOf[AnyRef])
      }
      ByteUtil.merge(elems)
    }

    override def decode(encoded: Array[Byte], offset: Int): Array[Any] = {
      val result: Array[Any] = Array.ofDim[Any](size)
      for (i <- 0 until size) {
        result(i) =
          elementType.decode(encoded, offset + i * elementType.getFixedSize)
      }
      result
    }

    override def getFixedSize()
    : Int = // return negative if elementType is dynamic
      elementType.getFixedSize * size

  }

  class DynamicArrayType(name: String) extends ArrayType(name) {

    override def getCanonicalName(): String =
      if (elementType.isInstanceOf[ArrayType]) {
        val elementTypeName: String = elementType.getCanonicalName
        val idx1: Int = elementTypeName.indexOf("[")
        elementTypeName.substring(0, idx1) + "[]" + elementTypeName.substring(
          idx1)
      } else {
        elementType.getCanonicalName + "[]"
      }

    protected override def getCanonicalDimension(): String = "[]"

    override def encodeList(l: List[_]): Array[Byte] = {
      var elems: Array[Array[Byte]] = null
      if (elementType.isDynamicType) {
        elems = Array.ofDim[Array[Byte]](l.size * 2 + 1)
        elems(0) = IntType.encodeInt(l.size)
        var offset: Int = l.size * 32
        for (i <- 0 until l.size) {
          elems(i + 1) = IntType.encodeInt(offset)
          val encoded: Array[Byte] = elementType.encode(l.get(i).asInstanceOf[AnyRef])
          elems(l.size + i + 1) = encoded
          offset += 32 * ((encoded.length - 1) / 32 + 1)
        }
      } else {
        elems = Array.ofDim[Array[Byte]](l.size + 1)
        elems(0) = IntType.encodeInt(l.size)
        for (i <- 0 until l.size) {
          elems(i + 1) = elementType.encode(l.get(i).asInstanceOf[AnyRef])
        }
      }
      ByteUtil.merge(elems)
    }

    override def decode(encoded: Array[Byte], origOffset_input: Int): AnyRef = {
      var origOffset = origOffset_input
      val len: Int = IntType.decodeInt(encoded, origOffset).intValue()
      origOffset += 32
      var offset: Int = origOffset
      val ret: Array[Any] = Array.ofDim[Any](len)
      for (i <- 0 until len) {
        ret(i) =
          if (elementType.isDynamicType)
            elementType.decode(
              encoded,
              origOffset + IntType.decodeInt(encoded, offset).intValue())
          else elementType.decode(encoded, offset)
        offset += elementType.getFixedSize
      }
      ret
    }
    override def isDynamicType(): Boolean = true
  }

  class BytesType protected (name: String) extends SolidityType(name) {

    def this() =
      this(???) /* TODO: Scala does not allow multiple super constructor calls
     * Change this code to call a constructor of the current class instead.
     * For your convenience, here is the invalid super constructor call:
     * }super("bytes")
     */

    override def encode(value: AnyRef): Array[Byte] = {
      var bb: Array[Byte] = null
      if (value.isInstanceOf[Array[Byte]]) {
        bb = value.asInstanceOf[Array[Byte]]
      } else if (value.isInstanceOf[String]) {
        bb = value.asInstanceOf[String].getBytes
      } else {
        throw new RuntimeException(
          "byte[] or String value is expected for type 'bytes'")
      }
      // padding 32 bytes
      val ret: Array[Byte] = Array.ofDim[Byte](((bb.length - 1) / 32 + 1) * 32)
      System.arraycopy(bb, 0, ret, 0, bb.length)
      ByteUtil.merge(Array(IntType.encodeInt(bb.length), ret))
    }

    override def decode(encoded: Array[Byte], offset_input: Int): AnyRef = {
      var offset = offset_input
      val len: Int = IntType.decodeInt(encoded, offset).intValue()
      if (len == 0) Array.ofDim[Byte](0)
      offset += 32
      Arrays.copyOfRange(encoded, offset, offset + len)
    }
    override def isDynamicType(): Boolean = true
  }

  class StringType extends BytesType("string") {

    override def encode(value: AnyRef): Array[Byte] = {
      if (!(value.isInstanceOf[String]))
        throw new RuntimeException("String value expected for type 'string'")
      super.encode(value.asInstanceOf[String].getBytes(StandardCharsets.UTF_8))
    }
    override def decode(encoded: Array[Byte], offset: Int): AnyRef =
      new String(super.decode(encoded, offset).asInstanceOf[Array[Byte]],
        StandardCharsets.UTF_8)
  }

  object Bytes32Type {

    def decodeBytes32(encoded: Array[Byte], offset: Int): Array[Byte] =
      Arrays.copyOfRange(encoded, offset, offset + 32)
  }

  class Bytes32Type(s: String) extends SolidityType(s) {

    override def encode(value: AnyRef): Array[Byte] = {
      if (value.isInstanceOf[Number]) {
        val bigInt: BigInteger = new BigInteger(value.toString)
        IntType.encodeInt(bigInt)
      } else if (value.isInstanceOf[String]) {
        val ret: Array[Byte] = Array.ofDim[Byte](32)
        val bytes: Array[Byte] =
          value.asInstanceOf[String].getBytes(StandardCharsets.UTF_8)
        System.arraycopy(bytes, 0, ret, 0, bytes.length)
        ret
      } else if (value.isInstanceOf[Array[Byte]]) {
        val bytes: Array[Byte] = value.asInstanceOf[Array[Byte]]
        val ret: Array[Byte] = Array.ofDim[Byte](32)
        System.arraycopy(bytes, 0, ret, 32 - bytes.length, bytes.length)
        ret
      }
      throw new RuntimeException("Can't encode java type " + value.getClass + " to bytes32")
    }

    override def decode(encoded: Array[Byte], offset: Int): AnyRef =
      Bytes32Type.decodeBytes32(encoded, offset)
  }

  class AddressType extends IntType("address") {

    override def encode(value_input: AnyRef): Array[Byte] = {
      var value = value_input
      if (value.isInstanceOf[String] && !value
        .asInstanceOf[String]
        .startsWith("0x")) {
        // address is supposed to be always in hex
        value = "0x" + value
      }
      val addr: Array[Byte] = super.encode(value)
      for (i <- 0.until(12) if addr(i) != 0) {
        throw new RuntimeException(
          "Invalid address (should be 20 bytes length): " + ByteUtil.toHexString(addr))
      }
      addr
    }

    override def decode(encoded: Array[Byte], offset: Int): AnyRef = {
      val bi: BigInteger =
        super.decode(encoded, offset).asInstanceOf[BigInteger]
      ByteUtil.bigIntegerToBytes(bi, 20)
    }
  }

  abstract class NumericType(name: String) extends SolidityType(name) {

    def encodeInternal(value: AnyRef): BigInteger = {
      var bigInt: BigInteger = null
      if (value.isInstanceOf[String]) {
        var s: String = value.asInstanceOf[String].toLowerCase().trim()
        var radix: Int = 10
        if (s.startsWith("0x")) {
          s = s.substring(2)
          radix = 16
        } else if (s.contains("a") || s.contains("b") || s.contains("c") ||
          s.contains("d") ||
          s.contains("e") ||
          s.contains("f")) {
          radix = 16
        }
        bigInt = new BigInteger(s, radix)
      } else if (value.isInstanceOf[BigInteger]) {
        bigInt = value.asInstanceOf[BigInteger]
      } else if (value.isInstanceOf[Number]) {
        bigInt = new BigInteger(value.toString)
      } else if (value.isInstanceOf[Array[Byte]]) {
        bigInt = ByteUtil.bytesToBigInteger(value.asInstanceOf[Array[Byte]])
      } else {
        throw new RuntimeException(
          "Invalid value for type '" + this + "': " + value + " (" +
            value.getClass +
            ")")
      }
      bigInt
    }
  }

  object IntType {

    def decodeInt(encoded: Array[Byte], offset: Int): BigInteger =
      new BigInteger(Arrays.copyOfRange(encoded, offset, offset + 32))

    def encodeInt(i: Int): Array[Byte] = encodeInt(new BigInteger("" + i))

    def encodeInt(bigInt: BigInteger): Array[Byte] =
      ByteUtil.bigIntegerToBytesSigned(bigInt, 32)

  }

  class IntType(name: String) extends NumericType(name) {

    override def getCanonicalName(): String = {
      if (getName.==("int")) "int256"
      super.getCanonicalName
    }

    override def decode(encoded: Array[Byte], offset: Int): AnyRef =
      IntType.decodeInt(encoded, offset)

    override def encode(value: AnyRef): Array[Byte] = {
      val bigInt: BigInteger = encodeInternal(value)
      IntType.encodeInt(bigInt)
    }

  }

  object UnsignedIntType {

    def decodeInt(encoded: Array[Byte], offset: Int): BigInteger =
      new BigInteger(1, Arrays.copyOfRange(encoded, offset, offset + 32))

    def encodeInt(i: Int): Array[Byte] = encodeInt(new BigInteger("" + i))

    def encodeInt(bigInt: BigInteger): Array[Byte] = {
      if (bigInt.signum() == -1) {
        throw new RuntimeException("Wrong value for uint type: " + bigInt)
      }
      ByteUtil.bigIntegerToBytes(bigInt, 32)
    }

  }

  class UnsignedIntType(name: String) extends NumericType(name) {

    override def getCanonicalName(): String = {
      if (getName.==("uint")) "uint256"
      super.getCanonicalName
    }

    override def encode(value: AnyRef): Array[Byte] = {
      val bigInt: BigInteger = encodeInternal(value)
      UnsignedIntType.encodeInt(bigInt)
    }

    override def decode(encoded: Array[Byte], offset: Int): AnyRef =
      UnsignedIntType.decodeInt(encoded, offset)

  }

  class BoolType extends IntType("bool") {

    override def encode(value: AnyRef): Array[Byte] = {
      if (!(value.isInstanceOf[java.lang.Boolean]))
        throw new RuntimeException("Wrong value for bool type: " + value)
      super.encode(if (value == true) 1.asInstanceOf[AnyRef] else 0.asInstanceOf[AnyRef])
    }

    override def decode(encoded: Array[Byte], offset: Int): AnyRef =
      java.lang.Boolean.valueOf(
        super.decode(encoded, offset).asInstanceOf[Number].intValue() !=
          0)

  }

  class FunctionType extends Bytes32Type("function") {

    override def encode(value: AnyRef): Array[Byte] = {
      if (!(value.isInstanceOf[Array[Byte]]))
        throw new RuntimeException("Expected byte[] value for FunctionType")
      if (value.asInstanceOf[Array[Byte]].length != 24)
        throw new RuntimeException("Expected byte[24] for FunctionType")
      super.encode(
        ByteUtil.merge(Array(value.asInstanceOf[Array[Byte]], Array.ofDim[Byte](8))))
    }
  }
}

abstract class SolidityType(protected var name: String) {

  /**
    * The type name as it was specified in the interface description
    */
  def getName(): String = name

  /**
    * The canonical type name (used for the method signature creation)
    * E.g. 'int' - canonical 'int256'
    */
  @JsonValue
  def getCanonicalName(): String = getName

  /**
    * Encodes the value according to specific type rules
    *
    * @param value
    */
  def encode(value: AnyRef): Array[Byte]

  def decode(encoded: Array[Byte], offset: Int): AnyRef

  def decode(encoded: Array[Byte]): AnyRef = decode(encoded, 0)

  /**
    * @return fixed size in bytes. For the dynamic types returns IntType.getFixedSize()
    * which is effectively the int offset to dynamic data
    */
  def getFixedSize(): Int = 32

  def isDynamicType(): Boolean = false

  override def toString(): String = getName

}

