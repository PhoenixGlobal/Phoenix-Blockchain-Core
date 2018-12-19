package com.apex.solidity

import java.io.IOException
import java.lang.String.format
import java.util
import java.util.{ArrayList, List}

import com.apex.utils.ByteUtil
import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonCreator
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.JsonProcessingException

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import org.apache.commons.collections4.ListUtils.select
import org.apache.commons.collections4.{CollectionUtils, Predicate}
import org.apache.commons.lang3.ArrayUtils.subarray
import org.apache.commons.lang3.StringUtils.{join, stripEnd}
import com.apex.crypto.Crypto.sha3
import com.apex.solidity.Abi.Entry.Param

import com.apex.solidity.SolidityType.IntType.decodeInt
import com.apex.solidity.SolidityType.IntType.encodeInt

object Abi {
  private val DEFAULT_MAPPER: ObjectMapper =
    new ObjectMapper() with ScalaObjectMapper
  //new ObjectMapper().disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES).enable(DeserializationFeature.READ_UNKNOWN_ENUM_VALUES_AS_NULL)

  DEFAULT_MAPPER.registerModule(DefaultScalaModule)

  def fromJson(json: String): Abi = try
    DEFAULT_MAPPER.readValue(json, classOf[Abi])
    //DEFAULT_MAPPER.readValue[Abi](json)
  catch {
    case e: IOException =>
      throw new RuntimeException(e)
  }

  object Entry {

    object EntryType extends Enumeration {
      val constructor = Value(0)
      val function = Value(1)
      val event = Value(2)
      val fallback = Value(3)
    }

    object Param {
      def decodeList(params: Seq[Entry.Param], encoded: Array[Byte]): util.List[AnyRef] = {
        val result: util.List[AnyRef] = new util.ArrayList[AnyRef](params.size)
        var offset: Int = 0
        import scala.collection.JavaConversions._
        for (param <- params) {
          val decoded: AnyRef =
            if (param.solidityType.isDynamicType)
              param.solidityType.decode(encoded, decodeInt(encoded, offset).intValue)
            else
              param.solidityType.decode(encoded, offset)
          result.add(decoded)
          offset += param.solidityType.getFixedSize
        }
        result
      }
    }

    @JsonInclude(Include.NON_NULL)
    class Param {
      var indexed = false
      var name: String = null
      var solidityType: SolidityType = null

      override def toString: String = format("%s%s%s", solidityType.getCanonicalName, if (indexed != null && indexed) " indexed " else " ", name)
    }

    @JsonCreator
    def create(@JsonProperty("anonymous") anonymous: Boolean,
               @JsonProperty("constant") constant: Boolean,
               @JsonProperty("name") name: String,
               @JsonProperty("inputs") inputs: Seq[Entry.Param],
               @JsonProperty("outputs") outputs: Seq[Entry.Param],
               @JsonProperty("type") entryType: EntryType.Value,
               @JsonProperty(value = "payable", required = false, defaultValue = "false") payable: Boolean): Abi.Entry = {
      var result: Abi.Entry = null
      entryType match {
        case EntryType.constructor =>
          result = new Abi.Constructor(inputs, outputs)
        case EntryType.function =>
          result = new Abi.Function(Some(constant), name, inputs, outputs, payable)
        case EntryType.fallback =>
          result = new Abi.Function(Some(constant), name, inputs, outputs, payable)
        case EntryType.event =>
          result = new Abi.Event(Some(anonymous), name, inputs, outputs)
      }
      result
    }
  }

  @JsonInclude(Include.NON_NULL)
  abstract class Entry(val anonymous: Option[Boolean],
                       val constant: Option[Boolean],
                       val name: String,
                       val inputs: Seq[Entry.Param],
                       val outputs: Seq[Entry.Param],
                       val entryType: Entry.EntryType.Value,
                       val payable: Boolean) {
    def formatSignature: String = {
      val paramsTypes = new StringBuilder
      import scala.collection.JavaConversions._
      for (param <- inputs) {
        paramsTypes.append(param.solidityType.getCanonicalName).append(",")
      }
      format("%s(%s)", name, stripEnd(paramsTypes.toString, ","))
    }

    def fingerprintSignature: Array[Byte] = sha3(formatSignature.getBytes)

    def encodeSignature: Array[Byte] = fingerprintSignature
  }

  class Constructor(override val inputs: Seq[Entry.Param], override val outputs: Seq[Entry.Param])    //  util.List
    extends Abi.Entry(null, null, "", inputs, outputs, Entry.EntryType.constructor, false) {

    def decode(encoded: Array[Byte]): util.List[_] = Param.decodeList(inputs, encoded)

    def formatSignature(contractName: String): String = format("function %s(%s)", contractName, join(inputs, ", "))
  }

  object Function {
    private val ENCODED_SIGN_LENGTH: Int = 4

    def extractSignature(data: Array[Byte]): Array[Byte] = subarray(data, 0, ENCODED_SIGN_LENGTH)
  }

  class Function(override val constant: Option[Boolean],
                 override val name: String,
                 override val inputs: Seq[Entry.Param],
                 override val outputs: Seq[Entry.Param],
                 override val payable: Boolean)
    extends Abi.Entry(null, constant, name, inputs, outputs, Entry.EntryType.function, payable) {

    def encode(args: Any*): Array[Byte] = ByteUtil.merge(Array(encodeSignature, encodeArguments(args)))

    private def encodeArguments(args: AnyRef*): Array[Byte] = {
      if (args.length > inputs.size)
        throw new RuntimeException("Too many arguments: " + args.length + " > " + inputs.size)
      var staticSize: Int = 0
      var dynamicCnt: Int = 0
      // calculating static size and number of dynamic params
      var i: Int = 0
      while (i < args.length) {
        val solidityType: SolidityType = inputs(i).solidityType   //inputs.get(i).solidityType
        if (solidityType.isDynamicType) dynamicCnt += 1
        staticSize += solidityType.getFixedSize
        i += 1
      }
      val bb: Array[Array[Byte]] = new Array[Array[Byte]](args.length + dynamicCnt)
      var curDynamicPtr: Int = staticSize
      var curDynamicCnt: Int = 0
      i = 0
      while (i < args.length) {
        val solidityType: SolidityType = inputs(i).solidityType   //.get(i).solidityType
        if (solidityType.isDynamicType) {
          val dynBB: Array[Byte] = solidityType.encode(args(i))
          bb(i) = encodeInt(curDynamicPtr)
          bb(args.length + curDynamicCnt) = dynBB
          curDynamicCnt += 1
          curDynamicPtr += dynBB.length
        }
        else
          bb(i) = solidityType.encode(args(i))
        i += 1
      }
      ByteUtil.merge(bb)
    }

    def decode(encoded: Array[Byte]): util.List[_] = Param.decodeList(inputs, subarray(encoded, Function.ENCODED_SIGN_LENGTH, encoded.length))

    def decodeResult(encoded: Array[Byte]): util.List[_] = Param.decodeList(outputs, encoded)

    override def encodeSignature: Array[Byte] = Function.extractSignature(super.encodeSignature)

    override def toString: String = {
      var returnTail: String = ""
      if (constant.get)
        returnTail += " constant"
      if (!outputs.isEmpty) {
        val types: util.List[String] = new util.ArrayList[String]
        import scala.collection.JavaConversions._
        for (output <- outputs) {
          types.add(output.solidityType.getCanonicalName)
        }
        returnTail += format(" returns(%s)", join(types, ", "))
      }
      format("function %s(%s)%s;", name, join(inputs, ", "), returnTail)
    }
  }

  class Event(override val anonymous: Option[Boolean],
              override val name: String,
              override val inputs: Seq[Entry.Param],
              override val outputs: Seq[Entry.Param])
    extends Abi.Entry(anonymous, None, name, inputs, outputs, Entry.EntryType.event, false) {

    def decode(data: Array[Byte], topics: Array[Array[Byte]]): util.List[_] = {
      val result: util.List[AnyRef] = new util.ArrayList[AnyRef](inputs.size)
      val argTopics: Array[Array[Byte]] = if (anonymous.get) topics else subarray(topics, 1, topics.length)
      val indexedParams = filteredInputs(true)
      val indexed: util.List[AnyRef] = new util.ArrayList[AnyRef]
      var i: Int = 0
      while (i < indexedParams.size) {
        var decodedTopic: AnyRef = null
        if (indexedParams(i).solidityType.isDynamicType) { // If arrays (including string and bytes) are used as indexed arguments,
          // the Keccak-256 hash of it is stored as topic instead.
          decodedTopic = SolidityType.Bytes32Type.decodeBytes32(argTopics(i), 0)
        }
        else
          decodedTopic = indexedParams(i).solidityType.decode(argTopics(i))
        indexed.add(decodedTopic)
        i += 1
      }
      val notIndexed: util.List[AnyRef] = Param.decodeList(filteredInputs(false), data)
      import scala.collection.JavaConversions._
      for (input <- inputs) {
        result.add(if (input.indexed) indexed.remove(0)
        else notIndexed.remove(0))
      }
      result
    }

    private def filteredInputs(indexed: Boolean): Seq[Entry.Param] = {
      //select(inputs, (param: Entry.Param) => param.indexed eq indexed)
      inputs.filter(p => p.indexed == indexed)
    }

    override def toString: String = format("event %s(%s);", name, join(inputs, ", "))
  }

}

class Abi() extends util.ArrayList[Abi.Entry] {
  def toJson: String = try
    new ObjectMapper().writeValueAsString(this)
  catch {
    case e: JsonProcessingException =>
      throw new RuntimeException(e)
  }

//  private def find[T <: Abi.Entry](resultClass: Class[T], entryType: Abi.Entry.EntryType.Value, searchPredicate: Predicate[T]): T = {
//    CollectionUtils.find(this, (entry: Abi.Entry) => (entry.`type` eq `type`) && searchPredicate.evaluate(entry.asInstanceOf[T])).asInstanceOf[T]
//  }
//
//  def findFunction(searchPredicate: Predicate[Abi.Function]): Abi.Function = {
//    find(classOf[Abi.Function], Abi.Entry.EntryType.function, searchPredicate)
//  }
//
//  def findEvent(searchPredicate: Predicate[Abi.Event]): Abi.Event = {
//    find(classOf[Abi.Event], Abi.Entry.EntryType.event, searchPredicate)
//  }
//
//  def findConstructor: Abi.Constructor = {
//    find(classOf[Abi.Constructor], Abi.Entry.EntryType.constructor, (`object`: Abi.Constructor) => true)
//  }

  override def toString: String = toJson
}


