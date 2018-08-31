/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Script.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-05 下午4:06@version: 1.0
 */

package com.apex.core.script

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import com.apex.crypto.{BinaryData, Ecdsa, UInt160}
import com.apex.common.Helper._
import com.apex.core.Transaction

object Script {

  import com.apex.core.script.ScriptOP._

  type Stack = List[Seq[Byte]]

  private val True = Seq(1: Byte)

  private val False = Seq.empty[Byte]

  @tailrec
  def parse(input: InputStream, stack: collection.immutable.Vector[ScriptOP] = Vector.empty[ScriptOP]): List[ScriptOP] = {
    val code = input.read()
    code match {
      case -1 => stack.toList
      case 0 => parse(input, stack :+ OP_0)
      case opCode if opCode > 0 && opCode < 0x4c => parse(input, stack :+ OP_PUSHDATA(bytes(input, opCode), opCode))
      case 0x4c => parse(input, stack :+ OP_PUSHDATA(bytes(input, uint8(input)), 0x4c))
      case 0x4d => parse(input, stack :+ OP_PUSHDATA(bytes(input, uint16(input)), 0x4d))
      case 0x4e => parse(input, stack :+ OP_PUSHDATA(bytes(input, uint32(input)), 0x4e))
      case opCode if code2op.contains(opCode) => parse(input, stack :+ code2op(opCode))
      case opCode => parse(input, stack :+ OP_INVALID(opCode))
    }
  }

  def parse(blob: BinaryData): List[ScriptOP] = if (blob.length > 10000) throw new RuntimeException("script is too large") else parse(new ByteArrayInputStream(blob))

  def write(script: Seq[ScriptOP], out: OutputStream): Unit = script match {
    case Nil => ()
    case OP_PUSHDATA(data, length) :: tail if data.length < 0x4c && data.length == length => out.write(data.length); out.write(data); write(tail, out)
    case OP_PUSHDATA(data, 0x4c) :: tail if data.length < 0xff => writeUInt8(0x4c, out); writeUInt8(data.length, out); out.write(data); write(tail, out)
    case OP_PUSHDATA(data, 0x4d) :: tail if data.length < 0xffff => writeUInt8(0x4d, out); writeUInt16(data.length, out); out.write(data); write(tail, out)
    case OP_PUSHDATA(data, 0x4e) :: tail if data.length < 0xffffffff => writeUInt8(0x4e, out); writeUInt32(data.length, out); out.write(data); write(tail, out)
    case op@OP_PUSHDATA(data, code) :: tail => throw new RuntimeException(s"invalid element $op")
    case head :: tail => out.write(op2code(head)); write(tail, out)
  }

  def write(script: Seq[ScriptOP]): BinaryData = {
    val out = new ByteArrayOutputStream()
    write(script, out)
    out.toByteArray
  }

  def cost(op: ScriptOP): Int = op match {
    case OP_PUSHDATA(_, _) => 0
    case OP_RESERVED => 0
    case _ => 1
  }

  def castToBoolean(input: Seq[Byte]): Boolean = input.reverse.toList match {
    case head :: tail if head == 0x80.toByte && tail.forall(_ == 0) => false
    case something if something.exists(_ != 0) => true
    case _ => false
  }

  def removeSignature(script: List[ScriptOP], signature: BinaryData): List[ScriptOP] = {
    val toRemove = OP_PUSHDATA(signature)
    script.filterNot(_ == toRemove)
  }

  def removeSignatures(script: List[ScriptOP], sigs: List[BinaryData]): List[ScriptOP] = sigs.foldLeft(script)(removeSignature)

  def execute(tx: Transaction, index: Int, scriptSig: BinaryData, scriptPubKey: BinaryData): Boolean = {
    new Script.Runner(new Script.Context(tx, index)).verifyScripts(scriptSig, scriptPubKey)
  }

  case class Context(tx: Transaction, inputIndex: Int  /*, amount: Satoshi */) {
    //require(inputIndex >= 0 && inputIndex < tx.txIn.length, "invalid input index")
  }

  object Runner {

    case class State(conditions: List[Boolean], altstack: Stack, opCount: Int, scriptCode: List[ScriptOP])

    type Callback = (List[ScriptOP], Stack, State) => Boolean
  }

  class Runner(context: Context, callback: Option[Runner.Callback] = None) {

    import Runner._

    def checkSignature(pubKey: Seq[Byte], sigBytes: Seq[Byte], scriptCode: Seq[Byte]): Boolean = {
      if (sigBytes.isEmpty) false
      else {
        val sigHashFlags = sigBytes.last & 0xff
        val sigBytes1 = sigBytes.take(sigBytes.length - 1)
        if (sigBytes1.isEmpty) false
        else {
          val result = com.apex.crypto.Crypto.verifySignature(context.tx.dataForSigning(), sigBytes.toArray, pubKey.toArray)
          result
        }
      }
    }

    def run(script: BinaryData): Stack = run(parse(script))

    def run(script: List[ScriptOP]): Stack = run(script, List.empty[Seq[Byte]])

    def run(script: BinaryData, stack: Stack): Stack = run(parse(script), stack)

    def run(script: List[ScriptOP], stack: Stack): Stack =
      run(script, stack, State(conditions = List.empty[Boolean], altstack = List.empty[Seq[Byte]], opCount = 0, scriptCode = script))

    @tailrec
    final def run(script: List[ScriptOP], stack: Stack, state: State): Stack = {
      import state._
      val MaxScriptElementSize = 520
      callback.map(f => f(script, stack, state))
      if ((stack.length + altstack.length) > 1000) throw new RuntimeException(s"stack is too large: stack size = ${stack.length} alt stack size = ${altstack.length}")
      if (opCount > 201) throw new RuntimeException("operation count is over the limit")
      script match {
        case Nil if conditions.nonEmpty => throw new RuntimeException("IF/ENDIF imbalance")
        case Nil => stack
        case OP_VERIF :: _ => throw new RuntimeException("OP_VERIF is always invalid")
        case OP_VERNOTIF :: _ => throw new RuntimeException("OP_VERNOTIF is always invalid")
        case OP_PUSHDATA(data, _) :: _ if data.size > MaxScriptElementSize => throw new RuntimeException("Push value size limit exceeded")
        case OP_ELSE :: tail => run(tail, stack, state.copy(conditions = !conditions.head :: conditions.tail, opCount = opCount + 1))
        case OP_ENDIF :: tail => run(tail, stack, state.copy(conditions = conditions.tail, opCount = opCount + 1))
        case head :: tail if conditions.contains(false) => run(tail, stack, state.copy(opCount = opCount + cost(head)))
        case OP_0 :: tail => run(tail, Seq.empty[Byte] :: stack, state)
        case OP_NOP :: tail => run(tail, stack, state.copy(opCount = opCount + 1))
        case OP_CHECKSIG :: tail => stack match {
          case pubKey :: sigBytes :: stacktail => {
            val scriptCode1 = if (true) {
              val scriptCode1 = removeSignature(scriptCode, sigBytes)
              if (scriptCode1.length != scriptCode.length)
                throw new RuntimeException("Signature is found in scriptCode")
              scriptCode1
            } else scriptCode
            val success = checkSignature(pubKey, sigBytes, Script.write(scriptCode1))
            if (!success) {
              require(sigBytes.isEmpty, "Signature must be zero for failed CHECKSIG operation")
            }
            run(tail, (if (success) True else False) :: stacktail, state.copy(opCount = opCount + 1))
          }
          case _ => throw new RuntimeException("Cannot perform OP_CHECKSIG on a stack with less than 2 elements")
        }
        case OP_CHECKSIGVERIFY :: tail => run(OP_CHECKSIG :: OP_VERIFY :: tail, stack, state.copy(opCount = opCount - 1))
        case OP_CODESEPARATOR :: tail => run(tail, stack, state.copy(opCount = opCount + 1, scriptCode = tail))
        case OP_DUP :: tail => run(tail, stack.head :: stack, state.copy(opCount = opCount + 1))
        case OP_EQUAL :: tail => stack match {
          case a :: b :: stacktail if a != b => run(tail, False :: stacktail, state.copy(opCount = opCount + 1))
          case a :: b :: stacktail => run(tail, True :: stacktail, state.copy(opCount = opCount + 1))
          case _ => throw new RuntimeException("Cannot perform OP_EQUAL on a stack with less than 2 elements")
        }
        case OP_EQUALVERIFY :: tail => stack match {
          case a :: b :: _ if a != b => throw new RuntimeException("OP_EQUALVERIFY failed: elements are different")
          case a :: b :: stacktail => run(tail, stacktail, state.copy(opCount = opCount + 1))
          case _ => throw new RuntimeException("Cannot perform OP_EQUALVERIFY on a stack with less than 2 elements")
        }
        case OP_HASH160 :: tail => run(tail, Ecdsa.hash160(stack.head) :: stack.tail, state.copy(opCount = opCount + 1))
        case OP_HASH256 :: tail => run(tail, Ecdsa.hash256(stack.head) :: stack.tail, state.copy(opCount = opCount + 1))
        case OP_PUSHDATA(data, _) :: tail => run(tail, data.toSeq :: stack, state)
        case OP_RIPEMD160 :: tail => run(tail, Ecdsa.ripemd160(stack.head) :: stack.tail, state.copy(opCount = opCount + 1))
        case OP_SHA1 :: tail => run(tail, Ecdsa.sha1(stack.head) :: stack.tail, state.copy(opCount = opCount + 1))
        case OP_SHA256 :: tail => run(tail, Ecdsa.sha256(stack.head) :: stack.tail, state.copy(opCount = opCount + 1))
        case OP_VERIFY :: tail => stack match {
          case Nil => throw new RuntimeException("cannot run OP_VERIFY on an empty stack")
          case head :: _ if !castToBoolean(head) => throw new RuntimeException("OP_VERIFY failed")
          case _ :: stacktail => run(tail, stacktail, state.copy(opCount = opCount + 1))
        }
      }
    }

    def verifyScripts(scriptSig: BinaryData, scriptPubKey: BinaryData): Boolean = {
      def checkStack(stack: Stack): Boolean = {
        if (stack.isEmpty) false
        else if (!Script.castToBoolean(stack.head)) false
        //else if ((scriptFlag & SCRIPT_VERIFY_CLEANSTACK) != 0) {
        //  stack.size == 1
        //}
        else true
      }

      val ssig = Script.parse(scriptSig)
      val stack = run(ssig)

      val spub = Script.parse(scriptPubKey)
      val stack0 = run(spub, stack)
      require(stack0.nonEmpty, "Script verification failed, stack should not be empty")
      require(castToBoolean(stack0.head), "Script verification failed, stack starts with 'false'")

      checkStack(stack0)
    }
  }

  def publicKeyHash(script: List[ScriptOP]): Array[Byte] = script match {
    case OP_DUP :: OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: OP_NOP :: Nil => data // non standard pay to pubkey...
    case OP_DUP :: OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil => data // standard pay to pubkey
    case OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUAL :: Nil if data.size == 20 => data // standard pay to script
  }

  def publicKeyHash(script: Array[Byte]): Array[Byte] = publicKeyHash(parse(script))

  def pay2pkh(pubKeyHash: BinaryData): Seq[ScriptOP] = {
    require(pubKeyHash.length == 20, "pubkey hash length must be 20 bytes")
    OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubKeyHash) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
  }

  def pay2pkh(pubKey: Ecdsa.PublicKey): Seq[ScriptOP] = pay2pkh(pubKey.pubKeyHash)

  def pay2pkh(pubKey: UInt160): Seq[ScriptOP] = pay2pkh(pubKey.data)

}
