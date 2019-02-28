package org.apex.vm

import java.util
import org.spongycastle.util.encoders.Hex

object BytecodeCompiler {
  private def isHexadecimal(token: String): Boolean = token.startsWith("0X")

  private def compileHexadecimal(token: String, bytecodes: util.List[Byte]): Unit = {
    val bytes: Array[Byte] = Hex.decode(token.substring(2))
    var k: Int = 0
    while (k < bytes.length) {
      bytecodes.add(bytes(k))
      k += 1
    }
  }

  def compile(code: String): Array[Byte] = compile(code.split("\\s+"))

  private def compile(tokens: Array[String]): Array[Byte] = {
    val bytecodes: util.List[Byte] = new util.ArrayList[Byte]
    val ntokens: Int = tokens.length
    var i: Int = 0
    while (i < ntokens) {
      val token: String = tokens(i).trim.toUpperCase
      if (token.isEmpty) {
        //continue
      }
      else {
        if (BytecodeCompiler.isHexadecimal(token))
          BytecodeCompiler.compileHexadecimal(token, bytecodes)
        else
          bytecodes.add(OpCode.byteVal(token))
      }
      i += 1
    }
    val nbytes: Int = bytecodes.size
    val bytes: Array[Byte] = new Array[Byte](nbytes)
    var k: Int = 0
    while (k < nbytes) {
      bytes(k) = bytecodes.get(k).byteValue
      k += 1
    }
    bytes
  }
}

