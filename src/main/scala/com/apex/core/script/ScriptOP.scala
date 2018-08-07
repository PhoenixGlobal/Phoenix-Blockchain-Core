/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ScriptOP.scala
 *
 * @author: shan.huang@chinapex.com: 2018-08-05 下午4:06@version: 1.0
 */

package com.apex.core.script

import com.apex.crypto.BinaryData

// @formatter:off
abstract class ScriptOP
case object OP_0 extends ScriptOP
case object OP_PUSHDATA1 extends ScriptOP
case object OP_PUSHDATA2 extends ScriptOP
case object OP_PUSHDATA4 extends ScriptOP
case object OP_1NEGATE extends ScriptOP
case object OP_RESERVED extends ScriptOP
case object OP_NOP extends ScriptOP
case object OP_VER extends ScriptOP
case object OP_IF extends ScriptOP
case object OP_NOTIF extends ScriptOP
case object OP_VERIF extends ScriptOP
case object OP_VERNOTIF extends ScriptOP
case object OP_ELSE extends ScriptOP
case object OP_ENDIF extends ScriptOP
case object OP_VERIFY extends ScriptOP
case object OP_RETURN extends ScriptOP
case object OP_DUP extends ScriptOP
case object OP_EQUAL extends ScriptOP
case object OP_EQUALVERIFY extends ScriptOP
case object OP_RIPEMD160 extends ScriptOP
case object OP_SHA1 extends ScriptOP
case object OP_SHA256 extends ScriptOP
case object OP_HASH160 extends ScriptOP
case object OP_HASH256 extends ScriptOP
case object OP_CODESEPARATOR extends ScriptOP
case object OP_CHECKSIG extends ScriptOP
case object OP_CHECKSIGVERIFY extends ScriptOP
case object OP_INVALIDOPCODE extends ScriptOP

object OP_PUSHDATA {
  def apply(data: BinaryData) = if (data.length < 0x4c) new OP_PUSHDATA(data, data.length)
    else if (data.length < 0xff) new OP_PUSHDATA(data, 0x4c)
    else if (data.length < 0xffff) new OP_PUSHDATA(data, 0x4d)
    else if (data.length < 0xffffffff) new OP_PUSHDATA(data, 0x4e)
    else throw new IllegalArgumentException(s"data is ${data.length}, too big for OP_PUSHDATA")

}
case class OP_PUSHDATA(data: BinaryData, code: Int) extends ScriptOP {
  override def toString = data.toString
}
case class OP_INVALID(code: Int) extends ScriptOP
// @formatter:off

object ScriptOP {
  // code -> ScriptOP
  val code2op: Map[Int, ScriptOP] = Map(
    0x00 -> OP_0,
    0x4c -> OP_PUSHDATA1,
    0x4d -> OP_PUSHDATA2,
    0x4e -> OP_PUSHDATA4,
    0x4f -> OP_1NEGATE,
    0x50 -> OP_RESERVED,
    0x61 -> OP_NOP,
    0x62 -> OP_VER,
    0x63 -> OP_IF,
    0x64 -> OP_NOTIF,
    0x65 -> OP_VERIF,
    0x66 -> OP_VERNOTIF,
    0x67 -> OP_ELSE,
    0x68 -> OP_ENDIF,
    0x69 -> OP_VERIFY,
    0x6a -> OP_RETURN,
    0x76 -> OP_DUP,
    0x87 -> OP_EQUAL,
    0x88 -> OP_EQUALVERIFY,
    0xa6 -> OP_RIPEMD160,
    0xa7 -> OP_SHA1,
    0xa8 -> OP_SHA256,
    0xa9 -> OP_HASH160,
    0xaa -> OP_HASH256,
    0xab -> OP_CODESEPARATOR,
    0xac -> OP_CHECKSIG,
    0xad -> OP_CHECKSIGVERIFY,
    0xff -> OP_INVALIDOPCODE)

  // ScriptOP -> code
  val op2code: Map[ScriptOP, Int] = code2op.map(_.swap)

  // name -> code
  val name2code = code2op.mapValues(_.asInstanceOf[Product].productPrefix.stripPrefix("OP_")).map(_.swap) + ("NOP2" -> 0xb1) + ("NOP3" -> 0xb2)
}
