/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: IllegalOperationException.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 上午10:43@version: 1.0
 *
 */

package com.apex.vm.exceptions

import com.apex.vm._

case class IllegalOperationException(opCode: Byte*) extends Exception(s"Invalid operation code: opCode[${opCode(0).toHex}]") with BytecodeExecutionException
