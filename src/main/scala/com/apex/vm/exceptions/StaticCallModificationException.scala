/*
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: StaticCallModificationException.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-11-30 上午10:43@version: 1.0
 *
 */

package com.apex.vm.exceptions

case class StaticCallModificationException(message: String)
  extends Exception(message) with BytecodeExecutionException