/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: UnExpectedError.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-3 下午7:30@version: 1.0
 */

package com.apex.exceptions

case class UnExpectedError (message: String = "unexpected error") extends Exception(message)