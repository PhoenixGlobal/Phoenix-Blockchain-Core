/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: Identifier.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-23 下午2:17@version: 1.0
 */

package com.apex.core

import com.apex.crypto.UIntBase

trait Identifier[A <: UIntBase] extends com.apex.common.Serializable {
  protected var _id: A = _

  def id(): A = {
    if (_id == null) {
      _id = genId
    }
    _id
  }

  protected def genId(): A
}