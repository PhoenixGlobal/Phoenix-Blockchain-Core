package com.apex.core.utils

import com.apex.crypto.{Base16, BytesEncoder}

trait ApexEncoding {
  implicit val encoder: BytesEncoder = Base16
}
