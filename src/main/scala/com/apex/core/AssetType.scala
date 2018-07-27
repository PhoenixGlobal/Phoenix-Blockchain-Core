package com.apex.core

object AssetType extends Enumeration {
  val Token = Value(0x01)

  implicit class Extension(val value: AssetType.Value) {
    def toByte: Byte = value.id.toByte
  }
}