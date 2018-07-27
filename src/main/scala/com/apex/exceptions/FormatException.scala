package com.apex.exceptions

case class FormatException(message: String = "format error") extends Exception(message)