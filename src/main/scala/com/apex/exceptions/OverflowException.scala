package com.apex.exceptions

case class OverflowException (message: String = "overflow") extends Exception(message)
