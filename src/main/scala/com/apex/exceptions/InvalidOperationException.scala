package com.apex.exceptions

case class InvalidOperationException(message: String = "invalid operation") extends Exception(message)