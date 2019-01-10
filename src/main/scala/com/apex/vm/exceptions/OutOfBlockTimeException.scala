package com.apex.vm.exceptions

case class OutOfBlockTimeException(message: String) extends Exception(message) with BytecodeExecutionException
