package com.apex.vm.exceptions

case class OutOfBlockTimeException(message: String) extends BytecodeExecutionException(message)
