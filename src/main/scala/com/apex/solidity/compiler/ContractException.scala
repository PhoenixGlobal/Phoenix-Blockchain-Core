package com.apex.solidity.compiler


object ContractException {
  def permissionError(msg: String, args: Any*): ContractException = error("contract permission error", msg, args)

  def compilationError(msg: String, args: Any*): ContractException = error("contract compilation error", msg, args)

  def validationError(msg: String, args: Any*): ContractException = error("contract validation error", msg, args)

  def assembleError(msg: String, args: Any*): ContractException = error("contract assemble error", msg, args)

  private def error(title: String, message: String, args: Any*) = new ContractException(title + ": " + String.format(message, args))
}

class ContractException(val message: String) extends RuntimeException(message) {

}


