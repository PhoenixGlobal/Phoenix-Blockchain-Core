package com.apex.core.validation

trait ModifierError {
  def message: String
  def isFatal: Boolean
  def toThrowable: Throwable

  def info: String = {
    val fatality = if (isFatal) "fatally" else "recoverably"
    s"Modifier Validation failed $fatality: $message"
  }
}

case class MalformedModifierError(message: String) extends Exception(message) with ModifierError {
  def isFatal: Boolean = true
  def toThrowable: Throwable = this
}

case class RecoverableModifierError(message: String) extends Exception(message) with ModifierError {
  def isFatal: Boolean = false
  def toThrowable: Throwable = this
}


@SuppressWarnings(Array("org.wartremover.warts.Null"))
case class MultipleErrors(errors: Seq[ModifierError])
     extends Exception(errors.mkString(" | "), errors.headOption.map(_.toThrowable).orNull) {
  def isFatal: Boolean = errors.exists(_.isFatal)
}
