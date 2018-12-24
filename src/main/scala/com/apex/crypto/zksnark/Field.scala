package com.apex.crypto.zksnark

abstract class Field[T] {

  def add (o: T): T
  def mul (o: T): T
  def sub (o: T): T
  def squared: T
  def dbl: T
  def inverse: T
  def negate: T
  def isZero: Boolean
  def isValid: Boolean

}
