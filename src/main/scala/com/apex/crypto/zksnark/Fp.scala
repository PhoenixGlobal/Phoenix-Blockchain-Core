package com.apex.crypto.zksnark

import java.math.BigInteger
import com.apex.crypto.zksnark.Params.P

object Fp {
  private[zksnark] val ZERO = new Fp(BigInteger.ZERO)
  private[zksnark] val _1 = new Fp(BigInteger.ONE)
  private[zksnark] val NON_RESIDUE = new Fp(new BigInteger("21888242871839275222246405745257275088696311157297823662689037894645226208582"))
  private[zksnark] val _2_INV = new Fp(BigInteger.valueOf(2).modInverse(P))

  private[zksnark] def create(v: Array[Byte]) = new Fp(new BigInteger(1, v))

  private[zksnark] def create(v: BigInteger) = new Fp(v)
}

class Fp private[zksnark](var v: BigInteger) extends Field[Fp] {
  override def add(o: Fp) = new Fp(this.v.add(o.v).mod(P))

  override def mul(o: Fp) = new Fp(this.v.multiply(o.v).mod(P))

  override def sub(o: Fp) = new Fp(this.v.subtract(o.v).mod(P))

  override def squared = new Fp(v.multiply(v).mod(P))

  override def dbl = new Fp(v.add(v).mod(P))

  override def inverse = new Fp(v.modInverse(P))

  override def negate = new Fp(v.negate.mod(P))

  override def isZero: Boolean = v.compareTo(BigInteger.ZERO) == 0

  override def isValid: Boolean = v.compareTo(P) < 0

  private[zksnark] def mul(o: Fp2): Fp2 = new Fp2(o.a.mul(this), o.b.mul(this))

  def bytes: Array[Byte] = v.toByteArray

  override def equals(o: Any): Boolean = {
    if (this == o) return true  // loop?
    if (o == null || (getClass ne o.getClass)) return false
    val fp = o.asInstanceOf[Fp]
    //return !(v != null ? v.compareTo(fp.v) != 0 : fp.v != null);
    if (if (v != null) v.compareTo(fp.v) != 0
    else fp.v != null) return false
    true
  }

  override def toString: String = v.toString
}


