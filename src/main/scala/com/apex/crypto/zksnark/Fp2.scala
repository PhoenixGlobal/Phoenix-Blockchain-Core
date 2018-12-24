package com.apex.crypto.zksnark


import com.apex.crypto.zksnark.Params.P

import java.math.BigInteger

object Fp2 {
  private[zksnark] val ZERO = new Fp2(Fp.ZERO, Fp.ZERO)
  private[zksnark] val _1 = new Fp2(Fp._1, Fp.ZERO)
  private[zksnark] val NON_RESIDUE = new Fp2(BigInteger.valueOf(9), BigInteger.ONE)
  private[zksnark] val FROBENIUS_COEFFS_B = Array[Fp](new Fp(BigInteger.ONE), new Fp(new BigInteger("21888242871839275222246405745257275088696311157297823662689037894645226208582")))

  private[zksnark] def create(aa: BigInteger, bb: BigInteger) = {
    val a = Fp.create(aa)
    val b = Fp.create(bb)
    new Fp2(a, b)
  }

  private[zksnark] def create(aa: Array[Byte], bb: Array[Byte]) = {
    val a = Fp.create(aa)
    val b = Fp.create(bb)
    new Fp2(a, b)
  }
}

class Fp2 private[zksnark](var a: Fp, var b: Fp) extends Field[Fp2] {
  def this(a: BigInteger, b: BigInteger) {
    this(new Fp(a), new Fp(b))
  }

  override def squared: Fp2 = { // using Complex squaring
    val ab = a.mul(b)
    val ra = a.add(b).mul(b.mul(Fp.NON_RESIDUE).add(a)).sub(ab).sub(ab.mul(Fp.NON_RESIDUE))
    // ra = (a + b)(a + NON_RESIDUE * b) - ab - NON_RESIDUE * b
    val rb = ab.dbl
    new Fp2(ra, rb)
  }

  override def mul(o: Fp2): Fp2 = {
    val aa = a.mul(o.a)
    val bb = b.mul(o.b)
    val ra = bb.mul(Fp.NON_RESIDUE).add(aa)
    // ra = a1 * a2 + NON_RESIDUE * b1 * b2
    val rb = a.add(b).mul(o.a.add(o.b)).sub(aa).sub(bb) // rb = (a1 + b1)(a2 + b2) - a1 * a2 - b1 * b2
    new Fp2(ra, rb)
  }

  override def add(o: Fp2) = new Fp2(a.add(o.a), b.add(o.b))

  override def sub(o: Fp2) = new Fp2(a.sub(o.a), b.sub(o.b))

  override def dbl: Fp2 = this.add(this)

  override def inverse: Fp2 = {
    val t0 = a.squared
    val t1 = b.squared
    val t2 = t0.sub(Fp.NON_RESIDUE.mul(t1))
    val t3 = t2.inverse
    val ra = a.mul(t3)
    // ra = a * t3
    val rb = b.mul(t3).negate // rb = -(b * t3)
    new Fp2(ra, rb)
  }

  override def negate = new Fp2(a.negate, b.negate)

  override def isZero: Boolean = this == Fp2.ZERO

  override def isValid: Boolean = a.isValid && b.isValid

  override def equals(o: Any): Boolean = {
    if (this == o) return true   // loop?
    if (o == null || (getClass ne o.getClass)) return false
    val fp2 = o.asInstanceOf[Fp2]
    if (if (a != null) !(a == fp2.a)
    else fp2.a != null) return false
    //return !(b != null ? !b.equals(fp2.b) : fp2.b != null);
    if (if (b != null) !(b == fp2.b)
    else fp2.b != null) return false
    true
  }

  private[zksnark] def frobeniusMap(power: Int) = {
    val ra = a
    val rb = Fp2.FROBENIUS_COEFFS_B(power % 2).mul(b)
    new Fp2(ra, rb)
  }

  private[zksnark] def mulByNonResidue = Fp2.NON_RESIDUE.mul(this)

  override def toString: String = String.format("%si + %s", a.toString, b.toString)
}

