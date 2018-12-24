package com.apex.crypto.zksnark

import java.math.BigInteger

abstract class BN128[T <: Field[T]] protected(val x: T, val y: T, val z: T) {

  protected def zero: BN128[T]

  protected def instance(x: T, y: T, z: T): BN128[T]

  protected def b: T

  protected def one: T


  def toAffine: BN128[T] = {
    if (isZero) {
      val zero_ = zero
      return instance(zero_.x, one, zero_.z) // (0; 1; 0)

    }
    val zInv = z.inverse
    val zInv2 = zInv.squared
    val zInv3 = zInv2.mul(zInv)
    val ax = x.mul(zInv2)
    val ay = y.mul(zInv3)
    instance(ax, ay, one)
  }


  def toEthNotation: BN128[T] = {
    val affine = toAffine

    if (affine.isZero) zero
    else affine
  }

  protected def isOnCurve: Boolean = {
    if (isZero) return true
    val z6 = z.squared.mul(z).squared
    val left = y.squared

    val right = x.squared.mul(x).add(b.mul(z6)) // x^3 + b * z^6
    left == right
  }

  def add(o: BN128[T]): BN128[T] = {
    if (this.isZero) return o // 0 + P = P
    if (o.isZero) return this // P + 0 = P
    val x1 = this.x
    val y1 = this.y
    val z1 = this.z
    val x2 = o.x
    val y2 = o.y
    val z2 = o.z

    val z1z1 = z1.squared
    val z2z2 = z2.squared
    val u1 = x1.mul(z2z2)
    val u2 = x2.mul(z1z1)
    val z1Cubed = z1.mul(z1z1)
    val z2Cubed = z2.mul(z2z2)
    val s1 = y1.mul(z2Cubed)
    // s1 = y1 * Z2^3
    val s2 = y2.mul(z1Cubed) // s2 = y2 * Z1^3
    if (u1 == u2 && s1 == s2) return dbl // P + P = 2P
    val h = u2.sub(u1)
    // h = u2 - u1
    val i = h.dbl.squared
    // i = (2 * h)^2
    val j = h.mul(i)
    // j = h * i
    val r = s2.sub(s1).dbl
    // r = 2 * (s2 - s1)
    val v = u1.mul(i)
    // v = u1 * i
    val zz = z1.add(z2).squared.sub(z1.squared).sub(z2.squared)
    val x3 = r.squared.sub(j).sub(v.dbl)
    // x3 = r^2 - j - 2 * v
    val y3 = v.sub(x3).mul(r).sub(s1.mul(j).dbl)
    // y3 = r * (v - x3) - 2 * (s1 * j)
    val z3 = zz.mul(h) // z3 = ((z1+z2)^2 - z1^2 - z2^2) * h = zz * h
    instance(x3, y3, z3)
  }

  def mul(s: BigInteger): BN128[T] = {
    if (s.compareTo(BigInteger.ZERO) == 0) { // P * 0 = 0
      return zero
    }
    if (isZero) return this // 0 * s = 0
    var res = zero
    var i = s.bitLength - 1
    while ( {
      i >= 0
    }) {
      res = res.dbl
      if (s.testBit(i)) res = res.add(this)

      {
        i -= 1; i + 1
      }
    }
    res
  }

  private def dbl: BN128[T] = {
    if (isZero) return this

    val a = x.squared
    // a = x^2
    val b = y.squared
    // b = y^2
    val c = b.squared
    // c = b^2
    var d = x.add(b).squared.sub(a).sub(c)
    d = d.add(d) // d = 2 * ((x + b)^2 - a - c)

    val e = a.add(a).add(a)
    // e = 3 * a
    val f = e.squared
    // f = e^2
    val x3 = f.sub(d.add(d))
    // rx = f - 2 * d
    val y3 = e.mul(d.sub(x3)).sub(c.dbl.dbl.dbl)
    // ry = e * (d - rx) - 8 * c
    val z3 = y.mul(z).dbl // z3 = 2 * y * z
    instance(x3, y3, z3)
  }

  //def x: T = x

  //def y: T = y

  def isZero: Boolean = z.isZero

  protected def isValid: Boolean = {
    if (!x.isValid || !y.isValid || !z.isValid) return false

    if (!isOnCurve) return false
    true
  }

  override def toString: String = String.format("(%s; %s; %s)", x.toString, y.toString, z.toString)

  @SuppressWarnings(Array("all")) override def equals(o: Any): Boolean = {
    if (this == o) return true   // loop?
    if (!o.isInstanceOf[BN128[_ <: Field[_]]]) return false
    val bn128 = o.asInstanceOf[BN128[_ <: Field[_]]]
    if (if (x != null) !(x == bn128.x)
    else bn128.x != null) return false
    if (if (y != null) !(y == bn128.y)
    else bn128.y != null) return false
    if (if (z != null) !(z == bn128.z)
    else bn128.z != null) return false
    return true
  }
}

