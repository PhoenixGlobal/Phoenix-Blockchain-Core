package com.apex.crypto.zksnark

import com.apex.crypto.zksnark.Params.B_Fp

object BN128Fp {
  private[zksnark] val ZERO = new BN128Fp(Fp.ZERO, Fp.ZERO, Fp.ZERO)

  def create(xx: Array[Byte], yy: Array[Byte]): BN128[Fp] = {
    val x = Fp.create(xx)
    val y = Fp.create(yy)

    if (x.isZero && y.isZero) return ZERO
    val p = new BN128Fp(x, y, Fp._1)

    if (p.isValid) p
    else null
  }
}

class BN128Fp protected(override val x: Fp, override val y: Fp, override val z: Fp) extends BN128[Fp](x, y, z) {

  override protected def zero: BN128[Fp] = BN128Fp.ZERO

  override protected def instance(x: Fp, y: Fp, z: Fp) = new BN128Fp(x, y, z)

  override protected def b: Fp = B_Fp

  override protected def one: Fp = Fp._1
}


