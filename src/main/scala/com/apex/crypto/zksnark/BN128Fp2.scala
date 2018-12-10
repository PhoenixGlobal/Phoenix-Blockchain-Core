package com.apex.crypto.zksnark

import com.apex.crypto.zksnark.Params.B_Fp2

object BN128Fp2 {
  private[zksnark] val ZERO = new BN128Fp2(Fp2.ZERO, Fp2.ZERO, Fp2.ZERO)

  def create(aa: Array[Byte], bb: Array[Byte], cc: Array[Byte], dd: Array[Byte]): BN128[Fp2] = {

    val x = Fp2.create(aa, bb)
    val y = Fp2.create(cc, dd)

    if (x.isZero && y.isZero) return ZERO

    val p = new BN128Fp2(x, y, Fp2._1)

    if (p.isValid) return p
    else return null

  }
}

class BN128Fp2 protected(override val x: Fp2, override val y: Fp2, override val z: Fp2) extends BN128[Fp2](x, y, z) {

  override protected def zero: BN128[Fp2] = BN128Fp2.ZERO

  override protected def instance(x: Fp2, y: Fp2, z: Fp2) = new BN128Fp2(x, y, z)

  override protected def b: Fp2 = B_Fp2

  override protected def one: Fp2 = Fp2._1
}


