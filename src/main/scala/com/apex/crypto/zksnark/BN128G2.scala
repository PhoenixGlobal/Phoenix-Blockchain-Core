package com.apex.crypto.zksnark

import com.apex.crypto.zksnark.Params.R
import com.apex.crypto.zksnark.Params.TWIST_MUL_BY_P_X
import com.apex.crypto.zksnark.Params.TWIST_MUL_BY_P_Y

import java.math.BigInteger

object BN128G2 {

  def create(a: Array[Byte], b: Array[Byte], c: Array[Byte], d: Array[Byte]): BN128G2 = {
    val p = BN128Fp2.create(a, b, c, d)

    if (p == null) return null

    if (!isGroupMember(p)) return null

    return new BN128G2(p)
  }

  private def isGroupMember(p: BN128[Fp2]): Boolean =  {
    val left = p.mul(FR_NEG_ONE).add(p)
    return left.isZero // should satisfy condition: -1 * p + p == 0, where -1 belongs to F_r
  }

  private[zksnark] val FR_NEG_ONE = BigInteger.ONE.negate.mod(R)

  def apply(x: Fp2, y: Fp2, z: Fp2): BN128G2 = {
    new BN128G2(BN128Fp2.create(x, y, z))
  }

}

class BN128G2 private[zksnark](val p: BN128[Fp2]) extends BN128Fp2(p.x, p.y, p.z) {
  override def toAffine = new BN128G2(super.toAffine)

  private[zksnark] def mulByP(): BN128G2 = {
    val rx = TWIST_MUL_BY_P_X.mul(x.frobeniusMap(1))
    val ry = TWIST_MUL_BY_P_Y.mul(y.frobeniusMap(1))
    val rz = z.frobeniusMap(1)
    BN128G2(rx, ry, rz)
  }

}


