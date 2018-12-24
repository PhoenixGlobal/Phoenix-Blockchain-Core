package com.apex.crypto.zksnark

object BN128G1 {

  def create(x: Array[Byte], y: Array[Byte]): BN128G1 = {
    val p = BN128Fp.create(x, y)
    if (p == null) return null
    if (!isGroupMember(p)) return null
    new BN128G1(p)
  }

  private def isGroupMember(p: BN128[Fp]) = true
}

class BN128G1 private[zksnark](val p: BN128[Fp]) extends BN128Fp(p.x, p.y, p.z) {

  override def toAffine = new BN128G1(super.toAffine)

}


