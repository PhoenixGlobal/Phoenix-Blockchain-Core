package com.apex.core.hash

import org.bouncycastle.crypto.digests.Blake2bDigest

class Blake2b256Unsafe extends CryptographicHash32 with ThreadUnsafeHash[Digest32] {
  private val digestFn = new Blake2bDigest(DigestSize * 8)

  override def hash(input: Message): Digest32 = {
    digestFn.update(input, 0, input.length)
    val res = new Array[Byte](DigestSize)
    digestFn.doFinal(res, 0)
    Digest32 @@ res
  }

  override def prefixedHash(prefix: Byte, inputs: Message*): Digest32 = {
    digestFn.update(prefix)
    hash(inputs: _*)
  }

  override def hash(inputs: Message*): Digest32 = {
    inputs.foreach(i => digestFn.update(i, 0, i.length))
    val res = new Array[Byte](DigestSize)
    digestFn.doFinal(res, 0)
    Digest32 @@ res
  }
}