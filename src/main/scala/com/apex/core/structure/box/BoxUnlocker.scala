package com.apex.core.structure.box

import com.apex.core.structure.box.proposition.Proposition
import com.apex.core.structure.proof.Proof
import com.apex.core.utils.ApexEncoding

trait BoxUnlocker[P <: Proposition] extends ApexEncoding {
  val closedBoxId: Array[Byte]
  val boxKey: Proof[P]

  override def toString: String = s"BoxUnlocker(id: ${encoder.encode(closedBoxId)}, boxKey: $boxKey)"
}
