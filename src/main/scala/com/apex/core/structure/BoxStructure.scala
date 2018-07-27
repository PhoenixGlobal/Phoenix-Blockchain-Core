package com.apex.core.structure

import com.google.common.primitives.{Bytes, Longs}
import com.apex.core.structure.box.proposition.Proposition
import com.apex.core.structure.box.{Box, BoxUnlocker}


abstract class BoxStructure[P <: Proposition, BX <: Box[P]] extends StructureMessage {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  val fee: Long

  val timestamp: Long

  override lazy val messageToSign: Array[Byte] =
    Bytes.concat(if (newBoxes.nonEmpty) com.apex.core.utils.concatBytes(newBoxes.map(_.bytes)) else Array[Byte](),
      com.apex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
}
