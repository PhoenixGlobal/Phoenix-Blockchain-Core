package com.apex.core.structure

import com.apex.core.{EphemerealNodeViewModifier, ModifierId, ModifierTypeId}
import com.apex.core.hash.Blake2b256


abstract class StructureMessage extends EphemerealNodeViewModifier {
  override val modifierTypeId: ModifierTypeId = StructureMessage.ModifierTypeId

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = ModifierId @@ Blake2b256(messageToSign)
}


object StructureMessage {
  val ModifierTypeId: com.apex.core.ModifierTypeId = com.apex.core.ModifierTypeId @@ 2.toByte
}