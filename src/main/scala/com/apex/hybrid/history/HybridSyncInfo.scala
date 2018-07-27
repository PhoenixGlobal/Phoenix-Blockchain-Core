package com.apex.hybrid.history

import com.apex.core.consensus.SyncInfo
import com.apex.network.message.SyncInfoMessageSpec
import com.apex.core.serialization.Serializer
import com.apex.core.{ModifierId, ModifierTypeId, NodeViewModifier}

import scala.util.Try


case class HybridSyncInfo(answer: Boolean,
                          lastPowBlockIds: Seq[ModifierId],
                          lastPosBlockId: ModifierId
                         ) extends SyncInfo {

  import HybridSyncInfo.MaxLastPowBlocks

  require(lastPowBlockIds.size <= MaxLastPowBlocks)

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = Nil
  // Seq(lastPowBlockIds.map(b => PowBlock.ModifierTypeId -> b) ++ Seq(PosBlock.ModifierTypeId -> lastPosBlockId)).flatten


  override type M = HybridSyncInfo

  override def serializer: Serializer[HybridSyncInfo] = HybridSyncInfoSerializer
}

object HybridSyncInfo {
  val MaxLastPowBlocks: Byte = 50 
}

object HybridSyncInfoSerializer extends Serializer[HybridSyncInfo] {

  import HybridSyncInfo.MaxLastPowBlocks

  override def toBytes(obj: HybridSyncInfo): Array[Byte] =
    Array(
      if (obj.answer) 1: Byte else 0: Byte,
      obj.lastPowBlockIds.size.toByte
    ) ++ obj.lastPowBlockIds.foldLeft(Array[Byte]())((a, b) => a ++ b) ++ obj.lastPosBlockId

  override def parseBytes(bytes: Array[Byte]): Try[HybridSyncInfo] = Try {
    val answer: Boolean = if (bytes.head == 1.toByte) true else false
    val lastPowBlockIdsSize = bytes.slice(1, 2).head

    require(lastPowBlockIdsSize >= 0 && lastPowBlockIdsSize <= MaxLastPowBlocks)
    require(bytes.length == 2 + (lastPowBlockIdsSize + 1) * NodeViewModifier.ModifierIdSize)

    val lastPowBlockIds = bytes.slice(2, 2 + NodeViewModifier.ModifierIdSize * lastPowBlockIdsSize)
      .grouped(NodeViewModifier.ModifierIdSize).toSeq.map(id => ModifierId @@ id)

    val lastPosBlockId = ModifierId @@ bytes
      .slice(2 + NodeViewModifier.ModifierIdSize * lastPowBlockIdsSize, bytes.length)

    HybridSyncInfo(answer, lastPowBlockIds, lastPosBlockId)
  }
}

object HybridSyncInfoMessageSpec extends SyncInfoMessageSpec[HybridSyncInfo](HybridSyncInfoSerializer.parseBytes)