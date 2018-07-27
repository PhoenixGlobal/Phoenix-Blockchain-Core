package com.apex.core.consensus

import com.apex.core._

import scala.util.Try


trait HistoryReader[PM <: PersistentNodeViewModifier, SI <: SyncInfo] extends NodeViewComponent {

  import History._

  def isEmpty: Boolean

  def contains(persistentModifier: PM): Boolean = contains(persistentModifier.id)

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  def applicableTry(modifier: PM): Try[Unit]

  def modifierById(modifierId: ModifierId): Option[PM]

  //def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  def openSurfaceIds(): Seq[ModifierId]

  def continuationIds(info: SI, size: Int): Option[ModifierIds]

  def syncInfo: SI

  def compare(other: SI): HistoryComparisonResult
}