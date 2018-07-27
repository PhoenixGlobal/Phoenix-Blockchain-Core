package com.apex.core.structure

import com.apex.core.{ModifierId, NodeViewComponent}

trait MempoolReader[TX <: StructureMessage] extends NodeViewComponent {

  def getById(id: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take(limit: Int): Iterable[TX]

}