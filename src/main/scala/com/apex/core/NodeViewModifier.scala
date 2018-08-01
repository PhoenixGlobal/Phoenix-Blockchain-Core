package com.apex.core

import com.typesafe.config.ConfigFactory
import com.apex.core.serialization.BytesSerializable
import com.apex.core.utils.ApexEncoding

import scala.util.Try

/**
 * 节点消息类型
 */
sealed trait NodeViewModifier extends BytesSerializable with ApexEncoding {
  self =>

  val modifierTypeId: ModifierTypeId

  def id: ModifierId

  def encodedId: String = encoder.encode(id)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: NodeViewModifier => (that.id sameElements id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }
}

trait EphemerealNodeViewModifier extends NodeViewModifier

object NodeViewModifier {
  private val DefaultIdSize: Byte = 32 // in bytes

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}
