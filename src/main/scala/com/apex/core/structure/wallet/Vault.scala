package com.apex.core.structure.wallet

import com.apex.core.structure.StructureMessage
import com.apex.core.{PersistentNodeViewModifier, VersionTag}

import scala.util.Try


trait Vault[TX <: StructureMessage, PMOD <: PersistentNodeViewModifier, V <: Vault[TX, PMOD, V]] extends VaultReader {
  self: V =>

  def scanOffchain(tx: TX): V

  def scanOffchain(txs: Seq[TX]): V

  def scanPersistent(modifier: PMOD): V

  def scanPersistent(modifiers: Option[PMOD]): V = modifiers.foldLeft(this) { case (v, mod) =>
    v.scanPersistent(mod)
  }

  def rollback(to: VersionTag): Try[V]

  def getReader: VaultReader = this

}