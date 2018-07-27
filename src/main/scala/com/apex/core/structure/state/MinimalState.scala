package com.apex.core.structure.state

import com.apex.core.structure._
import com.apex.core.structure.box.proposition.Proposition
import com.apex.core.{PersistentNodeViewModifier, VersionTag}

import scala.util.Try

trait MinimalState[M <: PersistentNodeViewModifier, MS <: MinimalState[M, MS]] extends StateReader {
  self: MS =>

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  def getReader: StateReader = this

}


trait StateFeature

trait StructureValidation[TX <: StructureMessage] extends StateFeature {
  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def validate(tx: TX): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}

trait BalanceSheet[P <: Proposition] extends StateFeature {
  def balance(id: P, height: Option[Int] = None): Long
}

trait AccountStructuresHistory[P <: Proposition, TX <: StructureMessage] extends StateFeature {
  def accountStructures(id: P): Array[TX]
}
