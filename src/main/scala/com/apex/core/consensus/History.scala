package com.apex.core.consensus

import com.apex.core._
import com.apex.core.consensus.History.ProgressInfo
import com.apex.crypto.BytesEncoder

import scala.util.Try


trait History[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]]
  extends HistoryReader[PM, SI] {

  def append(modifier: PM): Try[(HT, ProgressInfo[PM])]

  def reportModifierIsValid(modifier: PM): HT

  def reportModifierIsInvalid(modifier: PM, progressInfo: ProgressInfo[PM]): (HT, ProgressInfo[PM])

  def getReader: HistoryReader[PM, SI] = this

}

object History {

  type ModifierIds = Seq[(ModifierTypeId, ModifierId)]

  sealed trait HistoryComparisonResult

  case object Equal extends HistoryComparisonResult

  case object Younger extends HistoryComparisonResult

  case object Older extends HistoryComparisonResult

  case object Nonsense extends HistoryComparisonResult

  case object Unknown extends HistoryComparisonResult

  case class ProgressInfo[PM <: PersistentNodeViewModifier](branchPoint: Option[ModifierId],
                                                            toRemove: Seq[PM],
                                                            toApply: Seq[PM],
                                                            toDownload: Seq[(ModifierTypeId, ModifierId)])
                                                           (implicit encoder: BytesEncoder) {

    require(branchPoint.isDefined == toRemove.nonEmpty, s"should be non-emptye," +
      s" ${branchPoint.isDefined} == ${toRemove.nonEmpty} given")

    lazy val chainSwitchingNeeded: Boolean = toRemove.nonEmpty

    override def toString: String = {
      s"ProgressInfo(BranchPoint: ${branchPoint.map(encoder.encode)}, " +
        s" to remove: ${toRemove.map(_.encodedId)}, to apply: ${toApply.map(_.encodedId)})"
    }
  }

}