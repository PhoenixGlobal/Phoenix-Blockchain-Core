package com.apex.core

import akka.actor.Actor
import com.apex.common.ApexLogging
import com.apex.core.consensus.History.ProgressInfo
import com.apex.core.consensus.{History, SyncInfo}
import com.apex.network.ConnectedPeer
import com.apex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import com.apex.core.serialization.Serializer
import com.apex.core.settings.ApexSettings
import com.apex.core.structure._
import com.apex.core.structure.state.{MinimalState, StructureValidation}
import com.apex.core.structure.wallet.Vault
import com.apex.core.utils.ApexEncoding
import com.apex.core.validation.RecoverableModifierError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}


trait NodeViewHolder[TX <: com.apex.core.structure.StructureMessage, PMOD <: PersistentNodeViewModifier]
  extends Actor with ApexLogging with ApexEncoding {

  import NodeViewHolder.ReceivableMessages._
  import NodeViewHolder._
  import com.apex.core.network.NodeViewSynchronizer.ReceivableMessages._

  type SI <: SyncInfo
  type HIS <: History[PMOD, SI, HIS]
  type MS <: MinimalState[PMOD, MS]
  type VL <: Vault[TX, PMOD, VL]
  type MP <: MemoryPool[TX, MP]

  type NodeView = (HIS, MS, VL, MP)

  val apexSettings: ApexSettings

  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  def restoreState(): Option[NodeView]

  protected def genesisState: NodeView


  protected def history(): HIS = nodeView._1

  protected def minimalState(): MS = nodeView._2

  protected def vault(): VL = nodeView._3

  protected def memoryPool(): MP = nodeView._4


  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]]

  protected type MapKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): MapKey = new mutable.WrappedArray.ofByte(id)

  protected lazy val modifiersCache: ModifiersCache[PMOD, HIS] =
    new DefaultModifiersCache[PMOD, HIS](apexSettings.network.maxModifiersCacheSize)

  protected def txModify(tx: TX): Unit = {
    val errorOpt: Option[Throwable] = minimalState() match {
      case txValidator: StructureValidation[TX] =>
        txValidator.validate(tx) match {
          case Success(_) => None
          case Failure(e) => Some(e)
        }
      case _ => None
    }

    errorOpt match {
      case None =>
        memoryPool().put(tx) match {
          case Success(newPool) =>
            val newVault = vault().scanOffchain(tx)
            updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
            context.system.eventStream.publish(SuccessfulStructure[TX](tx))

          case Failure(e) =>
            context.system.eventStream.publish(FailedStructure[TX](tx, e))
        }

      case Some(e) =>
        context.system.eventStream.publish(FailedStructure[TX](tx, e))
    }
  }

  protected def updateNodeView(updatedHistory: Option[HIS] = None,
                               updatedState: Option[MS] = None,
                               updatedVault: Option[VL] = None,
                               updatedMempool: Option[MP] = None): Unit = {
    val newNodeView = (updatedHistory.getOrElse(history()),
      updatedState.getOrElse(minimalState()),
      updatedVault.getOrElse(vault()),
      updatedMempool.getOrElse(memoryPool()))
    if (updatedHistory.nonEmpty) {
      context.system.eventStream.publish(ChangedHistory(newNodeView._1.getReader))
    }
    if (updatedState.nonEmpty) {
      context.system.eventStream.publish(ChangedState(newNodeView._2.getReader))
    }
    if (updatedVault.nonEmpty) {
      context.system.eventStream.publish(ChangedVault(newNodeView._3.getReader))
    }
//    if (updatedMempool.nonEmpty) {
//      context.system.eventStream.publish(ChangedMempool(newNodeView._4.getReader))
//    }
    nodeView = newNodeView
  }

  protected def extractStructures(mod: PMOD): Seq[TX] = mod match {
    case tcm: StructuresCarryingPersistentNodeViewModifier[TX] => tcm.structures
    case _ => Seq()
  }


  protected def updateMemPool(blocksRemoved: Seq[PMOD], blocksApplied: Seq[PMOD], memPool: MP, state: MS): MP = {
    val rolledBackTxs = blocksRemoved.flatMap(extractStructures)

    val appliedTxs = blocksApplied.flatMap(extractStructures)

    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: StructureValidation[TX] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  private def requestDownloads(pi: ProgressInfo[PMOD]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      if(!modifiersCache.contains(id)) {
        context.system.eventStream.publish(DownloadRequest(tid, id))
      }
    }

  private def trimChainSuffix(suffix: IndexedSeq[PMOD], rollbackPoint: ModifierId): IndexedSeq[PMOD] = {
    val idx = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }


  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[PMOD],
                          suffixApplied: IndexedSeq[PMOD]): (HIS, Try[MS], Seq[PMOD]) = {
    requestDownloads(progressInfo)

    case class UpdateInformation(history: HIS,
                                 state: MS,
                                 failedMod: Option[PMOD],
                                 alternativeProgressInfo: Option[ProgressInfo[PMOD]],
                                 suffix: IndexedSeq[PMOD])

    val (stateToApplyTry: Try[MS], suffixTrimmed: IndexedSeq[PMOD]) = if (progressInfo.chainSwitchingNeeded) {
        @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
        val branchingPoint = VersionTag @@ progressInfo.branchPoint.get     //todo: .get
        if (!state.version.sameElements(branchingPoint)){
          state.rollbackTo(branchingPoint) -> trimChainSuffix(suffixApplied, branchingPoint)
        } else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>

        val u0 = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)

        val uf = progressInfo.toApply.foldLeft(u0) {case (u, modToApply) =>
          if(u.failedMod.isEmpty) {
            u.state.applyModifier(modToApply) match {
              case Success(stateAfterApply) =>
                val newHis = history.reportModifierIsValid(modToApply)
                context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
                //updateState(newHis, stateAfterApply, newProgressInfo, suffixTrimmed :+ modToApply)
                UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
              case Failure(e) =>
                val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
                context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
                //updateState(newHis, stateToApply, newProgressInfo, suffixTrimmed)
                UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
            }
          } else u
        }

        uf.failedMod match {
          case Some(mod) =>
            @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
            val alternativeProgressInfo = uf.alternativeProgressInfo.get
            updateState(uf.history, uf.state, alternativeProgressInfo, uf.suffix)
          case None => (uf.history, Success(uf.state), uf.suffix)
        }
      case Failure(e) =>
        context.system.eventStream.publish(RollbackFailed)
        ???
    }
  }

  protected def pmodModify(pmod: PMOD): Unit =
    if (!history().contains(pmod.id)) {
      context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))

      history().append(pmod) match {
        case Success((historyBeforeStUpdate, progressInfo)) =>
          context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
          context.system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))

          if (progressInfo.toApply.nonEmpty) {
            val (newHistory, newStateTry, blocksApplied) =
              updateState(historyBeforeStUpdate, minimalState(), progressInfo, IndexedSeq())

            newStateTry match {
              case Success(newMinState) =>
                val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, memoryPool(), newMinState)

                @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
                val newVault = if (progressInfo.chainSwitchingNeeded) {
                  vault().rollback(VersionTag @@ progressInfo.branchPoint.get).get
                } else vault()
                blocksApplied.foreach(newVault.scanPersistent)

                updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))


              case Failure(e) =>
                updateNodeView(updatedHistory = Some(newHistory))
                context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
            }
          } else {
            requestDownloads(progressInfo)
            updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
          }
        case Failure(e) =>
          context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
      }
    }


  protected def compareViews: Receive = {
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == com.apex.core.structure.StructureMessage.ModifierTypeId =>
          memoryPool().notIn(modifierIds)
        case _ =>
          modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(key(mid)))
      }

      sender() ! RequestFromLocal(peer, modifierTypeId, ids)
  }

  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  protected def processRemoteModifiers: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case (tx: TX@unchecked) if tx.modifierTypeId == com.apex.core.structure.StructureMessage.ModifierTypeId =>
            txModify(tx)

          case pmod: PMOD@unchecked =>
            if (history().contains(pmod) || modifiersCache.contains(key(pmod.id))) {
            } else {
              modifiersCache.put(key(pmod.id), pmod)
            }
        }


        var applied: Boolean = false
        do {
          modifiersCache.popCandidate(history()) match {
            case Some(mod) =>
              pmodModify(mod)
              applied = true
            case None =>
              applied = false
          }
        } while (applied)
      }
  }

  protected def processLocallyGeneratedModifiers: Receive = {
    case lt: LocallyGeneratedStructure[TX] =>
      txModify(lt.tx)

    case lm: LocallyGeneratedModifier[PMOD] =>
      pmodModify(lm.pmod)
      
  }

  protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  protected def getNodeViewChanges: Receive = {
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView._1.getReader)
      if (state) sender() ! ChangedState(nodeView._2.getReader)
      if (vault) sender() ! ChangedVault(nodeView._3.getReader)
//      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
  }

  override def receive: Receive =
      compareViews orElse
      processRemoteModifiers orElse
      processLocallyGeneratedModifiers orElse
      getCurrentInfo orElse
      getNodeViewChanges orElse {
      case a: Any => log.error("unknown input: " + a)
    }
}


object NodeViewHolder {

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)
    case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)
    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])
    case class ModifiersFromRemote(source: ConnectedPeer, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

    case class LocallyGeneratedStructure[TX <: StructureMessage](tx: TX)
    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)
    case class LocallyGeneratedStructureTest(str: String)
  }

  case class ModificationApplicationStarted[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
    extends NodeViewHolderEvent

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}