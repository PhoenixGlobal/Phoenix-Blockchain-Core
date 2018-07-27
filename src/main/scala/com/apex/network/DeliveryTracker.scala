package com.apex.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import com.apex.common.ApexLogging
import com.apex.core.network.NodeViewSynchronizer.ReceivableMessages.CheckDelivery
import com.apex.core.{ModifierId, ModifierTypeId}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}


class DeliveryTracker(system: ActorSystem,
                      deliveryTimeout: FiniteDuration,
                      maxDeliveryChecks: Int,
                      nvsRef: ActorRef) extends ApexLogging {

  protected type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  protected case class ExpectingStatus(peer: ConnectedPeer, cancellable: Cancellable, checks: Int)

  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  protected val expecting = mutable.Map[ModifierIdAsKey, ExpectingStatus]()
  protected val delivered = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  protected val deliveredSpam = mutable.Map[ModifierIdAsKey, ConnectedPeer]()


  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Unit =
    tryWithLogging(mids.foreach(mid => expect(cp, mtid, mid)))

  protected def expect(cp: ConnectedPeer,
                       mtid: ModifierTypeId,
                       mid: ModifierId,
                       checksDone: Int = 0)(implicit ec: ExecutionContext): Unit = {
    val cancellable = system.scheduler.scheduleOnce(deliveryTimeout, nvsRef, CheckDelivery(cp, mtid, mid))
    val midAsKey = key(mid)
    expecting.put(midAsKey, ExpectingStatus(cp, cancellable, checks = checksDone))
  }

  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = tryWithLogging {
    if(isExpecting(mid)) {
      val midAsKey = key(mid)
      val checks = expecting(midAsKey).checks + 1
      stopExpecting(mid)
      if (checks < maxDeliveryChecks) expect(cp, mtid, mid, checks)
    } else {
      expect(cp, mtid, mid)
    }
  }

  protected def stopExpecting(mid: ModifierId): Unit = {
    val midAsKey = key(mid)
    expecting(midAsKey).cancellable.cancel()
    expecting.remove(midAsKey)
  }

  protected[network] def isExpecting(mid: ModifierId): Boolean =
    expecting.contains(key(mid))

  def onReceive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    if (isExpecting(mid)) {
      stopExpecting(mid)
      delivered(key(mid)) = cp
    } else {
      deliveredSpam(key(mid)) = cp
    }
  }

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  protected def tryWithLogging(fn: => Unit): Unit = {
    Try(fn).recoverWith {
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
  }
}