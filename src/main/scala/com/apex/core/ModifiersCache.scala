package com.apex.core

import com.apex.common.ApexLogging
import com.apex.core.consensus.HistoryReader
import com.apex.core.validation.RecoverableModifierError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success}

trait ModifiersCache[PMOD <: PersistentNodeViewModifier, H <: HistoryReader[PMOD, _]] {
  require(maxSize >= 1)

  type K = mutable.WrappedArray[Byte]
  type V = PMOD

  protected val cache: mutable.Map[K, V] = mutable.Map[K, V]()

  def size: Int = cache.size

  def maxSize: Int

  protected val rememberedKeys: mutable.HashSet[K] = mutable.HashSet[K]()

  def findCandidateKey(history: H): Option[K]

  protected def onPut(key: K): Unit = {}
  protected def onRemove(key: K, rememberKey: Boolean): Unit = {}

  protected def keyToRemove(): K


  def contains(key: K): Boolean = cache.contains(key) || rememberedKeys.contains(key)

  def put(key: K, value: V): Unit = synchronized {
    if(!contains(key)) {
      onPut(key)
      cache.put(key, value)
      if (size > maxSize) remove(keyToRemove())
    }
  }

  def remove(key: K, rememberKey: Boolean = false): Option[V] = synchronized {
    cache.remove(key).map {removed =>
      onRemove(key, rememberKey)
      if (rememberKey) rememberedKeys += key
      removed
    }
  }

  def popCandidate(history: H): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }
}

trait LRUCache[PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, _]] extends ModifiersCache[PMOD, HR] {

  private val evictionQueue = mutable.Queue[K]()

  private val cleaningThreshold = 50

  @tailrec
  private def evictionCandidate(): K = {
    val k = evictionQueue.dequeue()
    if(cache.contains(k)) k else evictionCandidate()
  }

  override protected def onPut(key: K): Unit = {
    evictionQueue.enqueue(key)
    if(evictionQueue.size > maxSize + cleaningThreshold){
      evictionQueue.dequeueAll(k => !cache.contains(k))
    }
  }

  override protected def onRemove(key: K, rememberKey: Boolean): Unit = {
  }

  def keyToRemove(): K = {
    evictionCandidate()
  }
}

class DefaultModifiersCache[PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, _]]
  (override val maxSize: Int) extends ModifiersCache[PMOD, HR] with LRUCache[PMOD, HR] with ApexLogging {

  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  override def findCandidateKey(history: HR): Option[K] = {

    cache.find { case (k, v) =>
      history.applicableTry(v) match {
        case Failure(e) if e.isInstanceOf[RecoverableModifierError] =>
          false
        case Failure(e) =>
          remove(k, rememberKey = true)
          false
        case Success(_) =>
          true
      }
    }.map(_._1)
  }
}