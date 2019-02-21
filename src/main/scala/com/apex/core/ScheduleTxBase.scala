package com.apex.core

import com.apex.common.ApexLogging
import com.apex.settings.ScheduleBaseSettings
import com.apex.storage.Storage

class ScheduleTxBase(settings: ScheduleBaseSettings, db: Storage.lowLevelRaw, tracking: Tracking) extends ApexLogging{
  def this(settings: ScheduleBaseSettings, db: Storage.lowLevelRaw) = {
    this(settings, db, Tracking.root(db))
  }

  def this(settings: ScheduleBaseSettings) = {
    this(settings, Storage.open(settings.dbType, settings.dir))
  }

  def startTracking(): ScheduleTxBase = {
    new ScheduleTxBase(settings, db, tracking.newTracking)
  }

  // start new session
  def startSession(): Unit = {
    tracking.newSession()
  }

  // undo all operations in the latest session
  def rollBack(): Unit = {
    tracking.rollBack()
  }

  // commit all operations in sessions whose revision is equal to or larger than the specified revision
  def commit(revision: Long): Unit = {
    tracking.commit(revision)
  }

  // apply changes
  def commit(): Unit = {
    tracking.commit()
  }

  // return latest revision
  def revision(): Long = {
    tracking.revision()
  }

  def close(): Unit = {
    db.close()
  }
}
