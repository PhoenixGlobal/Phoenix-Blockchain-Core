package com.apex.utils

import java.net.InetAddress
import java.util.concurrent.atomic.AtomicLong

import com.apex.common.ApexLogging
import com.apex.settings.NetworkTimeProviderSettings
import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object NetworkTime {
  def localWithOffset(offset: Long): Long = System.currentTimeMillis() + offset

  type Offset = Long
  type Time = Long
}

class NetworkTimeProvider(ntpSettings: NetworkTimeProviderSettings)(implicit ec: ExecutionContext)
  extends ApexLogging {

  private val lastUpdate = new AtomicLong(0)
  private var offset = new AtomicLong(0)
  private val client = new NTPUDPClient()
  client.setDefaultTimeout(ntpSettings.timeout.toMillis.toInt)
  client.open()

  private def updateOffset(): Future[NetworkTime.Offset] = Future {
    val info = client.getTime(InetAddress.getByName(ntpSettings.server))
    info.computeDetails()
    info.getOffset
  }

  private def checkUpdateRequired(): Unit = {
    try {
      val time = NetworkTime.localWithOffset(offset.get())
      val lu = lastUpdate.getAndSet(time)
      if (time > lu + ntpSettings.updateEvery.toMillis) {
        updateOffset().onComplete {
          case Success(newOffset) =>
            offset.set(newOffset)
            log.info("新时间偏移调整: " + offset)
            lastUpdate.set(time)
          case Failure(e) =>
            log.warn("Problems with NTP: ", e)
            lastUpdate.compareAndSet(time, lu)
        }
      } else {
        lastUpdate.compareAndSet(time, lu)
      }
    } catch {
      case e: Throwable => log.error(e.getMessage)
    }
  }


  def time(): NetworkTime.Time = {
    //checkUpdateRequired()   // NTP disabled
    NetworkTime.localWithOffset(offset.get())
  }
}