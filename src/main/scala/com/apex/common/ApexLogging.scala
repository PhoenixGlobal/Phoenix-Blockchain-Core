package com.apex.common

import org.slf4j.LoggerFactory
import org.slf4j.Logger


trait ApexLogging {

  protected def log = LoggerFactory.getLogger(this.getClass)
}
