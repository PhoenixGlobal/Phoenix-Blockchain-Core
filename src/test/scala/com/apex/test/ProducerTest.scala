/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ProducerTest.scala
 *
 * @author: shan.huang@chinapex.com: 18-7-18 下午4:06@version: 1.0
 */

package com.apex.test

import com.apex.consensus.ProducerUtil
import org.junit.Test

@Test
class ProducerTest {   
  @Test
  def testTime = {

    assert(ProducerUtil.isTimeStampValid(500, 500))
    assert(!ProducerUtil.isTimeStampValid(600, 500))
    assert(!ProducerUtil.isTimeStampValid(999, 500))
    assert(ProducerUtil.isTimeStampValid(1000, 500))
    assert(!ProducerUtil.isTimeStampValid(1001, 500))

    assert(ProducerUtil.nextTime(100, 500) == 500)
    assert(ProducerUtil.nextTime(101, 500) == 500)
    assert(ProducerUtil.nextTime(499, 500) == 500)
    assert(ProducerUtil.nextTime(500, 500) == 1000)
    assert(ProducerUtil.nextTime(501, 500) == 1000)
    assert(ProducerUtil.nextTime(999, 500) == 1000)
    assert(ProducerUtil.nextTime(1000, 500) == 1500)

    assert(ProducerUtil.nextBlockTime(500, 500, 100, 500) == 1000)
    assert(ProducerUtil.nextBlockTime(500, 600, 100, 500) == 1000)
    assert(ProducerUtil.nextBlockTime(500, 900, 100, 500) == 1000)
    assert(ProducerUtil.nextBlockTime(500, 901, 100, 500) == 1500)
    assert(ProducerUtil.nextBlockTime(500, 999, 100, 500) == 1500)
    assert(ProducerUtil.nextBlockTime(500, 1000, 100, 500) == 1500)
    assert(ProducerUtil.nextBlockTime(500, 9000, 100, 500) == 9500)
    assert(ProducerUtil.nextBlockTime(9000, 100, 100, 500) == 9500)
  }
}
