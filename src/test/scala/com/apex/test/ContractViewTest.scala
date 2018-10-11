/*
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: ContractViewTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-7-19 上午11:08@version: 1.0
 */

package com.apex.test

import scala.util.Random
import com.apex.core.ContractView
import org.junit.Test

@Test
class ContractViewTest {
  @Test
  def testSerialize = {
    val a = new ContractView(
      name = "test",
      author = "test",
      email = "test",
      description = "test",
      script = Array.fill(32)((Random.nextInt(256) - 128).toByte)
    )
    val o = new SerializerHelper[ContractView](
      ContractView.deserialize,
      (x, _) => x.version.equals(a.version)
        && x.id.equals(a.id)
        && x.name.equals(a.name)
        && x.author.equals(a.author)
        && x.email.equals(a.email)
        && x.description.equals(a.description)
        && x.script.sameElements(a.script)
    )
    o.test(a)
  }
}
