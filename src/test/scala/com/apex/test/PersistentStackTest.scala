/*
 *
 *
 *
 *
 * Copyright  2018 APEX Technologies.Co.Ltd. All rights reserved.
 *
 * FileName: PersistentStackTest.scala
 *
 * @author: ruixiao.xiao@chinapex.com: 18-9-10 下午5:52@version: 1.0
 */

package com.apex.test

import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream}

import com.apex.storage.PersistentStack
import org.junit.Test

import scala.reflect.io.Directory

@Test
class PersistentStackTest {
  @Test
  def testSizeEmpty = {
    deleteDir("test_size_empty_persistent_stack")
    val stack = new PersistentStack("test_size_empty_persistent_stack", TestItem.fromBytes)
    assert(stack.size == 0)
    assert(stack.isEmpty)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testTopEmpty: Unit = {
    deleteDir("test_top_empty_persistent_stack")
    val stack = new PersistentStack("test_top_empty_persistent_stack", TestItem.fromBytes)
    stack.top
  }

  @Test
  def testPush = {
    deleteDir("test_push_persistent_stack")
    val stack = new PersistentStack("test_push_persistent_stack", TestItem.fromBytes)
    stack.push(TestItem("test1"))
    assert(stack.size == 1)
    assert(stack.top.value.equals("test1"))
    stack.push(TestItem("test2"))
    assert(stack.size == 2)
    assert(stack.top.value.equals("test2"))
  }

  @Test
  def testPop: Unit = {
    deleteDir("test_pop_persistent_stack")
    val stack = new PersistentStack("test_pop_persistent_stack", TestItem.fromBytes)
    stack.push(TestItem("test1"))
    stack.push(TestItem("test2"))
    stack.pop()
    assert(stack.size == 1)
    assert(stack.top.value.equals("test1"))
    stack.pop()
    assert(stack.size == 0)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testPopEmpty: Unit = {
    deleteDir("test_pop_empty_persistent_stack")
    val stack = new PersistentStack("test_pop_empty_persistent_stack", TestItem.fromBytes)
    stack.pop()
  }

  private def deleteDir(dir: String) = {
    val directory = Directory(dir)
    directory.deleteRecursively()
  }

  case class TestItem(val value: String) extends com.apex.common.Serializable {

    import com.apex.common.Serializable._

    override def serialize(os: DataOutputStream): Unit = os.writeString(value)
  }

  object TestItem {
    def fromBytes(bytes: Array[Byte]): TestItem = {
      import com.apex.common.Serializable._
      val bs = new ByteArrayInputStream(bytes)
      val is = new DataInputStream(bs)
      TestItem(is.readString)
    }
  }

}
