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

import com.apex.storage.{LevelDbStorage, PersistentStack}
import org.junit.{AfterClass, BeforeClass, Test}

import scala.collection.mutable.ListBuffer
import scala.reflect.io.Directory

@Test
class PersistentStackTest {
  @Test
  def testSizeEmpty = {
    val stack = PersistentStackTest.newStack("test_size_empty_persistent_stack")
    assert(stack.size == 0)
    assert(stack.isEmpty)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testTopEmpty: Unit = {
    val stack = PersistentStackTest.newStack("test_top_empty_persistent_stack")
    stack.top
  }

  @Test
  def testPush = {
    val stack = PersistentStackTest.newStack("test_push_persistent_stack")
    stack.push(TestItem("test1"))
    assert(stack.size == 1)
    assert(stack.top.value.equals("test1"))
    stack.push(TestItem("test2"))
    assert(stack.size == 2)
    assert(stack.top.value.equals("test2"))
  }

  @Test
  def testPop: Unit = {
    val stack = PersistentStackTest.newStack("test_pop_persistent_stack")
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
    val stack = PersistentStackTest.newStack("test_pop_empty_persistent_stack")
    stack.pop()
  }
}

case class TestItem(val value: String) extends com.apex.common.Serializable {

  import com.apex.common.Serializable._

  override def serialize(os: DataOutputStream): Unit = os.writeString(value)
}

object TestItem {
  implicit val deserializer: Array[Byte] => TestItem = fromBytes

  def fromBytes(bytes: Array[Byte]): TestItem = {
    import com.apex.common.Serializable._
    val bs = new ByteArrayInputStream(bytes)
    val is = new DataInputStream(bs)
    TestItem(is.readString)
  }
}

object PersistentStackTest {
  private final val dirs = ListBuffer.empty[String]

  def newStack(dir: String): PersistentStack[TestItem] = {
    val db = LevelDbStorage.open(dir)
    val stack = new PersistentStack[TestItem](db)
    dirs.append(dir)
    stack
  }

  @AfterClass
  def cleanUp: Unit = {
    dirs.foreach(cleanUp)
  }

  private def cleanUp(dir: String): Unit = {
    try {
      Directory(dir).deleteRecursively()
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }
}
