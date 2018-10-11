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
import org.junit.{AfterClass, Test}

case class A(a: Int)

@Test
class PersistentStackTest {
  @Test
  def testSizeEmpty = {
    val stack = PersistentStackTest.newStack("testSizeEmpty")
    assert(stack.size == 0)
    assert(stack.isEmpty)
  }

  @Test(expected = classOf[IndexOutOfBoundsException])
  def testTopEmpty: Unit = {
    val stack = PersistentStackTest.newStack("testTopEmpty")
    stack.top
  }

  @Test
  def testPush = {
    val stack = PersistentStackTest.newStack("testPush")
    stack.push(TestItem("test1"))
    assert(stack.size == 1)
    assert(stack.top.value.equals("test1"))
    stack.push(TestItem("test2"))
    assert(stack.size == 2)
    assert(stack.top.value.equals("test2"))
  }

  @Test
  def testPop: Unit = {
    val stack = PersistentStackTest.newStack("testPop")
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
    val stack = PersistentStackTest.newStack("testPopEmpty")
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
  def newStack(testMethod: String): PersistentStack[TestItem] = {
    val db = DbManager.open("PersistentStackTest", testMethod)
    val stack = new PersistentStack[TestItem](db)
    stack
  }

  @AfterClass
  def cleanUp: Unit = {
    DbManager.clearUp("PersistentStackTest")
  }
}
