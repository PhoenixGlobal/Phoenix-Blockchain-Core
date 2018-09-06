package com.apex.test

import com.apex.common.Serializable
import org.junit.Test

@Test
class JsonParseTest {
  @Test
  def jsonParse(): Unit ={

    val data = Value("hello  world", "test")
    val ret = Serializable.JsonMapperTo(data)
    println(ret)
    println("***************")
//    println(Serializable.JsonMapperFrom[Value](ret, ClassValue[Value]))
  }
}

case class Value( a: String,  b: String)
