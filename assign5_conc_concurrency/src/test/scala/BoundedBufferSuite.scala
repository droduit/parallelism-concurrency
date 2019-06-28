package pubsub

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import instrumentation._
import instrumentation.Stats._

import pubsub.collection._

@RunWith(classOf[JUnitRunner])
class BoundedBufferSuite extends FunSuite {

  import TestHelper._
  import TestUtils._

  test("Should work in a sequential setting") {
    val buffer = new BoundedBuffer[Int](4);
    buffer.put(1)
    buffer.put(2)
    buffer.put(3)
    buffer.put(4)
    assert(buffer.take() == 1)
    assert(buffer.take() == 2)
    assert(buffer.take() == 3)
    assert(buffer.take() == 4)
  }
  
  test("Should work when Thread 1: `put(1)`, Thread 2: `take` and a buffer of size 1") {
    testManySchedules(2, sched => {
      val prodCons = new SchedulableBoundedBuffer[Int](1, sched)
      List(() => prodCons.put(1), () => prodCons.take())
    }, args =>  (args(1) == 1, s"expected 1 your `take` implementation returned ${args(1)}"))    
  }
}

object TestUtils {
  def failsOrTimesOut[T](action: => T): Boolean = {
    val asyncAction = future {
      action
    }
    try {
      Await.result(asyncAction, 2000.millisecond)
    }
    catch {
      case _: Throwable => return true
    }
    return false
  }
}
