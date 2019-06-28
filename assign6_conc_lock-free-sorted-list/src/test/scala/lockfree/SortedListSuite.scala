package lockfree

import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.mutable.HashMap
import scala.util.Random

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SortedListSuite extends FunSuite {
  import instrumentation.SchedulableSortedList
  import instrumentation.TestHelper._
  import instrumentation.TestUtils._
  
  test("Should work when 1, 2, and 3 are inserted sequentially") {
    val l = new SortedList
    l.insert(1)
    l.insert(2)
    l.insert(3)
    assert(l.toList == List(1, 2, 3))
  }

  test("Should work when 3, 2, and 1 are inserted sequentially") {
    val l = new SortedList
    l.insert(3)
    l.insert(2)
    l.insert(1)
    assert(l.toList == List(1, 2, 3))
  }

  test("Should work when duplicate elements are inserted sequentially") {
    val l = new SortedList
    l.insert(0)
    l.insert(0)
    l.insert(2)
    l.insert(2)
    assert(l.toList == List(0, 0, 2, 2))
  }

  test(s"should return [1,4,5] when from [1,2,3,5], a thread removes 3, 2 and then inserts 4") {
    val l = new SortedList
    l.insert(1)
    l.insert(2)
    l.insert(3)
    l.insert(5)
    l.delete(3)
    l.delete(2)
    l.insert(4)
    assert(l.toList == List(1, 4, 5))
  }

  test("Should work when a random list of 100 elements are inserted sequentially") {
    val rand = new Random()    
    val randvals = for(i <- 1 to 100) yield rand.nextInt()
    val l = new SortedList
    randvals.foreach { l.insert }    
    assert(l.toList == randvals.sorted)
  }
  
  test("should insert in parallel 1, 2 and 3 in the list [0, 4]") {
    testManySchedules(3, sched => {
      val sortedList = new SchedulableSortedList(sched)
      sortedList.insert(0)
      sortedList.insert(4)
      ((for(i <- 1 to 3) yield () => sortedList.insert(i)).toList,
       results => {
        val res = sortedList.toList
        (res == List(0, 1, 2, 3, 4),
        s"expected List(0, 1, 2, 3, 4), got $res")
      })
    })
  }
  
   test("should return List(true, false) when the first thread deletes 2 and the second one deletes 4 from SortedList(1, 2, 3)") {
    testManySchedules(2, sched => {
      val sortedList = new SchedulableSortedList(sched)
      sortedList.insert(1)
      sortedList.insert(2)
      sortedList.insert(3)
      (List(() => sortedList.delete(2),
            () => sortedList.delete(4)),
      results => {
        val res = sortedList.toList
        val expected = List(1, 3)
        if(res != expected) {
           (false, s"expected the final list to be $expected, your `delete` implementation returned ${res}")
        } else if(results != List(true, false)) {
          (false, s"expected threads to return List(true, false), your `delete` implementation returned ${results}")
        } else (true, "")
      })
    })
  }
  
  test("additional test 1 from site (approximately)") {
    testManySchedules(3, sched => {
      val sortedList = new SchedulableSortedList(sched)
      (List(() => sortedList.insert(4),
            () => sortedList.insert(2),
            () => sortedList.insert(3)),
      results => {
        val res = sortedList.toList
        (res == List(2, 3, 4),
        s"expected List(2, 3, 4), got $res")
      })
    })
  }
  
  test("additional test 2 from site (approx.)") {
    testManySchedules(2, sched => {
      val sortedList = new SchedulableSortedList(sched)
      sortedList.insert(1)
      sortedList.insert(2)
      sortedList.insert(2)
      (List(() => sortedList.delete(2),
            () => sortedList.delete(2)),
      results => {
        val res = sortedList.toList
        val expected = List(1)
        if(res != expected) {
           (false, s"expected the final list to be $expected, your `delete` implementation returned ${res}")
        } else if(results != List(true, true)) {
          (false, s"expected threads to return List(true, false), your `delete` implementation returned ${results}")
        } else (true, "")
      })
    })
  }
  
  /* test 3 isn't here because i couldn't figure out how they did that one, so that's it */
}

