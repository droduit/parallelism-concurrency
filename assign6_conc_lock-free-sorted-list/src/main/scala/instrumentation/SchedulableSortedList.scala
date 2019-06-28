package instrumentation

import scala.annotation.tailrec
import lockfree._
import java.util.concurrent.atomic._

class SchedulableAtomicVariable[T](initial: T, scheduler: Scheduler, self: Node) extends AbstractAtomicVariable[T] {
  private val proxied: AtomicVariable[T] = new AtomicVariable[T](initial)

  override def get: T = scheduler.exec {
      proxied.get
  } ( s"", Some(res => s"$self: get $res") )

  override def compareAndSet(expect: T, newval: T): Boolean = {
    scheduler.exec {
      proxied.compareAndSet(expect, newval)
    } (s"$self: compareAndSet expect = $expect, newval = $newval", Some(res => s"$self: Did it set? $res") )
  }
}

class SchedulableNode(value: Int, initTail: Option[Node], val scheduler: Scheduler) extends Node(value, initTail) with LockFreeMonitor { self =>

  override val atomicState: AbstractAtomicVariable[State] = new SchedulableAtomicVariable[State](initialState, scheduler, this)

  override def toString: String = {
    f"Node($value)#${## % 100}%02d"
  }
}

class SchedulableSortedList(val scheduler: Scheduler) extends SortedList with LockFreeMonitor {

  override def createNode(value: Int, tail: Option[Node], isHead: Boolean) = new SchedulableNode(value, tail, scheduler) {
    override def toString = if(isHead) "HEAD" else super.toString
  }
}

