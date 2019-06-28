package lockfree

import java.util.concurrent.atomic._

abstract class AbstractAtomicVariable[T] {
  def get: T
  def compareAndSet(expect: T, newval: T) : Boolean
}

class AtomicVariable[T](initial: T) extends AbstractAtomicVariable[T] {

  private val atomic = new AtomicReference[T](initial)

  override def get: T = atomic.get()

  override def compareAndSet(expected: T, value: T): Boolean = {
    val current = atomic.get
    if (current == expected) {
      atomic.compareAndSet(current, value)
    }
    else {
      false
    }
  }
}