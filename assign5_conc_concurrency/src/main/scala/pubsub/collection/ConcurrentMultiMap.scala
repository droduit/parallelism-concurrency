package pubsub.collection

import scala.collection.mutable
import java.util.concurrent.locks.ReentrantReadWriteLock

class ConcurrentMultiMap[K,V] {

  private val lockRW = new ReentrantReadWriteLock()
  val map = mutable.HashMap[K, Set[V]]()

  def lock(): Unit = lockRW.writeLock().lock()

  def unlock(): Unit = lockRW.writeLock().unlock()

  def add(key: K, value: V): Unit = {
    try {
      lockRW.writeLock().lock()
      map.get(key) match {
        case Some(set) =>
          if (! set.contains(value)) {
            map += ((key, set + value))
          }
        case None =>
          map += ((key, Set(value)))
      }
    } finally {
      lockRW.writeLock().unlock()
    }
  }

  def get(key: K): Option[Set[V]] = {
    try {
      lockRW.readLock().lock()
      val v = map.get(key)
      v
    } finally {
      lockRW.readLock().unlock()
    }
  }

  def remove(key: K, value: V): Unit = {
    try {
      lockRW.writeLock().lock()
      map.get(key) match {
        case Some(set) =>
          if (set.contains(value)) {
            map += ((key, set - value))
          }
        case None =>
      }
    } finally {
      lockRW.writeLock().unlock()
    }
  }

  def removeValueFromAll(value: V): Unit = {
    try {
      lockRW.writeLock().lock()
      map.keys.foreach(remove(_, value))
    } finally {
      lockRW.writeLock().unlock()
    }
  }

}
