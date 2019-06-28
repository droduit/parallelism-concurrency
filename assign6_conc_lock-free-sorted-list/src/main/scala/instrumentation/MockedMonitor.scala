package instrumentation

trait MockedMonitor extends Monitor {
  def scheduler: Scheduler
  
  // Can be overriden.
  override def waitDefault() = {
    scheduler.log("wait")
    scheduler updateThreadState Wait(this, scheduler.threadLocks.tail)
  }
  override def synchronizedDefault[T](toExecute: =>T): T = {
    scheduler.log("synchronized check") 
    val prevLocks = scheduler.threadLocks
    scheduler updateThreadState Sync(this, prevLocks) // If this belongs to prevLocks, should just continue.
    scheduler.log("synchronized -> enter")
    val t = toExecute
    scheduler updateThreadState Running(prevLocks)
    scheduler.log("synchronized -> out")
    t
  }
  override def notifyDefault() = {
    scheduler mapOtherStates {
      state => state match {
        case Wait(lockToAquire, locks) if lockToAquire == this => SyncUnique(this, state.locks)
        case e => e
      }
    }
    scheduler.log("notify")
  }
  override def notifyAllDefault() = {
    scheduler mapOtherStates {
      state => state match {
        case Wait(lockToAquire, locks) if lockToAquire == this => Sync(this, state.locks)
        case e => e
      }
    }
    scheduler.log("notifyAll")
  }
}

trait LockFreeMonitor extends Monitor {
  override def waitDefault() = {
    throw new Exception("Please use lock-free structures and do not use wait()")
  }
  override def synchronizedDefault[T](toExecute: =>T): T = {
    throw new Exception("Please use lock-free structures and do not use synchronized()")
  }
  override def notifyDefault() = {
    throw new Exception("Please use lock-free structures and do not use notify()")
  }
  override def notifyAllDefault() = {
    throw new Exception("Please use lock-free structures and do not use notifyAll()")
  }
}


abstract class ThreadState {
  def locks: Seq[AnyRef]
}
trait CanContinueIfAcquiresLock extends ThreadState {
  def lockToAquire: AnyRef
}
case object Start extends ThreadState { def locks: Seq[AnyRef] = Seq.empty }
case object End extends ThreadState { def locks: Seq[AnyRef] = Seq.empty }
case class Wait(lockToAquire: AnyRef, locks: Seq[AnyRef]) extends ThreadState
case class SyncUnique(lockToAquire: AnyRef, locks: Seq[AnyRef]) extends ThreadState with CanContinueIfAcquiresLock
case class Sync(lockToAquire: AnyRef, locks: Seq[AnyRef]) extends ThreadState with CanContinueIfAcquiresLock
case class Running(locks: Seq[AnyRef]) extends ThreadState
case class VariableReadWrite(locks: Seq[AnyRef]) extends ThreadState
