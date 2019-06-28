package pubsub.collection

import instrumentation.Monitor


trait InternalBuffer[T] {
  def update(index: Int, elem: T): Unit
  def apply(index: Int): T
  def delete(index: Int): Unit
  val size: Int
}


abstract class AbstractBoundedBuffer[T](bufferSize: Int) extends Monitor {
  require(bufferSize > 0)

  def put(element: T): Unit
  def take(): T

  val buffer: InternalBuffer[T] = new InternalBuffer[T] {
    private val buffer: Array[Option[T]] = new Array(bufferSize)
    def update(index: Int, elem: T): Unit = buffer(index) = Some(elem)
    def apply(index: Int): T = buffer(index).get
    def delete(index: Int): Unit = buffer(index) = None
    val size = bufferSize
  }

  def head: Int = _head
  def head_=(e: Int): Unit = _head = e
  def count: Int = _count
  def count_=(e: Int): Unit = _count = e

  private var _head = 0;
  private var _count = 0;
}
