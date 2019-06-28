package lockfree

import instrumentation.LockFreeMonitor
import scala.collection._
import java.util.concurrent.atomic._


abstract class AbstractSortedList extends LockFreeMonitor {
  
  def createNode(value: Int, tail: Option[Node], isHead: Boolean = false) = new Node(value, tail) {
    override def toString = if(isHead) "HEAD" else super.toString
  }
  
  def firstNode: Option[Node] 
  
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node])
  
  def insert(e: Int): Unit
  
  def delete(e: Int): Boolean
    
  def toList: List[Int] = {
    var curr = firstNode
    var list = List[Int]()
    while(curr.nonEmpty) {
      if(!curr.get.deleted) list :+= curr.get.value
      curr = curr.get.next 
    }
    list
  }
}

