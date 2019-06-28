package lockfree

import scala.annotation.tailrec

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head = createNode(0, None, isHead=true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = {
     // Traverser la liste pour trouver le premier noeud dont la valeur
     // satisfait le predicat pred

     def find(prev : Node, curr : Option[Node]):(Node, Option[Node]) = {
       
       //val oldPrev = prev.atomicState.get

       if(curr.isEmpty) 
         (prev, None)
       else {
         val currNode : Node = curr.get
         
         if(currNode.deleted) {
           prev.atomicState.compareAndSet((curr, false), (currNode.next, false))
           find(_head, firstNode)
         } else {
           if(pred(currNode.value))
             (prev, curr)
           else
             find(currNode, currNode.next)
         }
         
       }
         
     }

     find(_head, firstNode)
  }

  // Insert an element in the list.
  def insert(e: Int): Unit = {
    val (prev, act) = findNodeWithPrev(x => x > e)
    
    val newNode : Node = createNode(e, act)

    val ret = prev.atomicState.compareAndSet((act,false), (Some(newNode), false))
    
    if(!ret)
      insert(e)
  }

  // Checks if the list contains an element.
  def contains(e: Int): Boolean = 
    findNodeWithPrev(x => x == e)._2 != None

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  def delete(e: Int): Boolean = {
    val (prev, act) = findNodeWithPrev(x => x == e)
    
    if(act == None)
      false
    else {
      if(!act.get.mark)
        delete(e)
      else 
        true
    }
  }
    
}
