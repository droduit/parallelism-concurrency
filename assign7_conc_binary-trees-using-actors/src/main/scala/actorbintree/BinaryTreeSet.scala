package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case message : Operation =>
      root.!(message)
    case GC => {
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case message : Operation =>
      pendingQueue = pendingQueue.enqueue(message)
    case GC =>
    case CopyFinished => {
      
      while(!pendingQueue.isEmpty) {
        // Returns a tuple with the first element in the queue, and a new queue with this element removed.
        val (operation, removedElm) = pendingQueue.dequeue
        newRoot ! operation
        pendingQueue = removedElm
      }
      
      root ! PoisonPill
      root = newRoot
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    
    case msg @ Contains(requester: ActorRef, id: Int, e: Int) => {
      if(e != elem) { // Belongs to the left or right subtrees
        val pos:Position = if(e < elem) Left else Right
        
        if(subtrees.contains(pos))
          subtrees(pos) ! msg
        else
          requester ! ContainsResult(id, false)
      } else { // Is the element itself
        requester ! ContainsResult(id, !removed)
      }
    }
    
    case msg @ Insert(requester: ActorRef, id:Int, e: Int) => {
      if(e != elem) { // Element a insérer différent du current élément
        val pos:Position = if(e < elem) Left else Right
        
        subtrees.get(pos) match {
          // On a trouvé un subtree, on le laisse gérer le message
          case Some(subtree) =>  subtree ! msg
          // On ajoute un subtree à la position courante comme noeud avec la valeur requise
          case None => {
              subtrees = subtrees + (pos -> context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false)))
              requester ! OperationFinished(id)
          }
        }
        
      } else { // Element a insérer est l'élément courant
        // S'il est marqueé delete on le "dé"-marque
        if(removed)  removed = false
        requester ! OperationFinished(id)
      }
    }
    
    case msg @ Remove(requester: ActorRef, id: Int, e: Int) => {
      if(e != elem) { // Si l'élément à supprimer n'est pas l'élément courant (donc est a gauche ou a droite)
        val pos: Position = if(e < elem) Left else Right
        
        subtrees.get(pos) match {
          case Some(subtree) => subtree ! msg
          case None => requester ! OperationFinished(id)
        }
      } else { // L'élément a supprimer est l'élément courant
        // On le marque comme supprimé
        removed = true
        requester ! OperationFinished(id)
      }
    }
    
    case CopyTo(node) => {
      val children = subtrees.values.toSet
      
      if(children.isEmpty && removed) {
        context.parent ! CopyFinished
      } else {
        for(c <- children) c ! CopyTo(node)
        
        context.become(copying(children, removed))
        
        if(!removed) node ! Insert(self, elem, elem)
        else self ! OperationFinished(elem)
      } 
    }
    
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) => {
      if(expected.isEmpty) {
        context.parent ! CopyFinished
        context.become(normal)
      } else {
        context.become(copying(expected, true))
      }
    }
    
    case CopyFinished => {
      val newExpected = expected - sender
      
      if(newExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        context.become(normal)
      } else {
        context.become(copying(newExpected, insertConfirmed))
      }
    }
  }
}
