package lockfree

abstract class Node(val value: Int, initTail: Option[Node]) {

  // The type of mutable state held by node.
  type State = (Option[Node], Boolean)

  // The initial mutable state of the node.
  def initialState: State = (initTail, false)

  // The atomic variable that holds the state.
  val atomicState: AbstractAtomicVariable[State] = new AtomicVariable[State](initialState)

  // Function to read the next node from the state.
  def next: Option[Node] = atomicState.get._1

  // Should return true if the node was marked as deleted.
  def deleted: Boolean = atomicState.get._2
  
  // Should mark the node as deleted.
  def mark: Boolean =  {
    val (tail, marked) = atomicState.get
    
    if(marked)
      false 
    else {
      if(atomicState.compareAndSet((tail, marked), (tail, true)))
          true
      else
        mark
    }
    
   }
  
}