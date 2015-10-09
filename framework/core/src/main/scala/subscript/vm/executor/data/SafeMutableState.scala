package subscript.vm.executor.data

import scala.collection.mutable.ListBuffer

/**
 * Represents a (not thread-safe) operation.
 */
trait Operation {
  def commit(): Unit
}

/**
 * This trait represents a collection with unsafe (fast) and safe (slow)
 * access modes. Safety implies synchronization.
 * It is supposed to be used in unsafe way from only one thread;
 * all other threads should access it in safe manner.
 */
trait SafeMutableState {
  
  /**
   * This is thread-safety stragegy implementation.
   */
  val lock: AnyRef
  
  /**
   * Buffer records changes that are meant for the collection. The changes
   * are stored in chronological order in this format.
   */
  private val buffer = ListBuffer[Operation]()
  
  /**
   * Safely push new operation for further execution. 
   */
  def push[O <: Operation](op: O): Unit = lock.synchronized {buffer += op}
  
  /**
   * This operation commits recorded changes into the underlying collection.
   * It must be called only when `lock` is taken. 
   */
  def commit() = {
    buffer.foreach(_.commit())
    buffer.clear()
  }
  
  def unsupportedOperation(msg: String): Nothing = throw new UnsupportedOperationException(msg)
  def unsupportedOperation: Nothing = unsupportedOperation("This operation is not supported")
  
}

trait SafeCollection[A] extends SafeMutableState {self =>
  
  def isEmpty: Boolean
  
  private case class Insert(e: A) extends Operation {def commit() = self insert e}
  private case class Remove(e: A) extends Operation {def commit() = self remove e}
  
  def insert(e: A): Unit
  def remove(e: A): Unit
  
  def sInsert(e: A) = this push Insert(e)
  def sRemove(e: A) = this push Remove(e)
}