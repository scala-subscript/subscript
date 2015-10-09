package subscript.vm.executor.data

import scala.collection.mutable.ListBuffer
import subscript.vm._
import subscript.vm.executor._
import MessageHandlers._

class MessageHandlers(val lock: AnyRef) extends SafeCollection[MessageHandler] {

  def isEmpty = collection.isEmpty

  val collection = ListBuffer[MessageHandler]()
  def insert(e: MessageHandler): Unit = collection += e
  def remove(e: MessageHandler): Unit = collection -= e
}

object MessageHandlers {
  type MessageHandler = PartialFunction[CallGraphMessage, Unit]
}