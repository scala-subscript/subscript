package scalaParser.subscript.util

/**
 * A Stack used for bottom-top communication.
 * Children push their messages (maps) to the parent node to the stack.
 * After the parent node is finished with them, they are popped,
 * merged with the parent's message and the resulting single message
 * is pushed to the stack as the parent's message to grandparent.
 * Every node must push exactly one message; empty map is pushed if nothing is to push.
 */
trait CommunicationStack {

  /** Underlying stack. */
  def stack: Seq[Map[String, Any]]

  /** Push a new message to the stack. */
  def push(m: Map[String, Any]): Unit

  /**
   * Retrieves and merges the given number of messages.
   * back to the stack and returns the resulting CommunicationStack.
   * The values of the resulting map are List[Any]:
   * the values of different maps with same keys are collected
   * to the list.
   */
  def last(n: Int): Map[String, List[Any]]

  def last: Map[String, List[Any]] = last(1)

  /**
   * Pops given number of messages, merges them, pushes the resulting message to the stack.
   */
  def reduce(n: Int): Unit

}

class CommunicationStackImpl extends CommunicationStack {

  private[this] var _stack = List[Map[String, Any]]()

  def stack = _stack

  def push(m: Map[String, Any]) = _stack ::= m

  def last(n: Int) = merge(stack.take(n).reverse)

  def reduce(n: Int) {
    val payload = last(n)
    _stack = payload :: (stack drop n)
  }


  def merge(ms: Seq[Map[String, Any]]): Map[String, List[Any]] = {
    ms.foldLeft(Map[String, List[Any]]()) {(acc, next) =>
      (acc.toSeq ++ next.toSeq)
        .groupBy(_._1)
        .mapValues {_.map {_._2}.toList}
        .mapValues {_.flatMap {
            case x: List[_] => x
            case x          => List(x)
          }
        }
    }
  }

}