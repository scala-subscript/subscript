package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import subscript.vm.model.callgraph.CallGraphNode

/*
 * Simple text based script debugger
 *
 * Operation mode 1: pass the class to be debugged as parameter from the command line
 *
 *   execute the main method of SimpleScriptDebuggerObject
 *   with first argument: the package+class name of the object to be debugged
 *   and later argument: the arguments to be passed to the debugged object
 *
 * Operation mode 2: pass the debugger as an argument to the subscript.vm._execute method:
 *
 * 	  val debugger = new SimpleScriptDebugger
 *    _execute(scriptDef, debugger, executor)
 */

object SimpleScriptDebugger extends SimpleScriptDebuggerClass {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) return

    val classNameIdx =
      if (args(0) == "-t") {
        throttle = args(1).toInt
        2
      } else 0

    ScriptExecutorFactory.addScriptDebugger(this)
    val className = args(classNameIdx)
    try {
      val c = Class.forName(className)
      val m = c.getMethod("main", classOf[Array[String]])
      m.invoke(null, args.drop(classNameIdx))
    }
    catch {
      case e: ClassNotFoundException => println("Could not find class "+className)
      case e: java.lang.reflect.InvocationTargetException => throw e.getTargetException
      case other: Throwable => other.printStackTrace
    }
  }
}

class SimpleScriptDebuggerClass extends MsgListener {
  protected var throttle: Long = -1

  private var _scriptExecutor: ScriptExecutor[_] = null
  def scriptExecutor = _scriptExecutor
  override def attach(p: MsgPublisher) {
    super.attach(p)
    _scriptExecutor = p.asInstanceOf[ScriptExecutor[_]]
  }

  def callGraphMessages = scriptExecutor.msgQueue.collection
  def rootNode          = scriptExecutor.rootNode

  // some tracing stuff
  var nSteps = 0
  var maxSteps = 0 // 0 means unlimited

  val treeTraceLevel = 3
  val highTraceLevel = 4

  var traceLevel = 2 // 0-no tracing; 1-message handling; 2-message insertion+handling; 3 - every step a tree; highTraceLevel - every step expected messages
  def trace(level:Int,as: Any*) = {
    if (throttle > 0) Thread sleep throttle

    if (traceLevel>=level) {
      as.foreach {a=>print(a.toString)};
      println
      //traceMessages
    }
    if (traceLevel >= treeTraceLevel) traceTree
    if (traceLevel >= highTraceLevel) traceMessages
    if (maxSteps>0 && nSteps > maxSteps) {println("Exiting after "+nSteps+"steps"); System.exit(0)}
    nSteps += 1
  }
  def traceTree: Unit = {
    var j = 0;
	  def traceTree[T <: TemplateNode](n: CallGraphNode, branches: List[Int], depth: Int): Unit = {
	    for (i<-1 to 30) {
	      print(if(i==depth)"*"else if (branches.contains(i)) "|" else if(j%5==0)"-"else" ")
	    }
	    j+=1
	    println(n.infoString)
	    n match {
	      case p:CallGraphNode =>
	        val pcl=p.children.length
	        p.children.foreach{ c =>
	          var bs = if (c.template.indexAsChild<pcl-1)
	                    depth::branches
	                    else branches
	          traceTree(c, bs, depth+1)}
	      case _ =>
	    }
	  }
	if (traceLevel >= 1) traceTree(rootNode, Nil, 0)
  }
  def traceMessages: Unit = {
	if (traceLevel >= 1) {
	  println("=== Messages ===")
	  callGraphMessages.foreach(m=>println(m.toFormattedString))
	  println("=== End ===")
	}
  }


  override def messageHandled(m: CallGraphMessage): Unit = {
        trace(1,">> ",m)
        m match {
          case AAToBeExecuted(_) =>
            if (traceLevel < treeTraceLevel) traceTree     // else already done in trace(1,...). messy but it works
            if (traceLevel < highTraceLevel) traceMessages
          case _ =>
        }
  }
  def messageQueueMsgs: String = if (traceLevel >= 999) {"\n"+callGraphMessages.toList.map(_.toString).mkString("\n") } else ""
  def messageQueueMsgs(m: CallGraphMessage): String = if (m.isInstanceOf[AAToBeExecuted[_]]) messageQueueMsgs else ""
  
  override def messageQueued      (m: CallGraphMessage                 ) = trace(2, "++ ", m, messageQueueMsgs)
  override def messageDequeued    (m: CallGraphMessage                 ) = trace(2, "-- ", m, messageQueueMsgs)
  override def messageContinuation(m: CallGraphMessage, c: Continuation) = trace(2, "** ", c)
  override def messageAwaiting: Unit = {traceTree; traceMessages}
}
