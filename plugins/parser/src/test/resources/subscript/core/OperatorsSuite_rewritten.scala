package subscript
import subscript.file

//import org.scalatest.FunSuite

import scala.util.{Try, Success, Failure}
import subscript.Predef._
import subscript.DSL._
import subscript.vm.{N_code_unsure, SimpleScriptDebuggerClass, ScriptNode, Script}
import subscript.vm.executor._
import subscript.vm.model.template.TemplateNode.Child
import subscript.vm.model.callgraph._

/**
 * This class is a test suite for the script operators as implemented in the SubScript VM. 
 * 
 * To run the test suite, you can 
 * 
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 *  - run from the command line, e.g.,
 *  
 *    qbin/scala -classpath build/quick/classes/subscript:build/quick/classes/subscript-supplements:build/deps/junit/junit-4.10.jar subscript.test.OperatorsSuiteApp 
 *    qbin/scala -classpath build/quick/classes/subscript:build/quick/classes/subscript-supplements:build/deps/junit/junit-4.10.jar subscript.test.OperatorsSuiteApp 45
 *  
 *    The second command line example specifies an index of a test as a parameter. 
 *    The program will then only run this test, while logging debug messages about the internal ongoings of the SubScript VM. 
 *    This is in particular useful after a failing test has occurred.
 *    One may call this "progression testing", as opposite of "regression testing"
 *  
 * Command line option -v is for verbose output; use -V for even more verbose output
 * 
 * *************************** 
 * High level methods
 * *************************** 
 * 
 * - testBehaviours: 
 *     tests many kinds of behaviors, of script expressions such as
 *     
 *      (-)
 *      a
 *      a b
 *      a;b
 *      a+b
 *      
 *    a, b, c are called "atoms"
 *    
 * - testFailingBehaviours: 
 *     behaviour cases that are known to fail may be marked using "FAIL:". 
 *   Then such behaviour cases should in a sense fail; if not an error has apparently been resolved
 *   and the FAIL: marking should be taken away. 
 *   So if the behavior in itself is OK, JUnit will report a problem of the type:
 *   java.lang.AssertionError: 376 a b+(+)&.&(-) : 
 *      after input '' should expect '1a' 
 *      This test had been marked as 'FAIL:' but it is passed anyway. Please remove the mark.
 *   
 *  
 * *************************** 
 * Test case specifications
 * *************************** 
 * 
 *   A script test case specification is a tuple of
 *   - a script lambda expression, e.g., "[a+b]"
 *   - a "behaviours" string, i.e. a space-separated-list of behaviour cases, e.g. "->a a->b ab"
 *   
 *   A behaviour case either matches one of the following:
 *   - "input"        => the scripts accepts the tokens of the given input and then ends successfully
 *   - "input->result => the script accepts the tokens in input, with the given result
 *      Here "input" is a sequence of tokens "a","b","c", e.g. "aaba"; 
 *      each token is to be eaten by the corresponding atom a,b,c.
 *      "result" is either "0" for deadlock, or a subsequence of "1abc", e.g. "1a" or "b"
 *         "1" means the script may terminate successfully, "a" means an "a" is expected, etc.
 *        
 *   Example test specification: 
 *   
 *      [a|b] -> "->ab a->1b b->1a ab ba"  
 *       
 *   BTW: a behaviour "a" is equivalent to "a->1" etc.
 *
 *  - each behaviour case may be prefixed with "FAIL:", to mark an expected failure. E.g., 
 *  
 *    [ (a b+(+))  & .  & (-) ] -> "->1a FAIL:a->b FAIL:ab->0"
 *  
 *  
 * *************************** 
 * Low level implementation 
 * *************************** 
 * 
 * main and JUnit will each call testBehaviours which in turn calls for each lambda script testScriptBehaviours 
 * 
 * testScriptBehaviours creates a script structure for the vm, and interprets the specified behaviours 
 * - a referred behaviour "=expr" results in a recursive call to testScriptBehaviours
 * - other behaviours strings are split into single behaviours, and then fed into the method testScriptBehaviour, 
 *   together with the script structure
 * 
 * testScriptBehaviour executes the handed script using an executor and checks the execution with the given specification.
 * The execution should end in success (as witnessed by executor.hasSuccess) if the specification says so,
 * This is a bit tricky.
 * Note that the specified input string is to be eaten by the atoms. 
 * An atom is programmed as the following script expression:
 *   @expect(there,name): {?tryAccept(here, name)?}
 * The "expect(there,name)" annotation puts the atom name in the list "expectedAtoms"; it also makes sure
 * that the atom name is removed from "expectedAtoms" when the atom script expression deactivates (using method "unexpect")
 * 
 * Note that the script execution has 2 kinds of phases that are relevant here:
 * 1 - activation and deactivation, when the expectations are added and removed
 * 2 - try out of the unsure code fragments, with method "tryAccept"
 * 
 * The variable "expectedAtomsAtEndOfInput" should contain the expectedAtoms 
 * as valid just after input processing has stopped (either because the input was at its end, or because of unexpected input)
 * At an activation belonging to a phase 1, i.e. in the "expect" method, this expectedAtomsAtEndOfInput is set to null
 * At the start of a phase 2, i.e. at the first call to "tryAccept", identified by expectedAtomsAtEndOfInput==null,
 * expectedAtomsAtEndOfInput is set to expectedAtoms at that moment.
 * In case this is the last phase2, this version of expectedAtomsAtEndOfInput will be used in testScriptBehaviour
 * to compare with the specified result.
 * 
 * Likewise, the variable "scriptSuccessAtEndOfInput" should contain the contain the success state of the script
 * as valid just after input processing. Both this variable and expectedAtomsAtEndOfInput are only set in case there 
 * is any atom expected; for other scripts such as "(-)" and "(+)" the end value of executor.hasSuccess will do. 
 * 
 * With a given (input,result) specification, the script execution can come to an end in the following ways:
 * 1 - all input has been accepted
 *     the specified result should match expectedAtomsAtEndOfInput and match executor.hasSuccess 
 * 2 - an input token had not been expected; then the test fails
 * In both cases, all calls to tryAccept give the UnsureCodeFragment result status "Failure" so that the 
 * script call graph deactivates totally, so that the script execution ends
 * 
 * In other cases there must be a call to tryAccept for an atom that matches the current input token. 
 * Then the UnsureCodeFragment result status becomes "Success"; the input stream goes to the next token, and the 
 * accepted token is logged in acceptedTokens. The latter variable is used later to see whether the entire specified input
 * has been consumed.
 * 
 * All other calls to tryAccept result in the UnsureCodeFragment result status becoming "Ignore"; these code fragments
 * stay in the queue for the time being so that they may be retried; it is also possible that they are excluded because 
 * a "relatively exclusive" UnsureCodeFragment succeeds (e.g., as with a in "a+b" when b happens).
 * 
 * ********************
 * Notes
 * ********************
 * expectedAtoms etc are lists rather than sets; in principle multiple instances of atom may be expected synchronously.
 * A MultiSet would therefore do better than a Set, but since it is not in the standard Scala library we use a List here.
 * 
 */
//@RunWith(classOf[JUnitRunner])
abstract class OperatorsSuiteBase {

  var doVerboseLevel = 0
  def doSilent       = doVerboseLevel < 0
  def doVerbose      = doVerboseLevel > 0
  
  /*
   * Behaviour operators characterized by their logic property
   * Logical-Or  means that (-) ("zero") is the neutral element
   * Logical-And means that (+) ("one")  is the neutral element
   */
  val logicalAnd_string = "; & &&"
  val logicalOr_string  = "+ | || /"
    
  val logicalAndOperators = logicalAnd_string.split(" ")
  val logicalOrOperators  = logicalOr_string .split(" ") 
  val behaviorOperators   = logicalAndOperators.toList:::logicalOrOperators.toList
  
  var testIndexForDebugging = -1
  def debug = testIndexForDebugging >= 0
  
  val FAILmarker = "FAIL:"
  
  /*
   * Low level stuff
   */
  def testScriptBehaviours(scriptDef: ScriptNode[Any], scriptString: String, behaviours: String) {
    
    import scala.util.matching.Regex
    val pattern = new Regex(" +") // replace all multispaces by a single space, just before splitting behaviours:
    for (behaviour<-(pattern replaceAllIn(behaviours," ")).split(" ")) {
      val inputAndResult = behaviour.split("->")
      var input          = inputAndResult(0)
      val hasFAILmarker  = input.startsWith(FAILmarker)
      if (hasFAILmarker)   input = input.substring(FAILmarker.length)
      val expectedResult = if (inputAndResult.length>1) inputAndResult(1) else "1"
      testScriptBehaviour(scriptDef, scriptString, input, expectedResult, expectTestFailure = hasFAILmarker)
    }
  }

  val mainThread = Thread.currentThread
  
  var acceptedAtoms : String       = null
  var inputStream   : Stream[Char] = null
  var expectedAtoms : List[Char] = null
  
  var expectedAtomsAtEndOfInput: Option[List[Char]] = None
  var scriptSuccessAtEndOfInput: Option[Boolean]    = None
  var executor: ScriptExecutor[Any] = null
  var currentTestIndex = 0

  def testScriptBehaviour(scriptDef: ScriptNode[Any], scriptString: String, input: String, expectedResult: String, expectTestFailure: Boolean) {
    
    currentTestIndex += 1
    
    if (testIndexForDebugging > 0 && 
        testIndexForDebugging != currentTestIndex) return
 
    lazy val afterInput    = if(input=="") "" else s"after input: $input"
    lazy val failureString = /*if(expectTestFailure) "Fails as marked" else*/ "Fails"
    var testInfo      = f"test $currentTestIndex%3d:   $scriptString%-21s   $afterInput%-18s should do: $expectedResult%4s"

		val expectedResultFailure = expectedResult(0)=='0'
		val expectedResultSuccess = expectedResult(0)=='1'
		val expectedResultAtoms   = (if (expectedResultSuccess||expectedResultFailure) expectedResult.drop(1) else expectedResult)
		                            .sortWith(_<_).mkString

    if (expectedResultFailure && !expectedResultAtoms.isEmpty)                              
       println(s"$testInfo - Error in test specification: no atoms should be expected in combination with 0") // very unlikely to occur
    else {
      if (debug)         println(testInfo)
      else if (doVerbose) {print(testInfo); testInfo = ""}
        
      acceptedAtoms         = ""
      inputStream           = scala.io.Source.fromString(input).toStream
      expectedAtoms         = Nil
      expectedAtomsAtEndOfInput = None
      scriptSuccessAtEndOfInput = None
      
      executor     = new CommonScriptExecutor
      val debugger: SimpleScriptDebuggerClass = null //if (debug) new SimpleScriptDebuggerClass else null
      if (doVerbose && debug) {executor.traceLevel = 2; debugger.traceLevel = if (doVerboseLevel==1) 3 else 4}
      
      val watchDogThread = new Thread(new Runnable {def run() { 
          if (!Thread.interrupted)
          try {Thread.sleep(1500); doPostMortemDump(testInfo)}
          catch {case e: InterruptedException => /*println("Interrupted") this is actually Ok*/
                 case e: Throwable => e.printStackTrace
          }
      }})
      try {
        watchDogThread.start
        _execute(scriptDef, debugger, executor) ///////////////////////////////// Script Execution //////////////////////////
      }
      catch {case e: Throwable => println(s"$testInfo - an exception occurred"); throw e}
      finally {/*println(s"Interrupting...");*/ watchDogThread.interrupt()}
      
      val executionSuccess = scriptSuccessAtEndOfInput.getOrElse(executor.hasSuccess)
      val expectedAtomsAtEndOfInputString = expectedAtomsAtEndOfInput.getOrElse(Nil).sortWith(_<_).mkString
      
      var hadTestFailure = false
      if (executionSuccess                != expectedResultSuccess
      ||  expectedAtomsAtEndOfInputString != expectedResultAtoms
      ||  acceptedAtoms != input) 
      {
         hadTestFailure = true
         val expectedItemsStr = f" does: ${(if(executionSuccess) "1" else "")+expectedAtomsAtEndOfInputString}%4s"
         val acceptedInputStr = if (acceptedAtoms == input) "" else s" accepted input: $acceptedAtoms"

         if (expectTestFailure) {
           if (!doSilent||debug) {
             println(s"$testInfo - $failureString;$acceptedInputStr$expectedItemsStr; already marked as FAIL")
           }
         }
         else {
           println(s"$testInfo - $failureString;$acceptedInputStr$expectedItemsStr")
         }
      }    
      if (!hadTestFailure) {
         if (expectTestFailure)       println(s"$testInfo - Ok although marked as 'FAIL:'. Please remove the mark")
         else if (doVerbose || debug) println(s"$testInfo - Ok")
      }
    }
  }

  def doPostMortemDump(testInfo: String) = {
    println(s"$testInfo - timeout reached") /*; interrupted = ${Thread.interrupted}"*/
    val debugger: SimpleScriptDebuggerClass = null //new SimpleScriptDebuggerClass
    debugger.attach(executor)
    debugger.traceLevel = 4
    debugger.traceTree
    debugger.traceMessages
    mainThread.getStackTrace.foreach(println)
  }
  
  // utility method: remove 1 occurrence of elt from list; see http://stackoverflow.com/a/5640727
  def remove1Element[T](list: List[T], elt: T): List[T] = list diff List(elt)
  
  // add expectation of the given atom; also prepares for the symmetric to unexpect during the inevitable deactivation
  def expect   (where: N_code_unsure[_], atomName: Char) {where.onDeactivate(unexpect(where, atomName)); expectedAtoms ::= atomName}
  // remove expectation of the given atom
  def unexpect (where: N_code_unsure[_], atomName: Char) {expectedAtoms = remove1Element(expectedAtoms, atomName)}
  
  // try to accept the token from the input (if any); match it to the current atom.
  def tryAccept(where: N_code_unsure[_], atomName: Char) {
    if (inputStream.isEmpty || !expectedAtoms.contains(inputStream.head)) {
       where.result = ExecutionResult.Failure; 
       if (expectedAtomsAtEndOfInput== None) {
           expectedAtomsAtEndOfInput = Some(expectedAtoms)
           scriptSuccessAtEndOfInput = Some(executor.hasSuccess)
           if (debug) println(s"inputStream.isEmpty=${inputStream.isEmpty} expectedAtoms=${expectedAtoms.mkString}"
                             +     (if (inputStream.isEmpty) "" else s" inputStream.head=${inputStream.head}") 
                             +                                          s" scriptSuccess=$scriptSuccessAtEndOfInput")
       }
    }
    else if (inputStream.head == atomName) {inputStream = inputStream.drop(1); acceptedAtoms += atomName}
    else                                   {where.result = ExecutionResult.Ignore}
    
    //println("<<<tryAccept: "+where+" inputStream "+ (if (inputStream.isEmpty) "Empty" else ("head = "+inputStream.head))+"  has success = "+where.hasSuccess)
  } 

  //  script expression structure for an atom. It essentially comes down to the following script:
def atom(name: Char) = subscript.DSL._script[Any](None, Symbol("atom")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_unsure[Any], subscript.vm.model.template.concrete.T_code_unsure[Any]](here => {
  implicit val there: subscript.vm.N_code_unsure[Any] = here.there;
subscript.DSL._maybeVarCall("expect(subscript.DSL._maybeVarCall(\"there\"),subscript.DSL._maybeVarCall(\"name\"))")
}).apply(subscript.DSL._unsure[Any] (_node => {
  implicit val here = _node
val n: Any = subscript.DSL._maybeVarCall("name"); subscript.DSL._maybeVarCall("tryAccept(subscript.DSL._maybeVarCall(\"here\"), subscript.DSL._maybeVarCall(\"name\"))"); subscript.DSL._maybeVarCall("$success_=(subscript.DSL._maybeVarCall(\"n\"))")
}, true))}
def a = subscript.DSL._script[Any](None, Symbol("a")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("'a'")))}
def b = subscript.DSL._script[Any](None, Symbol("b")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("'b'")))}
def c = subscript.DSL._script[Any](None, Symbol("c")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("'c'")))}
def d = subscript.DSL._script[Any](None, Symbol("d")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("'d'")))}
def e = subscript.DSL._script[Any](None, Symbol("e")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("'e'")))}
def v = subscript.DSL._script[Any](None, Symbol("v")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("'v'")))}
def p(v: Any) = subscript.DSL._script[Any](None, Symbol("p")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => atom(subscript.DSL._maybeVarCall("v.toString.charAt(subscript.DSL._maybeVarCall(\"0\"))")))}

  def scriptBehaviourList_for_debug: Seq[(Script[Any],String)]
  def scriptBehaviourList          : Seq[(Script[Any],String)]

  def scriptBehaviourMap = scriptBehaviourList.toMap

  // Tests behaviours  marked with FAIL: have been known to fail. 
  //   if such a behaviour unexpectedly passes the test then this results in a JUnit failure.
  //   That should be a trigger to regard the issue underlying to the FAIL: marking to be resolved.
  //   So then the FAIL: marking should be removed and JUnit will be able to proceed further.

  def testBehaviours: Unit = {
    val behaviours = if (testIndexForDebugging==0) scriptBehaviourList_for_debug else scriptBehaviourList
    for ( (key, behaviours) <- behaviours) {
      val aScript = key.asInstanceOf[ScriptNode[Any]]
      val bodyString = toScriptBodyString(aScript)
      testScriptBehaviours(aScript, bodyString, behaviours.asInstanceOf[String])
    }
  }
  
  /*
   * High level calls that will be tested:
   */
  //testBehaviours
  //testLogicalOr
  //testLogicalAnd

}


object OperatorsSuiteApp extends OperatorsSuite {
  
  def usage = println(
"""Usage: <scala> subscript.test.OperatorsSuite [option] [testIndex]

Options:
  -s: silent: do not report tests that fail expectedly
  -v: verbose
  -V: more verbose
      
If a testIndex is supplied, only that test is run, in debug mode (which means very verbose)
If that testIndex is 0 the tests marked for debugging are run.
Else all tests are run  
""")

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val argsList = args.toList
      val head::tail = argsList
      val numList = head match {
                  case "-s" => doVerboseLevel = -1; tail
                  case "-v" => doVerboseLevel =  1; tail
                  case "-V" => doVerboseLevel =  2; tail
                  case   _  => argsList
      }
      numList match {
        case h::t => testIndexForDebugging = Try(h.toInt).getOrElse{usage; return}
        case   _  =>
      }
    }
    testBehaviours
  }
}



class OperatorsSuite extends OperatorsSuiteBase {

  /*
   * scriptBehaviourMap: relation between scripts and outcomes
   *   keys are script strings, which should also be keys in the scriptBodyMap, so that bodies can be found
   *   the outcomes are a input traces in relation to their outcomes, see #testScriptBehaviour
   */
  val scriptBehaviourList = List(subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._deadlock}     -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._empty}     -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._neutral}    -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._break}   -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._optionalBreak}       -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._optionalBreak_loop}      -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._loop}     -> "->1"
                  
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)}       -> "->a a"
                               
   //  a op b 
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->ab a b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->ab a->b  b->a  ab ba"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}    -> "->ab a->b  b->a  ab ba"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->ab a->1b b->1a ab ba"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}    -> "->ab a b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->ab a b"
                               
   // a op neutral
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
                             
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a a"
                               
   // a op antineutral
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}   -> "->a a->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}   -> "->a a->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a a->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock)}  -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->0"
                             
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}   -> "->1a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->1a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}   -> "->1a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->1a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}  -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._empty)}   -> "->1a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->1"
                               
   // break a
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))} -> "->1"
                            
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))} -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->0"
                               
   // while(pass<1) a
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))} -> "->a a"

   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))} -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<1")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a a"
                               
   // while(pass<2) a
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->a  a->a aa"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->aa a->a aa"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))} -> "->aa a->a aa"

   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->aa a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->aa a->1a aa"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))} -> "->aa a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("pass<2")
}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}  -> "->aa a"
                               
   // 2 operand sequences with iterator or break or optional break, 
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}     -> "->1a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}    -> "->1a a->1a aa->1a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}   -> "->a  a->a  aa->a"
                  
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._break)} -> "->a  a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak)}     -> "->a  a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak_loop)}    -> "->a  a->1a aa->1a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._loop)}   -> "->a  a->a  aa->a"
   
   // 3 operand sequences with iterator or break or optional break, 
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._break)}   -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak)}       -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._optionalBreak_loop)}      -> "->a  a->b ab->1a aba->b abab->1a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._loop)}     -> "->a  a->b ab->a  aba->b abab->a"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->a  a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}       -> "->a  a->1b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}      -> "->a  a->1b ab->a aba->1b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->a  a->b  ab->a aba->b"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}       -> "->1a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}      -> "->1a a->b ab->1a aba->b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}     -> "->a  a->b  ab->a aba->b"
   
   // 2 level nested 2 operand sequences with iterator or break or optional break, 
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._break))}  -> "->a  a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._optionalBreak))}  -> "->a  a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._optionalBreak_loop))}  -> "->a  a->b ab->1b abb->1b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._loop))}  -> "->a  a->b  ab->b"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._break)}  -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak)}  -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak_loop)}  -> "->a  a->b ab->1a aba->b abab->1a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._loop)}  -> "->a  a->b ab->a  aba->b abab->a"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->a  a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->a  a->1b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->a  a->1b ab->1b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->a  a->b  ab->b"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._break), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->a  a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->a  a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak_loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->a  a->ab aa->ab ab aab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->a  a->a  aa->a"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._break, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->1a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._optionalBreak_loop, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->1a a->b ab->1a aba->b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}  -> "->a  a->b  ab->a aba->b"
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._break, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}  -> "->b b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}  -> "->ab  a->b ab b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}  -> "->ab  a->ab aa->ab b ab aab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}  -> "->a  a->a  aa->a"

   // parallel composition
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->ab  a->ab  aa->ab  b->a  ba->a  ab->a  aba->a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)))}   -> "->ab  a->ab  aa->ab  b->a  ba->a  ab->a  aba->a"  // commutative
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}   -> "->ab  a->ab  aa->ab  b->1a  ba->1a  ab->1a  aba->1a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)))}   -> "->ab  a->ab  aa->ab  b->1a  ba->1a  ab->1a  aba->1a"  // commutative
                      
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}       -> "->abc a->bc  b->ac  c->ab  ab->c  ac->b  ba->c  bc->a  ca->b  cb->a  abc acb bac bca cab cba" 
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}     -> "->abc a->bc  b->ac  c->ab  ab->c  ac->b  ba->c  bc->a  ca->b  cb->a  abc acb bac bca cab cba"  
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}       -> "->abc a->1bc b->1ac c->1ab ab->1c ac->1b ba->1c bc->1a ca->1b cb->1a abc acb bac bca cab cba"  
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}     -> "->abc a b c"  
                                       
   // disruption with compound left hand operand
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}     -> "->abc a b c"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._par_or(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}     -> "->abc a->1bc b->1ac c ac bc ab ba"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}     -> "->ac a->bc ab c ac" 
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._empty)}   -> "->1a a->1b ab"
   
   // optional break
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak)}             -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak_loop)}             -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}           -> "->a a->bc ab ac"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak_loop)}             -> "->a  a->ab ab aa->ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._optionalBreak, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}             -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d)))}         -> "->a a->bc  ab     ac->d  acd"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d)))}         -> "->a a->bc  FAIL:ab->1c ac->bd abc->d  abcd acb->d  acd->b  acbd acdb"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d)))}         -> "->a a->bc  ab->1c ac->bd abc->1d abcd acb->1d acd->1b acbd acdb"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}               -> "FAIL:->1a a->b  ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}               -> "->1a FAIL:a->b  ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}               -> "FAIL:->1a a->b  ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._disrupt(subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d)))}     -> "FAIL:->1a a->bc ab ac->d acd"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._empty)}       -> "->a a->1b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._empty)}       -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._deadlock)}       -> "->a a->b ab->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak, subscript.DSL._deadlock)}       -> "->a a->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._empty), subscript.DSL._optionalBreak, subscript.DSL._deadlock)} -> "->1a a->b ab->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_and2(subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._empty), subscript.DSL._optionalBreak, subscript.DSL._deadlock)} -> "->1a a->0"

  
   // if
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("true")
})(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}            -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("true")
})(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}          -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("false")
})(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}            -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("false")
})(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}          -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if_else (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("true")
})(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))} -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if_else (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("true")
})(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d)))} -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if_else (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("false")
})(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))} -> "->c c"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._if_else (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("false")
})(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d)))} -> "->c c->d cd"

   // do
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}              -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)))}            -> "->a a->b ab->c abc"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->a a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->a a->b ab->c abc"
   
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}              -> "->b b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._empty, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)))}            -> "->b b->c bc"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._empty, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._empty, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->b b"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._empty, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->b b->c bc"
   
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}              -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._deadlock, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)))}            -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->d d"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._deadlock, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->d d->e de"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._deadlock, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->d d"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._deadlock, subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->d d->e de"
   
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}              -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)))}            -> "->a a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->a a->d ad"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_else(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->a a->d ad->e ade"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}   -> "->a a->d ad"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._do_then_else(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._deadlock), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => e)))}   -> "->a a->d ad->e ade"
   
   // Threaded code fragments. TBD: check whether the test mechanism can handle this; maybe not
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node

}, true), subscript.DSL._optionalBreak_loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}         -> "->a a->ab aa->ab aaa->ab ab aab"

   // launching
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._launch_anchor(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}             -> "a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._launch_anchor(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}         -> "ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch_anchor(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}          -> "ab"
   
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._launch(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))}              -> "a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._launch(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))}              -> "a->b ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}            -> "->ab a->b b->a ab ba"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}         -> "->ac a->bc  c->ad  ab->c  ac->bd  ca->bd  cd->a  abc->d  acb->d acd->b cab->d cad->b cda->b abcd  acbd acdb cabd cadb cdab"
   
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch_anchor(subscript.DSL._launch(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}    -> "->a a->b abc"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch_anchor(subscript.DSL._seq(subscript.DSL._launch(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}  -> "->ac a->bc  c->a ab->c ac->b ca->b abc->d acb->d cab->d abcd acbd cabd"

   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.launch(subscript.DSL._maybeVarCall(\"subscript.DSL._script[Any](None, Symbol(\\\"lambda\\\")){(_node: subscript.vm.Script[Any]) =>\\n  implicit val script = _node\\nsubscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)}\"))")
}, true)} -> "a"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.launch(subscript.DSL._maybeVarCall(\"subscript.DSL._script[Any](None, Symbol(\\\"lambda\\\")){(_node: subscript.vm.Script[Any]) =>\\n  implicit val script = _node\\nsubscript.DSL._seq(subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}\"))")
}, true)} -> "ab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.launch(subscript.DSL._maybeVarCall(\"subscript.DSL._script[Any](None, Symbol(\\\"lambda\\\")){(_node: subscript.vm.Script[Any]) =>\\n  implicit val script = _node\\nsubscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)}\"))")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))} -> "->ab a->b b->a ab ba"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.launch(subscript.DSL._maybeVarCall(\"subscript.DSL._script[Any](None, Symbol(\\\"lambda\\\")){(_node: subscript.vm.Script[Any]) =>\\n  implicit val script = _node\\nsubscript.DSL._seq(subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}\"))")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))} -> "->ac a->bc  c->ad  ab->c  ac->bd  ca->bd  cd->a  abc->d  acb->d acd->b cab->d cad->b cda->b abcd  acbd acdb cabd cadb cdab"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch_anchor(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.launch(subscript.DSL._maybeVarCall(\"subscript.DSL._script[Any](None, Symbol(\\\"lambda\\\")){(_node: subscript.vm.Script[Any]) =>\\n  implicit val script = _node\\nsubscript.DSL._seq(subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}\"))")
}, true)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))}    -> "->a a->b abc"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._launch_anchor(subscript.DSL._seq(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.launch(subscript.DSL._maybeVarCall(\"subscript.DSL._script[Any](None, Symbol(\\\"lambda\\\")){(_node: subscript.vm.Script[Any]) =>\\n  implicit val script = _node\\nsubscript.DSL._seq(subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._maybeCall(\\\"\\\", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))}\"))")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => d))}  -> "->ac a->bc  c->a ab->c ac->b ca->b abc->d acb->d cab->d abcd acbd cabd"
   
   // failure
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._unsure[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.fail")
}, true)}        -> "->0"
   
   // priorities
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"there.priority\",  subscript.DSL._maybeVarCall(\"1\"))")
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true)), subscript.DSL._seq(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true), subscript.DSL._deadlock))} -> "->1"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true), subscript.DSL._seq(subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"there.priority\",  subscript.DSL._maybeVarCall(\"1\"))")
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true)), subscript.DSL._deadlock))} -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"there.priority\", subscript.DSL._maybeVarCall(\"-1\"))")
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true)), subscript.DSL._seq(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true), subscript.DSL._deadlock))} -> "->0"
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true), subscript.DSL._seq(subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"there.priority\", subscript.DSL._maybeVarCall(\"-1\"))")
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

}, true)), subscript.DSL._deadlock))} -> "->1" 
   
   // Various
   ,subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node

}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._loop), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c), subscript.DSL._loop))}  -> "->ac FAIL:a->bc FAIL:ab->ac c->ac cc->ac FAIL:ca->bc FAIL:ac->bc FAIL:acc->bc FAIL:acb->ac"
   
   // problem in LookupFrame2: after 2 iterations an optional success of the guard  (a . b . c)
   // caused a "bypass" of the searchcommand (d) so that the search (e) was activated
   //, [(a;.;b;.;c) d; e ] -> "->a a->bd ab->cd abc->d ad->e abd->e abcd->e ade abde abcde"
   //, [(a..) b; c ] -> "->a a->ab aa->ab aaa->ab ab->c aab->c aaab->c abc aabc aaabc"
 
 )

   
  val scriptBehaviourList_for_debug = List(subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => c)))} -> ""
      
      
   //  [ {Hello} ~~(msg:String)~~> {println(msg)} ]  -> "a->1"
 //, [{println("starting...");"hello"} ~~~(msg:String)~~~> {println(msg)} ]  -> "->1"
 //, [a ~~(t:String)~~>p(t)]       -> "a->a"
 //, [b ~~(t:String)~~>p(t)]       -> "b->b"
  )

  // unfortunately we cannot test event handling code fragments, for the time being, as in , [ {. .} / .. ]    -> "???"

  
}
