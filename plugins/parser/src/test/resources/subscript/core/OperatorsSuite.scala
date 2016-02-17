package subscript
import subscript.file

//import org.scalatest.FunSuite

import scala.util.{Try, Success, Failure}
import subscript.Predef._
import subscript.DSL._
import subscript.vm.{N_code_unsure, SimpleScriptDebuggerClass, Script, Script}
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
  def testScriptBehaviours(scriptDef: Script[Any], scriptString: String, behaviours: String) {
    
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

  def testScriptBehaviour(scriptDef: Script[Any], scriptString: String, input: String, expectedResult: String, expectTestFailure: Boolean) {
    
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
  script ..
    atom(name: Char) = @{expect(there,name)}: {?val n: Any = name; tryAccept(here, name); $success_=(n) ?}
    a = atom('a')
    b = atom('b')
    c = atom('c')
    d = atom('d')
    e = atom('e')
    v = atom('v')
    p(v:Any) = atom(v.toString.charAt(0))

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
      val aScript = key.asInstanceOf[Script[Any]]
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
  val scriptBehaviourList = List( // list, not a map, to remain ordered

     // [a {*println("Hello"); here.onSuccess{println("onSuccess")}*} .. ; b]   -> "->a  a->ab aa->ab aaa->ab ab aab"   ,
      
   // simple terms
     [(-)]     -> "->0"
   , [(+)]     -> "->1"
   , [(+-)]    -> "->1"
   , [break]   -> "->1"
   , [.]       -> "->1"
   , [..]      -> "->1"
   , [...]     -> "->1"
                  
   , [a]       -> "->a a"
                               
   //  a op b 
   , [a;b]     -> "->a a->b ab"
   , [a+b]     -> "->ab a b"
   , [a&b]     -> "->ab a->b  b->a  ab ba"
   , [a&&b]    -> "->ab a->b  b->a  ab ba"
   , [a|b]     -> "->ab a->1b b->1a ab ba"
   , [a||b]    -> "->ab a b"
   , [a/b]     -> "->ab a b"
                               
   // a op neutral
   , [a (+)]   -> "->a a"
   , [(+) a]   -> "->a a"
   , [a&(+)]   -> "->a a"
   , [(+)&a]   -> "->a a"
   , [a&&(+)]  -> "->a a"
   , [(+)&&a]  -> "->a a"
                             
   , [a+(-)]   -> "->a a"
   , [(-)+a]   -> "->a a"
   , [a|(-)]   -> "->a a"
   , [(-)|a]   -> "->a a"
   , [a||(-)]  -> "->a a"
   , [(-)||a]  -> "->a a"
   , [a/(-)]   -> "->a a"
   , [(-)/a]   -> "->a a"
                               
   // a op antineutral
   , [a (-)]   -> "->a a->0"
   , [(-) a]   -> "->0"
   , [a&(-)]   -> "->a a->0"
   , [(-)&a]   -> "->a a->0"
   , [a&&(-)]  -> "->0"
   , [(-)&&a]  -> "->0"
                             
   , [a+(+)]   -> "->1a a"
   , [(+)+a]   -> "->1a a"
   , [a|(+)]   -> "->1a a"
   , [(+)|a]   -> "->1a a"
   , [a||(+)]  -> "->1"
   , [(+)||a]  -> "->1"
   , [a/(+)]   -> "->1a a"
   , [(+)/a]   -> "->1"
                               
   // break a
   , [break a]  -> "->1"
   , [break&a]  -> "->1"
   , [break&&a] -> "->1"
                            
   , [break+a]  -> "->0"
   , [break|a]  -> "->0"
   , [break||a] -> "->0"
   , [break/a]  -> "->0"
                               
   // while(pass<1) a
   , [while(pass<1) a]  -> "->a a"
   , [while(pass<1)&a]  -> "->a a"
   , [while(pass<1)&&a] -> "->a a"

   , [while(pass<1)+a]  -> "->a a"
   , [while(pass<1)|a]  -> "->a a"
   , [while(pass<1)||a] -> "->a a"
   , [while(pass<1)/a]  -> "->a a"
                               
   // while(pass<2) a
   , [while(pass<2) a]  -> "->a  a->a aa"
   , [while(pass<2)&a]  -> "->aa a->a aa"
   , [while(pass<2)&&a] -> "->aa a->a aa"

   , [while(pass<2)+a]  -> "->aa a"
   , [while(pass<2)|a]  -> "->aa a->1a aa"
   , [while(pass<2)||a] -> "->aa a"
   , [while(pass<2)/a]  -> "->aa a"
                               
   // 2 operand sequences with iterator or break or optional break, 
   , [. a]     -> "->1a a"
   , [.. a]    -> "->1a a->1a aa->1a"
   , [... a]   -> "->a  a->a  aa->a"
                  
   , [a break] -> "->a  a"
   , [a;.]     -> "->a  a"
   , [a ..]    -> "->a  a->1a aa->1a"
   , [a ...]   -> "->a  a->a  aa->a"
   
   // 3 operand sequences with iterator or break or optional break, 
   , [a b break]   -> "->a a->b ab"
   , [a b;.]       -> "->a a->b ab"
   , [a b ..]      -> "->a  a->b ab->1a aba->b abab->1a"
   , [a b ...]     -> "->a  a->b ab->a  aba->b abab->a"
                      
   , [a break b]   -> "->a  a"
   , [a;. b]       -> "->a  a->1b ab"
   , [a .. b]      -> "->a  a->1b ab->a aba->1b"
   , [a ... b]     -> "->a  a->b  ab->a aba->b"
                      
   , [break a b]   -> "->1"
   , [. a b]       -> "->1a a->b ab"
   , [.. a b]      -> "->1a a->b ab->1a aba->b"
   , [... a b]     -> "->a  a->b  ab->a aba->b"
   
   // 2 level nested 2 operand sequences with iterator or break or optional break, 
   , [a; b break]  -> "->a  a->b ab"
   , [a;(b;.   )]  -> "->a  a->b ab"
   , [a; b ..   ]  -> "->a  a->b ab->1b abb->1b"
   , [a; b ...  ]  -> "->a  a->b  ab->b"
                      
   , [a b; break]  -> "->a a->b ab"
   , [a b; .    ]  -> "->a a->b ab"
   , [a b; ..   ]  -> "->a  a->b ab->1a aba->b abab->1a"
   , [a b; ...  ]  -> "->a  a->b ab->a  aba->b abab->a"
                      
   , [a; break b]  -> "->a  a"
   , [a; .     b]  -> "->a  a->1b ab"
   , [a; ..    b]  -> "->a  a->1b ab->1b"
   , [a; ...   b]  -> "->a  a->b  ab->b"
                      
   , [a break;b]   -> "->a  a->b ab"
   , [(a;. ) ;b]   -> "->a  a->b ab"
   , [a ..   ;b]   -> "->a  a->ab aa->ab ab aab"
   , [a ...  ;b]   -> "->a  a->a  aa->a"
                      
   , [break; a b]  -> "->1"
   , [.    ; a b]  -> "->1a a->b ab"
   , [..   ; a b]  -> "->1a a->b ab->1a aba->b"
   , [...  ; a b]  -> "->a  a->b  ab->a aba->b"
                      
   , [break a; b]  -> "->b b"
   , [.     a; b]  -> "->ab  a->b ab b"
   , [..    a; b]  -> "->ab  a->ab aa->ab b ab aab"
   , [...   a; b]  -> "->a  a->a  aa->a"

   // parallel composition
   , [(...;a)&b]   -> "->ab  a->ab  aa->ab  b->a  ba->a  ab->a  aba->a"
   , [b&(...;a)]   -> "->ab  a->ab  aa->ab  b->a  ba->a  ab->a  aba->a"  // commutative
   , [(...;a)|b]   -> "->ab  a->ab  aa->ab  b->1a  ba->1a  ab->1a  aba->1a"
   , [b|(...;a)]   -> "->ab  a->ab  aa->ab  b->1a  ba->1a  ab->1a  aba->1a"  // commutative
                      
   , [a&b&c]       -> "->abc a->bc  b->ac  c->ab  ab->c  ac->b  ba->c  bc->a  ca->b  cb->a  abc acb bac bca cab cba" 
   , [a&&b&&c]     -> "->abc a->bc  b->ac  c->ab  ab->c  ac->b  ba->c  bc->a  ca->b  cb->a  abc acb bac bca cab cba"  
   , [a|b|c]       -> "->abc a->1bc b->1ac c->1ab ab->1c ac->1b ba->1c bc->1a ca->1b cb->1a abc acb bac bca cab cba"  
   , [a||b||c]     -> "->abc a b c"  
                                       
   // disruption with compound left hand operand
   , [(a+b)/c]     -> "->abc a b c"
   , [(a|b)/c]     -> "->abc a->1bc b->1ac c ac bc ab ba"
   , [(a;b)/c]     -> "->ac a->bc ab c ac" 
   , [(a;b)/(+)]   -> "->1a a->1b ab"
   
   // optional break
   , [ a / .     ]             -> "->a a"
   , [ a / ..    ]             -> "->a a"
   , [ a b / . / c ]           -> "->a a->bc ab ac"
   , [ a b / ..  ]             -> "->a  a->ab ab aa->ab"
   , [ a / . / b ]             -> "->a a"
   , [ a b / . / c d ]         -> "->a a->bc  ab     ac->d  acd"
   , [ a b & . & c d ]         -> "->a a->bc  FAIL:ab->1c ac->bd abc->d  abcd acb->d  acd->b  acbd acdb"
   , [ a b | . | c d ]         -> "->a a->bc  ab->1c ac->bd abc->1d abcd acb->1d acd->1b acbd acdb"
   , [ . / a b ]               -> "FAIL:->1a a->b  ab"
   , [ . & a b ]               -> "->1a FAIL:a->b  ab"
   , [ . | a b ]               -> "FAIL:->1a a->b  ab"
   , [ . / a b / . / c d ]     -> "FAIL:->1a a->bc ab ac->d acd"
   , [ a b  | .  | (+) ]       -> "->a a->1b ab"
   , [ a b || . || (+) ]       -> "->a a"
   , [ a b  & .  & (-) ]       -> "->a a->b ab->0"
   , [ a b && . && (-) ]       -> "->a a->0"
   , [ (a b+(+))  & .  & (-) ] -> "->1a a->b ab->0"
   , [ (a b+(+)) && . && (-) ] -> "->1a a->0"

  
   // if
   , [ if  true then a ]            -> "->a a"
   , [ if  true then a b ]          -> "->a a->b ab"
   , [ if false then a ]            -> "->1"
   , [ if false then a b ]          -> "->1"
   , [ if  true then a   else c   ] -> "->a a"
   , [ if  true then a b else c d ] -> "->a a->b ab"
   , [ if false then a   else c   ] -> "->c c"
   , [ if false then a b else c d ] -> "->c c->d cd"

   // do
   , [ do a     then b ]              -> "->a a->b ab"
   , [ do a     then b c ]            -> "->a a->b ab->c abc"
   , [ do a              else d   ]   -> "->a a"
   , [ do a              else d e ]   -> "->a a"
   , [ do a     then b   else d   ]   -> "->a a->b ab"
   , [ do a     then b c else d e ]   -> "->a a->b ab->c abc"
   
   , [ do   (+) then b ]              -> "->b b"
   , [ do   (+) then b c ]            -> "->b b->c bc"
   , [ do   (+)          else d   ]   -> "->1"
   , [ do   (+)          else d e ]   -> "->1"
   , [ do   (+) then b   else d   ]   -> "->b b"
   , [ do   (+) then b c else d e ]   -> "->b b->c bc"
   
   , [ do   (-) then b ]              -> "->1"
   , [ do   (-) then b c ]            -> "->1"
   , [ do   (-)          else d   ]   -> "->d d"
   , [ do   (-)          else d e ]   -> "->d d->e de"
   , [ do   (-) then b   else d   ]   -> "->d d"
   , [ do   (-) then b c else d e ]   -> "->d d->e de"
   
   , [ do a (-) then b ]              -> "->a a"
   , [ do a (-) then b c ]            -> "->a a"
   , [ do a (-)          else d   ]   -> "->a a->d ad"
   , [ do a (-)          else d e ]   -> "->a a->d ad->e ade"
   , [ do a (-) then b   else d   ]   -> "->a a->d ad"
   , [ do a (-) then b c else d e ]   -> "->a a->d ad->e ade"
   
   // Threaded code fragments. TBD: check whether the test mechanism can handle this; maybe not
   , [ a {**} .. ; b ]         -> "->a a->ab aa->ab aaa->ab ab aab"

   // launching
   , [ (** a **) ]             -> "a"
   , [ (** a b **)   ]         -> "ab"
   , [ (** a **) b  ]          -> "ab"
   
   , [ (*  a  *)]              -> "a"
   , [ (* a b *)]              -> "a->b ab"
   , [ (* a *) b  ]            -> "->ab a->b b->a ab ba"
   , [ (* a b *) c d ]         -> "->ac a->bc  c->ad  ab->c  ac->bd  ca->bd  cd->a  abc->d  acb->d acd->b cab->d cad->b cda->b abcd  acbd acdb cabd cadb cdab"
   
   , [ (** (* a b *) **) c]    -> "->a a->b abc"
   , [ (** (* a b *) c **) d]  -> "->ac a->bc  c->a ab->c ac->b ca->b abc->d acb->d cab->d abcd acbd cabd"

   , [ {here.launch([  a  ])}     ] -> "a"
   , [ {here.launch([ a b ])}     ] -> "ab"
   , [ {here.launch([  a  ])} b   ] -> "->ab a->b b->a ab ba"
   , [ {here.launch([ a b ])} c d ] -> "->ac a->bc  c->ad  ab->c  ac->bd  ca->bd  cd->a  abc->d  acb->d acd->b cab->d cad->b cda->b abcd  acbd acdb cabd cadb cdab"
   , [ (**{here.launch([ a b ])}**) c]    -> "->a a->b abc"
   , [ (**{here.launch([ a b ])} c**) d]  -> "->ac a->bc  c->a ab->c ac->b ca->b abc->d acb->d cab->d abcd acbd cabd"
   
   // failure
   , [ {?here.fail?} ]        -> "->0"
   
   // priorities
   , [      @{there.priority=  1}:{} + {}(-)] -> "->1"
   , [ {} + @{there.priority=  1}:     {}(-)] -> "->0"
   , [      @{there.priority= -1}:{} + {}(-)] -> "->0"
   , [ {} + @{there.priority= -1}:     {}(-)] -> "->1" 
   
   // Various
   , [(a {**} b) ... || c...]  -> "->ac FAIL:a->bc FAIL:ab->ac c->ac cc->ac FAIL:ca->bc FAIL:ac->bc FAIL:acc->bc FAIL:acb->ac"
   
   // problem in LookupFrame2: after 2 iterations an optional success of the guard  (a . b . c)
   // caused a "bypass" of the searchcommand (d) so that the search (e) was activated
   //, [(a;.;b;.;c) d; e ] -> "->a a->bd ab->cd abc->d ad->e abd->e abcd->e ade abde abcde"
   //, [(a..) b; c ] -> "->a a->ab aa->ab aaa->ab ab->c aab->c aaab->c abc aabc aaabc"
 
 )

   
  val scriptBehaviourList_for_debug = List(
      [ a
        b; c
/*        
        {! !}
        {* *}; {! !}
        @{ }: {. .}
        {! !}
        if 1 != 0 then (+)
*/
      ] -> ""
      
      
   //  [ {Hello} ~~(msg:String)~~> {println(msg)} ]  -> "a->1"
 //, [{println("starting...");"hello"} ~~~(msg:String)~~~> {println(msg)} ]  -> "->1"
 //, [a ~~(t:String)~~>p(t)]       -> "a->a"
 //, [b ~~(t:String)~~>p(t)]       -> "b->b"
  )

  // unfortunately we cannot test event handling code fragments, for the time being, as in , [ {. .} / .. ]    -> "???"

  
}
