package subscript
import subscript.file

// import org.junit.runner.RunWith

import subscript.Predef._
import subscript.DSL._
import subscript.vm.{N_code_unsure, SimpleScriptDebugger, Script}
import subscript.vm.model.callgraph._
import subscript.vm.executor._

class TestC {
    
  script..
    times(n:Int) = while(here.pass < n)
    key(??c: Char) = {!c='!'!}
    mainScript(args: Array[String]) = val c: Char = ' '
                                      key(?c)  {println(s"key =>  $c")}
                                      key('!') {println("key <= '!'")}

  val times1: Int=>Script[Any] = n=>{_script(this, 'lambda) {script => _while{implicit here=>pass<n}}}
  val times2: Int=>Script[Any] = n=> [ while(here.pass < n) ]
  //val times3: Int=>Script =     [ while(pass < _) ]
  //val noXml = [noXML]

  //TBD: allow ? in all parameter lists; parse < script > blocks
}
object Test extends TestC {
  // bridge method:
  def main( args: Array[String]): Unit = _execute(mainScript(args))
}