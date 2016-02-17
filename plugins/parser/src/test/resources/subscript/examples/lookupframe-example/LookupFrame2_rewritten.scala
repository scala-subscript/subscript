package subscript.example
import subscript.file

import scala.swing._
import scala.swing.event._

import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._

// Subscript sample application: a text entry field with a search button, that simulates the invocation of a background search
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala
object LookupFrame2 extends LookupFrame2Application

class LookupFrame2Application extends SimpleSubscriptApplication {
  import scala.language.implicitConversions

  val outputTA     = new TextArea         {editable      = false}
  val searchButton = new Button("Go"    ) {enabled       = false; focusable = false}
  val cancelButton = new Button("Cancel") {enabled       = false; focusable = false}
  val   exitButton = new Button("Exit"  ) {enabled       = false; focusable = false}
  val searchLabel  = new Label("Search")  {preferredSize = new Dimension(45,26)}
  val searchTF     = new TextField        {preferredSize = new Dimension(100, 26)}
  
  val top          = new MainFrame {
    title          = "LookupFrame - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(500,300)
    contents       = new BorderPanel {
      add(new FlowPanel(searchLabel, searchTF, searchButton, cancelButton, exitButton), BorderPanel.Position.North) 
      add(outputTA, BorderPanel.Position.Center) 
    }
  }
  
  top.listenTo (searchTF.keys)
  val f = top.peer.getRootPane().getParent().asInstanceOf[javax.swing.JFrame]
  f.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE) // TBD: does not seem to work on MacOS
  
  def sleep(duration_ms: Long) = try {Thread.sleep(duration_ms)} catch {case e: InterruptedException => /*println("sleep interrupted")*/}
  def confirmExit: Boolean = Dialog.showConfirmation(null, "Are you sure?", "About to exit")==Dialog.Result.Yes
  
  override def  live = subscript.DSL._execute(liveScript)

  implicit def vkey(k: subscript.vm.FormalConstrainedParameter[Key.Value]) = subscript.DSL._script[Any](None, Symbol("vkey"), k.~??(Symbol("k"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => vkey2(subscript.DSL._maybeVarCall("top"), subscript.DSL._maybeVarCall("subscript.vm.ActualAdaptingParameter(k)")))}

def searchCommand = subscript.DSL._script[Any](None, Symbol("searchCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => Key.Enter))}
def cancelCommand = subscript.DSL._script[Any](None, Symbol("cancelCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => cancelButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => Key.Escape))}
def exitCommand = subscript.DSL._script[Any](None, Symbol("exitCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => windowClosing(subscript.DSL._maybeVarCall("top"))))}
def doExit = subscript.DSL._script[Any](None, Symbol("doExit")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitCommand), subscript.DSL._dataflow_then(
  subscript.DSL._script[Any](None, Symbol("~~>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("confirmExit")
}, true))}
, (_r: Any) => _r match {case r: Boolean => subscript.DSL._script[Any](None, Symbol("<lambda>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("!r")
})}}
))}
def cancelSearch = subscript.DSL._script[Any](None, Symbol("cancelSearch")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => cancelCommand), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => showCanceledText))}
def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchSequence)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => doExit))}
def searchSequence = subscript.DSL._script[Any](None, Symbol("searchSequence")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => guard(subscript.DSL._maybeVarCall("searchTF"), ()=> subscript.DSL._maybeVarCall("!searchTF.text.isEmpty"))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchCommand), subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => showSearchingText), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchInDatabase), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => showSearchResults)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => cancelSearch)))}
def showSearchingText = subscript.DSL._script[Any](None, Symbol("showSearchingText")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"outputTA.text\", subscript.DSL._maybeVarCall(\"\\\"Searching: \\\"+searchTF.text\"))")
}, true))}
def showCanceledText = subscript.DSL._script[Any](None, Symbol("showCanceledText")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"outputTA.text\", subscript.DSL._maybeVarCall(\"\\\"Searching Canceled\\\"\"))")
}, true))}
def showSearchResults = subscript.DSL._script[Any](None, Symbol("showSearchResults")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"outputTA.text\", subscript.DSL._maybeVarCall(\"\\\"Results: 1, 2, 3\\\"\"))")
}, true))}
def searchInDatabase = subscript.DSL._script[Any](None, Symbol("searchInDatabase")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("sleep(subscript.DSL._maybeVarCall(\"5000\"))")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => progressMonitor))}
def progressMonitor = subscript.DSL._script[Any](None, Symbol("progressMonitor")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("outputTA.text+=here.pass")
}, true)), subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("sleep(subscript.DSL._maybeVarCall(\"200\"))")
}, true))}
    
/*
 override def _live     = _script(this, 'live             ) {_par_or2(_seq(_loop, _searchSequence), _exit)}
  def _searchCommand     = _script(this, 'searchCommand    ) {_alt(_clicked(searchButton), _vkey(Key.Enter))} 
  def _cancelCommand     = _script(this, 'cancelCommand    ) {_alt(_clicked(cancelButton), _vkey(Key.Escape))}
  def   _exitCommand     = _script(this, 'exitCommand      ) {_alt(_clicked(  exitButton), _windowClosing(top))}
  def _cancelSearch      = _script(this, 'cancelSearch     ) {_seq(_cancelCommand, _at{gui0} (_call{_showCanceledText}))}
  def _searchSequence    = _script(this, 'searchSequence   ) {_seq(_guard(searchTF, ()=> !(searchTF.text.isEmpty)),  
                                                                   _searchCommand, 
                                                                 _disrupt(_seq(_showSearchingText, _searchInDatabase, _showSearchResults),
                                                                                 _cancelSearch ))}

  def   _exit            = {val _r = _declare[Boolean]('r)
                           _script(this, 'exit) {_seq(_var(_r, (here:N_localvar[_]) => false), 
                                                      _exitCommand,
                                                      _at{gui0} (_normal{here => _r.at(here).value = confirmExit}),
                                                      _while{here=> {! _r.at(here).value}})}
  }
  
  def _showSearchingText = _script(this, 'showSearchingText) {_at{gui0} (_normal0 {            
    outputTA.text = 
      "Searching: "+searchTF.text
      })}
  def _showSearchResults = _script(this, 'showSearchResults) {_at{gui0} (_normal{(here: N_code_normal) => 
    outputTA.text = "Found: "+here.index+" items"})}
  def _showCanceledText  = _script(this, 'showCanceledText ) {_at{gui0} (_normal0 {outputTA.text = "Searching Canceled"})}
  def _searchInDatabase  = _script(this, 'searchInDatabase ) {_par_or2(_threaded0{sleep(5000)}, _progressMonitor)} 
  def _progressMonitor   = _script(this, 'progressMonitor  ) {
  _seq(_loop, 
      _at{gui0} (_normal{(here: N_code_normal) => outputTA.text+=" "+pass(here)}), 
      _threaded0{sleep(200)})}
 
  def _vkey(_k:FormalConstrainedParameter[Key.Value]) = _script(this, 'vkey, _k~??'k) {subscript.swing.Scripts._vkey(top, _k~??)}
               
// bridge method   
override def live = _execute(_live)
*/  
}
