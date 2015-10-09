package subscript.example
import subscript.file

import scala.language.implicitConversions

import scala.swing._
import scala.swing.event._

import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._

// Subscript sample application: a text entry field with a search button, that simulates the invocation of a background search
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala

object LookupFrame extends LookupFrameApplication

class LookupFrameApplication extends SimpleSubscriptApplication {
  
  val outputTA     = new TextArea        {editable      = false}
  val searchButton = new Button("Go")    {enabled       = false}
  val searchLabel  = new Label("Search") {preferredSize = new Dimension(45,26)}
  val searchTF     = new TextField       {preferredSize = new Dimension(100, 26)}
  
  val top          = new MainFrame {
    title          = "LookupFrame - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(300,300)
    contents       = new BorderPanel {
      add(new FlowPanel(searchLabel, searchTF, searchButton), BorderPanel.Position.North) 
      add(outputTA, BorderPanel.Position.Center) 
    }
  }
  override def live = subscript.DSL._execute(liveScript)
  
  override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchSequence))}
  
  def sleep(time: Long) = Thread.sleep(time)
def searchSequence = subscript.DSL._script[Any](None, Symbol("searchSequence")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchCommand), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => showSearchingText), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchInDatabase), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => showSearchResults))}
def searchCommand = subscript.DSL._script[Any](None, Symbol("searchCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchButton)}
def showSearchingText = subscript.DSL._script[Any](None, Symbol("showSearchingText")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"outputTA.text\", subscript.DSL._maybeVarCall(\"\\\"Searching: \\\"+searchTF.text\"))")
}, true))}
def showSearchResults = subscript.DSL._script[Any](None, Symbol("showSearchResults")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"outputTA.text\", subscript.DSL._maybeVarCall(\"\\\"Found: \\\"+here.index+\\\" items\\\"\"))")
}, true))}
def searchInDatabase = subscript.DSL._script[Any](None, Symbol("searchInDatabase")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("sleep(subscript.DSL._maybeVarCall(\"2000\"))")
}, true)} // simulate a time consuming action

implicit def vkey(k: subscript.vm.FormalConstrainedParameter[Key.Value]) = subscript.DSL._script[Any](None, Symbol("vkey"), k.~??(Symbol("k"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => vkey2(subscript.DSL._maybeVarCall("top"), subscript.DSL._maybeVarCall("subscript.vm.ActualAdaptingParameter(k)")))}

/* translated into:

  override def _live     = _script(this, 'live             ) {_seq(_loop, _searchSequence)}
  def _searchSequence    = _script(this, 'searchSequence   ) {_seq(_searchCommand, _showSearchingText, _searchInDatabase, _showSearchResults)}
  def _searchCommand     = _script(this, 'searchCommand    ) {_clicked(searchButton)}
  def _showSearchingText = _script(this, 'showSearchingText) {_at{gui0} (_normal0 {                         outputTA.text = "Searching: "+searchTF.text})}
  def _showSearchResults = _script(this, 'showSearchResults) {_at{gui0} (_normal{(here: N_code_normal) => outputTA.text = "Found: "+here.index+" items"})}
  def _searchInDatabase  = _script(this, 'searchInDatabase ) {_threaded0{Thread.sleep(2000)}}
               
  // bridge method   
  override def live = _execute(_live             )
  * 
  */
}
