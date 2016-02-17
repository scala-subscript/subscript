package scalaParser.subscript.ast


object Constants {
  def pkg(name: String, pkgName: String) = s"$pkgName.$name"

  def dsl             (name: String) = pkg(name, "subscript.DSL"                       )
  def scriptDsl       (name: String) = pkg(name, "subscript.ScriptDSL"                 )
  def vm              (name: String) = pkg(name, "subscript.vm"                        )
  def templateConcrete(name: String) = pkg(name, "subscript.vm.model.template.concrete")

  def mkCall(name: String, content: String): String =
  s"""subscript.DSL._call[Any]("$name", ((here: subscript.vm.N_call[Any]) => {
     |  val s: subscript.vm.Script[Any] = $content
     |  here.calls(s.template, (s.p: _*));
     |  s
     |}), true)
    """.stripMargin

  object DSL {
    object Op {
      val SEQUENCE            = dsl("_seq")

      val IF_EXPR             = dsl("_if")
      val IF_ELSE_EXPR        = dsl("_if_else")

      val DO_THEN             = dsl("_do_then")
      val DO_THEN_ELSE        = dsl("_do_then_else")
      val DO_ELSE             = dsl("_do_else")

      val OR_PAR_1            = dsl("_par_or")
      val OR_PAR_2            = dsl("_par_or2")

      val AND_PAR_1           = dsl("_par")
      val AND_PAR_2           = dsl("_par_and2")

      val EQUALITY            = dsl("_par_equal")

      val ALTERNATIVE         = dsl("_alt")

      val DISRUPT             = dsl("_disrupt")
      val SHUFFLE             = dsl("_shuffle")
      val SHUFFLE_1_OR_MORE   = dsl("_shuffle_1_or_more")
      val INTERRUPT           = dsl("_interrupt")
      val INTERRUPT_0_OR_MORE = dsl("_interrupt_0_or_more")

      val LAUNCH              = dsl("_launch"       )
      val LAUNCH_ANCHOR       = dsl("_launch_anchor")

      val CARET               = dsl("_caret")
      val DOUBLE_CARET        = dsl("_double_caret")
      val DOUBLE_CARET_NUMBER = dsl("_double_caret_number")

      val DATAFLOW           = scriptDsl("_dataflow"    )
      val DATAFLOW_MAP       = scriptDsl("_dataflow_map")
    }

    object Term {
      val NORMAL0             = dsl("_normal0")
      val THREADED0           = dsl("_threaded0")
      val USURE0              = dsl("_unsure0")
      val TINY0               = dsl("_tiny0")
      val EVENTHANDLING0      = dsl("_eventhandling0")
      val EVENTHANDLING_LOOP0 = dsl("_eventhandling_loop0")

      val NORMAL             = dsl("_normal")
      val THREADED           = dsl("_threaded")
      val USURE              = dsl("_unsure")
      val TINY               = dsl("_tiny")
      val EVENTHANDLING      = dsl("_eventhandling")
      val EVENTHANDLING_LOOP = dsl("_eventhandling_loop")

      val VAR                = dsl("_var")
      val VAL                = dsl("_val")
      val DECLARE            = dsl("_declare")
      val UNTYPED_DECLARE    = dsl("_declareNoType")
      val UNTYPED_VALUE_CODE = dsl("_valueCodeNoType")

      val VAR_CALL       = dsl("_maybeVarCall")
      val VAR_ASSIGNMENT = dsl("_maybeVarAssignment")

      val SCRIPT  = dsl("_script")

      val DELTA   = dsl("_deadlock")
      val EPSILON = dsl("_empty"   )
      val NEUTRAL = dsl("_neutral" )

      val LOOP                = dsl("_loop"              )
      val OPTIONAL_BREAK_LOOP = dsl("_optionalBreak_loop")
      val OPTIONAL_BREAK      = dsl("_optionalBreak"     )
      val BREAK               = dsl("_break"             )

      val WHILE   = dsl("_while")

      val ANNOTATION = dsl("_at")
    }

    object Type {
      val               OUTPUT_PARAMETER = vm("FormalOutputParameter"       )
      val CONSTRAINABLE_OUTPUT_PARAMETER = vm("ConstrainableFormalParameter")
      val        ACTUAL_OUTPUT_PARAMETER = vm("ActualOutputParameter"       )
      val      ACTUAL_ADAPTING_PARAMETER = vm("ActualAdaptingParameter"     )
      val   FORMAL_CONSTRAINED_PARAMETER = vm("FormalConstrainedParameter"  )
      
      val SCRIPT                         = vm("ScriptTrait"                 )
      val SCRIPT_NODE                    = vm("Script"                      )

      val LOCAL_VAR            = vm("N_localvar")
      val SCRIPT_CALL          = vm("N_call"    )
      val CALL_GRAPH_TREE_NODE = vm("model.callgraph.CallGraphTreeNode") 
      val CALL_GRAPH_NODE      = vm("model.callgraph.CallGraphNode")

      val TEMPLATE_CHILD       = vm("model.template.TemplateNode.Child")


      def pref(prefix: String, str: String) = s"${prefix}_$str"
      def nPref(str: String) = pref("N_code", str)
      def tPref(str: String) = pref("T_code", str)


      val NORMAL             = "normal"
      val THREADED           = "threaded"
      val UNSURE             = "unsure"
      val TINY               = "tiny"
      val EVENTHANDLING      = "eventhandling"
      val EVENTHANDLING_LOOP = "eventhandling_loop"

      val N_NORMAL =               vm(nPref(NORMAL))
      val T_NORMAL = templateConcrete(tPref(NORMAL))
      
      val N_THREADED =               vm(nPref(THREADED))
      val T_THREADED = templateConcrete(tPref(THREADED))
      
      val N_UNSURE =               vm(nPref(UNSURE))
      val T_UNSURE = templateConcrete(tPref(UNSURE))
      
      val N_TINY =               vm(nPref(TINY))
      val T_TINY = templateConcrete(tPref(TINY))
      
      val N_EVENTHANDLING =               vm(nPref(EVENTHANDLING))
      val T_EVENTHANDLING = templateConcrete(tPref(EVENTHANDLING))
      
      val N_EVENTHANDLING_LOOP =               vm(nPref(EVENTHANDLING_LOOP))
      val T_EVENTHANDLING_LOOP = templateConcrete(tPref(EVENTHANDLING_LOOP))

      val T_CALL = templateConcrete("T_call")
    }

  }

  object Name {
    val NODE   = "_node"
    val HERE   = "here"
    val THERE  = "there"
    val SCRIPT = "script"

    val FILE   = "subscript.language"

    val LAMBDA = "<lambda>"

    val ACTOR_CALL = "r$"
  }

  object Key {
    val MODIFIERS     = "mods"
    val SCRIPT        = "script"
    val HEADER_NAME   = "header_name"
    val FORMAL_PARAMS = "formal_params"

    // Aspects
    def before(name: String) = s"${name}_before"
    def after (name: String) = s"${name}_after"
  }

}
