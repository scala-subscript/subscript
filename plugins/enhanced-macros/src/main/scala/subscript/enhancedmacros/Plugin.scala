package subscript.enhancedmacros

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}
import scala.reflect.internal.Mode

class Plugin(val global: Global) extends NscPlugin {

  import global._
  import analyzer._
  import analyzer.{MacroPlugin => NscMacroPlugin}

  val name        = "enhancedmacros"
  val description = "Enables whitebox macros to receive the type from their enclosing environment"
  val components  = Nil


  class DefMacroExpanderEnhanced(typer: Typer, expandee: Tree, mode: Mode, outerPt: Type) extends DefMacroExpander(typer, expandee, mode, outerPt) {
    override def onSuccess(expanded0: Tree) = {
      // prematurely annotate the tree with a macro expansion attachment
      // so that adapt called indirectly by typer.typed knows that it needs to apply the existential fixup
      linkExpandeeAndExpanded(expandee, expanded0)

      def typecheck(label: String, tree: Tree, pt: Type): Tree = {
        if (tree.isErrorTyped) tree
        else {
          if (macroDebugVerbose) println(s"$label (against pt = $pt): $tree")
          // `macroExpandApply` is called from `adapt`, where implicit conversions are disabled
          // therefore we need to re-enable the conversions back temporarily
          val result = typer.context.withImplicitsEnabled(typer.typed(tree, mode, pt))
          if (result.isErrorTyped && macroDebugVerbose) println(s"$label has failed: ${typer.context.reporter.errors}")
          result
        }
      }

      if (isBlackbox(expandee)) {
        val expanded1 = atPos(enclosingMacroPosition.makeTransparent)(Typed(expanded0, TypeTree(innerPt)))
        typecheck("blackbox typecheck", expanded1, outerPt)
      } else {
        // We are now vulnurable to SI-6992, SI-8048 and SI-8209
        // Though it is not likely that they will happen in our application context
        val expanded1 = typecheck("whitebox typecheck #1", expanded0, outerPt)
        typecheck("whitebox typecheck #2", expanded1, innerPt)
      }
    }
  }

  object MacroPlugin extends NscMacroPlugin {
   
    override def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = {
      expandee match {
        case q"subscript.DSL._maybeVarCall($_)" => 
          val expander = new DefMacroExpanderEnhanced(typer, expandee, mode, pt)
          Some(expander(expandee))

        case _ => None
      }
    }
    
  }

  analyzer.addMacroPlugin(MacroPlugin)
}
