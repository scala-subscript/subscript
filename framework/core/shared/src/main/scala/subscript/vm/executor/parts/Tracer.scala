package subscript.vm.executor.parts

import subscript.vm._

/**
 * Simple tracing and error tracking
 */
trait Tracer {
  def traceLevel: Int
  def trace(s: => String) = if (traceLevel>0) {println(s); Console.flush}
  def trace_nonl(s: => String) = if (traceLevel>0) {print(s); Console.flush}
  def error(s: String) {throw new Exception(s)}
  
  def traceAttribute(name: => String, value: Any) = traceAttribute_internal(name, value)
  private def traceAttribute_internal(name: String, value: Any) = if (traceLevel>=2) println(f"$name%41s: $value")
  def traceAttributes(n: N_n_ary_op, str: => String) = {
    if (traceLevel>=2)
    {
    println(s"$str:")
    traceAttribute_internal("activationMode", n.activationMode)
    traceAttribute_internal("hadFullBreak"  , n.hadFullBreak)
    traceAttribute_internal("nActivatedMandatoryChildren"              , n.nActivatedMandatoryChildren)
    traceAttribute_internal("nActivatedMandatoryChildrenWithSuccess"   , n.nActivatedMandatoryChildrenWithSuccess)
  //traceAttribute_internal("nActivatedMandatoryChildrenWithoutSuccess", n.nActivatedMandatoryChildrenWithoutSuccess)
    traceAttribute_internal("nActivatedOptionalChildren"               , n.nActivatedOptionalChildren)
    traceAttribute_internal("nActivatedOptionalChildrenWithSuccess"    , n.nActivatedOptionalChildrenWithSuccess)
  //traceAttribute_internal("nActivatedOptionalChildrenWithoutSuccess" , n.nActivatedOptionalChildrenWithoutSuccess)
    traceAttribute_internal("indexChild_optionalBreak_last"            , n.indexChild_optionalBreak_last)
    traceAttribute_internal("indexChild_optionalBreak_secondLast"      , n.indexChild_optionalBreak_secondLast)
    traceAttribute_internal("indexChild_lastActivated"                 , if(n.lastActivatedChild==null)"" else n.lastActivatedChild.index)
    traceAttribute_internal("aaActivated_notBeforeLastOptionalBreak"   , n.aaActivated_notBeforeLastOptionalBreak)
    traceAttribute_internal("aaHappenedInOptionalChildren"             , n.aaHappenedInOptionalChildren)
    }
  }  
}
