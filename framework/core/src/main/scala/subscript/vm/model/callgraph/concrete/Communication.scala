// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._
import subscript.vm.model.template.TemplateNode
import scala.collection.mutable.ListBuffer

object Multiplicity extends Enumeration {
  type MultiplicityType = Value
  val Zero_or_One, Zero_or_More, One, One_or_More = Value
}
/*
case class Communication(_body: N_communication => TemplateNode) {
  var communicatorRoles: List[CommunicatorRole] = null
  var template: T_communication = null
  def setCommunicatorRoles(crs: List[CommunicatorRole]): Unit = {
    var names = new ListBuffer[Symbol]
    communicatorRoles = crs
    for (cr <- crs) {
      cr.communication = this
      cr.communicator.roles += cr
      names += cr.communicator.name
    }
    template = T_communication(null, "comm", names)
  }
}
case class Communicator(name: Symbol) {
  def removePendingCall(call: N_call) {instances -= call}
  val instances = scala.collection.mutable.ArrayBuffer.empty[N_call]
  var roles = new ListBuffer[CommunicatorRole]
}
case class CommunicatorRole(communicator: Communicator) {
  var communication: Communication = null
  var multiplicity = Multiplicity.One
  var parameterNames = new ListBuffer[Symbol] 
  def ~(s: Symbol): CommunicatorRole = {parameterNames += s; this}
  def ~(m: Multiplicity.MultiplicityType): CommunicatorRole = {multiplicity = m; this}
}
*/