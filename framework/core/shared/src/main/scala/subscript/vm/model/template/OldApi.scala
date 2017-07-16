package subscript.vm.model.template

trait TemplateNodeHelpers {this: TemplateNode =>
  // TBD: remove asInstanceOf
  def hierarchyString: String = TemplateNode.hierarchyString(this, parent.asInstanceOf[TemplateNode.Parent], false)
}
