package generator

import parser._

object TemplateHelper {

  implicit class TemplateAction(action: ActionDefinition) {
    def template: String = s"new ${action.pattern.template}${if(action.conditions.size > 0) " with " else ""}${action.conditions.map(_.template).mkString(" with ")} with To${action.name}"
  }

  implicit class TemplateIdentifier(identifier: Identifier) {
    def template: String = identifier match {
      case IdentifierSingle(name) => name
      case IdentifierWithOptions(name, options) => s"$name(${options.map(_.template).mkString(", ")})"
    }
  }

  implicit class TemplateKeyValue(keyValue: KeyValue) {
    def template: String = s"${keyValue.name} = ${keyValue.value}"
  }

}
