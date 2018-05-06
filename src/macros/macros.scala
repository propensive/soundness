package progeny

import scala.reflect._, macros._, blackbox.Context

object Macros {

  def rewriteAttributes(c: Context)(methodName: c.Tree)(attributes: c.Tree*): c.Tree = {
    import c.universe._

    methodName match {
      case Literal(Constant("apply")) =>
        val rewrittenAttributes = attributes.map {
          case q"(${Literal(Constant(identifier: String))}, $value)" =>
            q"${TermName(s"${identifier}Attribute")} = $value"
        }
        
        q"${c.prefix}.attributes(..$rewrittenAttributes)"
    }
  }
}
