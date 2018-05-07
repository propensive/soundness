package nidificant

import scala.reflect._, macros._, blackbox.Context

object Macros {

  def rewriteAttributes(c: Context)(methodName: c.Tree)(attributes: c.Tree*): c.Tree = {
    import c.universe._

    if (attributes.isEmpty) throw new Exception()

    methodName match {
      case Literal(Constant("apply")) =>
        val rewrittenAttributes = attributes.map {
          case q"(${Literal(Constant(identifier: String))}, $value)" =>
            val term = TermName(s"${identifier}Attribute")
            q"_root_.nidificant.html5.$term() = $value"
        }

        q"${c.prefix}.attributes(..$rewrittenAttributes)"
    }
  }
}
