package anticipation

import scala.quoted.*
import scala.compiletime.*
import scala.util.*
import scala.reflect.*

object Anticipation:
  opaque type Text <: Matchable = String

  object Text:
    def apply(string: String): Text = string
    extension (text: Text) inline def s: String = text

    given fromString: CommandLineParser.FromString[Text] = identity(_)
    given genericHttpRequestParam: GenericHttpRequestParam[String, Text] = identity(_)
    
    given fromExpr(using fromExpr: FromExpr[String]): FromExpr[Text] with
      def unapply(expr: Expr[Text])(using Quotes): Option[Text] = fromExpr.unapply(expr)

    given toExpr(using toExpr: ToExpr[String]): ToExpr[Text] with
      def apply(text: Text)(using Quotes): Expr[Text] = toExpr(text)

    given stringText: Conversion[String, Text] = identity(_)

    erased given canEqual: CanEqual[Text, Text] = erasedValue

    given typeable: Typeable[Text] with
      def unapply(value: Any): Option[value.type & Text] = value.asMatchable match
        case str: String => Some(str)
        case _           => None

export Anticipation.Text
