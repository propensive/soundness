package anticipation

import symbolism.*

import scala.quoted.*
import scala.compiletime.*
import scala.util.*
import scala.reflect.*

object Anticipation:
  opaque type Text <: Matchable = String

  object Text:
    def apply(string: String): Text = string
    extension (text: Text) inline def s: String = text

    given add: ClosedOperator["+", Text] = _+_
    
    given times: Operator["*", Text, Int] with
      type Result = Text
      
      private def recur(text: Text, n: Int, acc: Text): Text =
        if n == 0 then acc else recur(text, n - 1, acc+text)
      
      inline def apply(inline left: Text, inline right: Int): Text =
        recur(left, right.max(0), "")

    given ordering: Ordering[Text] = Ordering.String.on[Text](identity)
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
