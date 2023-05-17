package rudiments

import anticipation.*

import scala.util.*
import scala.quoted.*

import language.experimental.captureChecking

opaque type Text <: Matchable = String

object Text:
  def apply(string: String): Text = string
  extension (string: Text) def s: String = string

  given fromString: CommandLineParser.FromString[Text] = identity(_)
  given ordering: Ordering[Text] = Ordering.String.on[Text](_.s)
  given genericHttpRequestParam: GenericHttpRequestParam[String, Text] = _.s

  given fromExpr(using fromExpr: FromExpr[String]): FromExpr[Text] with
    def unapply(expr: Expr[Text])(using Quotes): Option[Text] = fromExpr.unapply(expr).map(Text(_))
  
  given toExpr(using toExpr: ToExpr[String]): ToExpr[Text] with
    def apply(text: Text)(using Quotes): Expr[Text] = toExpr(text.s)

  given stringText: Conversion[String, Text] = Text(_)

  erased given CanEqual[Text, Text] = compiletime.erasedValue

  given typeTest: Typeable[Text] with
    def unapply(value: Any): Option[value.type & Text] = value.matchable(using Unsafe) match
      case str: String => Some(str.asInstanceOf[value.type & Text])
      case _           => None

extension (xs: Iterable[Text])
  transparent inline def ss: Iterable[String] = xs
