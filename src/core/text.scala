/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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

  given typeTest: Typeable[Text] with
    def unapply(value: Any): Option[value.type & Text] = value.matchable(using Unsafe) match
      case str: String => Some(str.asInstanceOf[value.type & Text])
      case _           => None

extension (xs: Iterable[Text])
  transparent inline def ss: Iterable[String] = xs
