/*
    Anticipation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import prepositional.*
import symbolism.*

import scala.compiletime.*
import scala.quoted.*
import scala.reflect.*
import scala.util.*

import language.experimental.captureChecking

object Anticipation:
  opaque type Text <: Matchable = String

  object Text:
    def apply(string: String): Text = string
    def apply(chars: IArray[Char]): Text = String(chars.asInstanceOf[Array[Char]])

    extension (text: Text) inline def s: String = text

    given Text is Addable by Text into Text as addable = _ + _

    private def recur(text: Text, n: Int, acc: Text): Text =
      if n == 0 then acc else recur(text, n - 1, acc+text)

    given Text is Multiplicable by Int into Text as multiplicable =
      (text, n) => recur(text, n.max(0), "".tt)

    given Ordering[Text] as ordering = Ordering.String.on[Text](identity)
    given CommandLineParser.FromString[Text] as fromString = identity(_)

    given (using fromExpr: FromExpr[String]) => FromExpr[Text] as fromExpr:
      def unapply(expr: Expr[Text])(using Quotes): Option[Text] = fromExpr.unapply(expr)

    given ToExpr[Text] as toExpr:
      def apply(text: Text)(using Quotes) =
        import quotes.reflect.*
        val expr = Literal(StringConstant(text)).asExprOf[String]
        '{Text($expr)}

    given Conversion[String, Text] as stringText = identity(_)

    erased given CanEqual[Text, Text] as canEqual = erasedValue

    given Typeable[Text] as typeable:
      def unapply(value: Any): Option[value.type & Text] = value.asMatchable match
        case str: String => Some(str)
        case _           => None
