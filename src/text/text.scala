/*
    Anticipation, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import symbolism.*

import scala.quoted.*
import scala.compiletime.*
import scala.util.*
import scala.reflect.*

import language.experimental.captureChecking

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
      
      inline def apply(left: Text, right: Int): Text =
        recur(left, right.max(0), "")

    given ordering: Ordering[Text] = Ordering.String.on[Text](identity)
    given fromString: CommandLineParser.FromString[Text] = identity(_)
    
    given fromExpr(using fromExpr: FromExpr[String]): FromExpr[Text] with
      def unapply(expr: Expr[Text])(using Quotes): Option[Text] = fromExpr.unapply(expr)

    given toExpr: ToExpr[Text] with
      def apply(text: Text)(using Quotes) =
        import quotes.reflect.*
        val expr = Literal(StringConstant(text)).asExprOf[String]
        '{Text($expr)}

    given stringText: Conversion[String, Text] = identity(_)

    erased given canEqual: CanEqual[Text, Text] = erasedValue

    given typeable: Typeable[Text] with
      def unapply(value: Any): Option[value.type & Text] = value.asMatchable match
        case str: String => Some(str)
        case _           => None

export Anticipation.Text

extension (texts: Iterable[Text])
  transparent inline def ss: Iterable[String] = texts.map(_.s)

extension (string: String) def tt: Text = Text(string)
