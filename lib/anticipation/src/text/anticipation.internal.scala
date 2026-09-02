                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package anticipation

import scala.caps

import scala.compiletime.*
import scala.quoted.*
import scala.reflect.*
import scala.util.*

import java.nio.charset.StandardCharsets
import prepositional.*
import symbolism.*

object internal:
  into opaque type Text <: Matchable & caps.Pure = String & caps.Pure

  object Text:
    // `caps.Pure` is an erased marker, so `Text` still erases to `String`; this cast is a
    // runtime no-op that lets the capture checker treat `Text` as a pure type.
    private inline def make(string: String): Text = string.asInstanceOf[Text]

    def apply(string: String): Text = make(string)
    def apply(chars: Array[Char]^{}): Text = make(String(chars.asInstanceOf[scala.Array[Char]]))
    def apply(bytes: Array[Byte]^{}): Text =
      make(String(bytes.asInstanceOf[scala.Array[Byte]], StandardCharsets.US_ASCII))

    extension (text: Text) inline def s: String = text.asInstanceOf[String]

    given zeroic: Text is Zeroic:
      inline def zero: Text = "".tt

    given concatenable: [text <: Text] => text is Concatenable:
      type Self = text
      type Operand = Text
      type Result = Text

      def concat(left: text, right: Text): Text = make(left.s+right.s)

    given addableString: [text <: Text] => text is Addable:
      type Self = text
      type Operand = String
      type Result = Text

      def add(left: text, right: String): Text = (left.s+right).tt

    private def recur(text: Text, n: Int, acc: Text): Text =
      if n == 0 then acc else recur(text, n - 1, make(acc.s + text.s))

    given multiplicable: [text <: Text] => text is Multiplicable:
      type Self = Text
      type Operand = Int
      type Result = Text

      def multiply(text: Text, n: Int): Text = recur(text, n.max(0), "".tt)

    given fromString: CommandLineParser.FromString[Text] = make(_)

    // The collection-typeclass instances. These lived in murmuration until it shed its
    // dependency on this module (making the collection vocabulary available to the
    // libraries between `proscenium` and here); implicit scope finds them equally well
    // from the subject's companion. `Self` is subtype-parametric throughout, so
    // intersections like `Text & Populated` (from `occupied`) also match.
    given traversable: [text <: Text] => text is murmuration.Traversable by Char = _.s.iterator

    given reshapable: [text <: Text]
    =>  text is murmuration.Reshapable.Stable by Char to Text =
      chars => make(String(chars.toArray))

    given inclusive: Text is murmuration.Inclusive by Char =
      (text, char) => text.s.indexOf(char.toInt) >= 0

    given convertible: [self] => (traversable: self is murmuration.Traversable by Char)
    =>  self is murmuration.Convertible in Text to Text =
      self => make(traversable.traverse(self).mkString)

    // `StringBuilder#reverse` is surrogate-pair-aware.
    given reversible: [text <: Text] => (text is murmuration.Reversible { type Result = Text }) =
      new murmuration.Reversible:
        type Self = text
        type Result = Text
        def reverse(value: text): Text = make(StringBuilder(value.s).reverse.nn.toString.nn)

    given fromExpr: (fromExpr: FromExpr[String]) => FromExpr[Text]:
      def unapply(expr: Expr[Text])(using Quotes): Option[Text] = fromExpr.unapply(expr).map(make)

    given toExpr: ToExpr[Text]:
      def apply(text: Text)(using Quotes) =
        import quotes.reflect.*

        val expr = Literal(StringConstant(text)).asExprOf[String]
        '{Text($expr)}

    given conversion: Conversion[String, Text] = make(_)
    inline given canEqual: CanEqual[Text, Text] = caps.unsafe.unsafeErasedValue
    inline given canEqual2: CanEqual[String, Text] = caps.unsafe.unsafeErasedValue
    inline given canEqual3: CanEqual[Text, String] = caps.unsafe.unsafeErasedValue

    given typeable: Typeable[Text]:
      def unapply(value: Any): Option[value.type & Text] = value.asMatchable match
        case string: String => Some(make(string).asInstanceOf[value.type & Text])
        case _              => None
