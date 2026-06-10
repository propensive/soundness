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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package cataclysm

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import vacuous.*

// Compile-time helpers shared by the `Css.Style` dynamic constructor and the
// `css"…"` interpolator: reading a `CssConvertible`'s value-definition-syntax
// `Topic`, and checking a value of that type against a property's grammar.
private[cataclysm] object CssMacros:
  // The VDS type a `CssConvertible` instance tags its values with (or "" if none).
  def topicOf(using quotes: Quotes)(convertible: Expr[Any]): Text =
    import quotes.reflect.*

    def refinements(repr: TypeRepr): Map[Text, TypeRepr] = repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

    val members = refinements(convertible.asTerm.tpe) ++ refinements(convertible.asTerm.tpe.widen)

    members.get(t"Topic") match
      case Some(ConstantType(StringConstant(name))) => name.tt
      case _                                        => t""

  // A canonical instance of a value type, used to probe a property's grammar.
  private def sample(topic: Text): Optional[Text] = topic match
    case t"length"     => t"1px"
    case t"color"      => t"#000000"
    case t"percentage" => t"1%"
    case t"number"     => t"1.5"
    case t"integer"    => t"1"
    case t"time"       => t"1s"
    case t"angle"      => t"1deg"
    case t"flex"       => t"1fr"
    case _             => Unset

  // `Unset` if a value of VDS type `topic` is acceptable for `property`; otherwise
  // a `Message` describing why not (unknown property, or wrong value type).
  def propertyIssue(property: Text, topic: Text): Optional[Message] =
    PropertyDef.of(property).lay(m"cataclysm: $property is not a known CSS property"): definition =>
      // A wildcard topic (the CSS-wide keywords) is valid for any known property.
      if topic == t"*" then Unset
      else sample(topic).lay(Unset):
        sampleText =>
          val outcome = safely(SyntaxMatcher.check(definition, sampleText))

          if outcome.or(Outcome.Unsupported(Nil)) == Outcome.Invalid
          then m"cataclysm: a $topic value is not valid for the $property property"
          else Unset
