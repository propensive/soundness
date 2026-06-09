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

// Compile-time machinery behind the `Css.Style(borderWidth = …, color = …)`
// dynamic constructor. Each named parameter's camelCase label becomes a
// kebab-case property name; the value's type must have a `CssConvertible`
// instance (which both renders it and tags it with a value-definition-syntax
// type); and that type is checked against the property's grammar, so e.g.
// `Css.Style(color = 4.0*Px)` and `Css.Style(notAProperty = …)` fail to compile.
object StyleMacros:
  def style(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Css.Style] =
    import quotes.reflect.*

    def refinements(repr: TypeRepr): Map[Text, TypeRepr] = repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

    // The value-definition-syntax type the convertible tags its values with.
    def topicOf(convertible: Expr[Any]): Text =
      val members = refinements(convertible.asTerm.tpe) ++ refinements(convertible.asTerm.tpe.widen)

      members.get(t"Topic") match
        case Some(ConstantType(StringConstant(name))) => name.tt
        case _                                        => t""

    // A canonical instance of a value type, used to probe the property's grammar.
    def sample(topic: Text): Optional[Text] = topic match
      case t"length"     => t"1px"
      case t"color"      => t"#000000"
      case t"percentage" => t"1%"
      case t"number"     => t"1.5"
      case t"integer"    => t"1"
      case _             => Unset

    def check(property: Text, topic: Text): Unit =
      val propertyDef = PropertyDef.of(property).or:
        halt(m"cataclysm: $property is not a known CSS property")

      sample(topic).lay(()): sampleText =>
        val outcome = safely(SyntaxMatcher.check(propertyDef, sampleText))

        if outcome.or(Outcome.Unsupported(Nil)) == Outcome.Invalid
        then halt(m"cataclysm: a $topic value is not valid for the $property property")

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(Text, Text)]] = exprs match
      case '{type key <: Label; ($key: key, $value: value)} +: tail =>
        val convertible = Expr.summon[value is CssConvertible].getOrElse:
          halt(m"cataclysm: no CSS value is available for this property's value")

        val name = key.value.getOrElse(halt(m"cataclysm: the property name must be a literal"))
        val property = name.tt.uncamel.kebab
        check(property, topicOf(convertible))

        '{(${Expr(property)}, $convertible.value($value))} :: recur(tail)

      case _ =>
        Nil

    properties match
      case Varargs(exprs) => '{Css.Style.of(${Expr.ofList(recur(exprs))})}
      case _              => '{Css.Style.of(Nil)}
