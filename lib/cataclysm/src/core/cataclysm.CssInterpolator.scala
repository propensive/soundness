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
import nomenclature.*
import spectacular.*
import vacuous.*

// The macro behind the `css"…"` interpolator. The literal parts are joined with a
// sentinel char at each `$substitution`, parsed at compile time, and the parsed
// `Css` is rebuilt as an `Expr` — a declaration value that is a lone sentinel
// becomes the (type-checked) substitution, everything else is literal. Each such
// substitution's type must have a `CssConvertible` whose VDS type is valid for that
// property, so `css"a { color: $length }"` does not compile.
//
// A substitution within a selector is also permitted, but only of a `Name[CssClass]`
// or a `Name[DomId]`, which render as the simple selectors `.class` and `#id`
// respectively; so `css"$button { … }"` is a rule for the class `button`. Anywhere
// else (a property name, an at-rule prelude, or mixed with literal property text) is
// a compile error.
//
// NB: `Optional`'s `let`/`lay`/… are inline and crash `pickleQuotes` when used
// around the quotes below, so this file branches on `Optional` with plain `match`.
object CssInterpolator:
  // A private-use sentinel marks each substitution. NUL would be removed by the
  // parser's `.trim` (it is ≤ space), so a non-whitespace char is needed.
  private val sentinel: Char = 0xe000.toChar
  private val placeholder: Text = sentinel.toString.tt

  def expand[parts <: Tuple: Type, origins <: Tuple: Type]
    (insertions0: Expr[Seq[Any]])(using Quotes)
  :   Expr[Css] =

    import quotes.reflect.*

    val insertions: Seq[Expr[Any]] = insertions0 match
      case Varargs(insertions) => insertions
      case _                   => Nil

    def partList[tuple: Type]: List[String] = Type.of[tuple] match
      case '[head *: tail] => TypeRepr.of[head] match
        case ConstantType(StringConstant(part)) => part :: partList[tail]
        case _                                  => halt(m"cataclysm: a CSS part was not a literal")

      case _ =>
        Nil

    // `contextual` builds the `Transport` tuple in reverse, so restore source order.
    val parts = partList[parts].reverse
    val joined = parts.mkString(sentinel.toString).tt

    val css: Css = safely(CssParser.parse(Iterator(joined), validating = false)).or:
      halt(m"cataclysm: invalid CSS, or a substitution outside a property-value position")

    def has(text: Text): Boolean = text.s.contains(sentinel.toString)
    def lift(value: Text): Expr[Text] = '{${Expr(value.s)}.tt}

    var holeIndex = 0

    // The rendered text of the next substitution, type-checked against `property`.
    def hole(property: Text): Expr[Text] =
      val expr = insertions(holeIndex)
      holeIndex += 1
      val pos = expr.asTerm.underlyingArgument.pos

      expr match
        case '{$value: tpe} => Expr.summon[(? >: tpe) is CssConvertible] match
          case Some(convertible) =>
            CssMacros.propertyIssue(property, CssMacros.topicOf(convertible)) match
              case message: Message => halt(message, pos)
              case _                => ()

            '{$convertible.value($value)}

          case None =>
            halt(m"cataclysm: ${TypeRepr.of[tpe].show} cannot be used as a CSS value", pos)

        case _ =>
          halt(m"cataclysm: the substitution could not be read")

    // The selector fragment for the next substitution, which must be a name: a
    // `Name[CssClass]` renders as `.class` and a `Name[DomId]` as `#id`.
    def selectorFragment(): Expr[Text] =
      val expr = insertions(holeIndex)
      holeIndex += 1
      val pos = expr.asTerm.underlyingArgument.pos

      expr match
        case '{$value: tpe} =>
          if TypeRepr.of[tpe] <:< TypeRepr.of[Name[CssClass]]
          then '{(${Expr(".")} + ${value.asExprOf[Text]}.s).tt}
          else if TypeRepr.of[tpe] <:< TypeRepr.of[Name[DomId]]
          then '{(${Expr("#")} + ${value.asExprOf[Text]}.s).tt}
          else halt(m"cataclysm: only a CSS class or DOM id may be substituted in a selector", pos)

        case _ =>
          halt(m"cataclysm: the substitution could not be read")

    // Rebuild a selector containing substitutions: split its rendered text at each
    // sentinel and interleave the (name-typed) holes between the literal segments.
    def assembleSelector(text: Text): Expr[SelectorList] =
      val segments = text.cut(placeholder)
      var acc: Expr[String] = Expr(segments.head.s)

      for segment <- segments.tail do
        acc = '{$acc + ${selectorFragment()}.s + ${Expr(segment.s)}}

      '{SelectorList.read($acc.tt)}

    def listExpr(nodes: List[Css.Node]): Expr[List[Css.Node]] = Expr.ofList(nodes.map(nodeExpr))

    def nodeExpr(node: Css.Node): Expr[Css.Node] = node match
      case Css.Node.Rule(selector, body) =>
        val selectorExpr =
          if has(selector.show) then assembleSelector(selector.show)
          else '{SelectorList.read(${lift(selector.show)})}

        '{Css.Node.Rule($selectorExpr, ${listExpr(body)})}

      case Css.Node.Declaration(property, value) =>
        if has(property) then
          halt(m"cataclysm: a substitution may only be a property value, not a property name")
        else if value == placeholder then
          '{Css.Node.Declaration(${lift(property)}, ${hole(property)})}
        else if has(value) then
          halt(m"cataclysm: a substitution must be a whole property value, not mixed with text")
        else
          '{Css.Node.Declaration(${lift(property)}, ${lift(value)})}

      case Css.Node.At(name, prelude, body) =>
        if has(name) || has(prelude) then
          halt(m"cataclysm: a substitution may only be a property value, not part of an at-rule")

        body match
          case Unset =>
            '{Css.Node.At(${lift(name)}, ${lift(prelude)}, Unset)}

          case nodes: List[Css.Node] @unchecked =>
            '{Css.Node.At(${lift(name)}, ${lift(prelude)}, ${listExpr(nodes)})}

    val result = '{Css(${listExpr(css.rules)})}

    if holeIndex != insertions.length
    then halt(m"cataclysm: a substitution is only allowed as a property value or in a selector")

    result
