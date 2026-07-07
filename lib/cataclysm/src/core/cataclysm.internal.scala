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
┃    Soundness, version 0.63.0.                                                                    ┃
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

object internal:
  // A private-use sentinel marks each substitution. NUL would be removed by the
  // parser's `.trim` (it is ≤ space), so a non-whitespace char is needed.
  private val sentinel: Char = 0xe000.toChar
  private val placeholder: Text = sentinel.toString.tt

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
  def expand[parts <: Tuple: Type, origins <: Tuple: Type]
    (insertions0: Expr[Seq[Any]])(using Quotes)
  :   Expr[Css | Css.Style] =

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
            propertyIssue(property, topicOf(convertible)) match
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

    // The (type-checked) value of a declaration: a lone sentinel becomes the typed
    // substitution; a substitution mixed with literal text, or in the property name,
    // is an error. Shared by the stylesheet (`Css`) and inline-style (`Css.Style`) builds.
    def declarationValue(property: Text, value: Text): Expr[Text] =
      if has(property) then
        halt(m"cataclysm: a substitution may only be a property value, not a property name")
      else if value == placeholder then
        hole(property)
      else if has(value) then
        halt(m"cataclysm: a substitution must be a whole property value, not mixed with text")
      else
        lift(value)

    def listExpr(nodes: List[Css.Node]): Expr[List[Css.Node]] = Expr.ofList(nodes.map(nodeExpr))

    def nodeExpr(node: Css.Node): Expr[Css.Node] = node match
      case Css.Node.Rule(selector, body) =>
        val selectorExpr =
          if has(selector.show) then assembleSelector(selector.show)
          else '{SelectorList.read(${lift(selector.show)})}

        '{Css.Node.Rule($selectorExpr, ${listExpr(body)})}

      case Css.Node.Declaration(property, value) =>
        '{Css.Node.Declaration(${lift(property)}, ${declarationValue(property, value)})}

      case Css.Node.At(name, prelude, body) =>
        if has(name) || has(prelude) then
          halt(m"cataclysm: a substitution may only be a property value, not part of an at-rule")

        (body: @unchecked) match
          case Unset =>
            '{Css.Node.At(${lift(name)}, ${lift(prelude)}, Unset)}

          case nodes: List[Css.Node] @unchecked =>
            '{Css.Node.At(${lift(name)}, ${lift(prelude)}, ${listExpr(nodes)})}

    def isDeclaration(node: Css.Node): Boolean = node match
      case Css.Node.Declaration(_, _) => true
      case _                          => false

    // A bare declaration in inline-style position, e.g. `("color", $value)`.
    def stylePair(node: Css.Node): Expr[(Text, Text)] = node match
      case Css.Node.Declaration(property, value) =>
        '{(${lift(property)}, ${declarationValue(property, value)})}

      case _ =>
        halt(m"cataclysm: only declarations may appear in an inline style")

    // Detect the kind of CSS from its content: top-level declarations with no selector
    // or at-rule are an inline style set (`Css.Style`); anything with a rule or at-rule
    // is a stylesheet (`Css`). The `transparent inline` interpolator returns whichever.
    val result: Expr[Css | Css.Style] =
      if css.rules.nonEmpty && css.rules.forall(isDeclaration)
      then '{Css.Style.of(${Expr.ofList(css.rules.map(stylePair))})}
      else '{Css(${listExpr(css.rules)})}

    if holeIndex != insertions.length
    then halt(m"cataclysm: a substitution is only allowed as a property value or in a selector")

    result

  // Compile-time machinery behind the `Css.Style(borderWidth = …, color = …)`
  // dynamic constructor. Each named parameter's camelCase label becomes a
  // kebab-case property name; the value's type must have a `CssConvertible`
  // instance (which both renders it and tags it with a value-definition-syntax
  // type); and that type is checked against the property's grammar, so e.g.
  // `Css.Style(color = 4.0*Px)` and `Css.Style(notAProperty = …)` fail to compile.
  def style(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Css.Style] =
    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[(Text, Text)]] = exprs match
      case '{type key <: Label; ($key: key, $value: value)} +: tail =>
        val convertible = Expr.summon[value is CssConvertible].getOrElse:
          halt(m"cataclysm: no CSS value is available for this property's value")

        val name = key.value.getOrElse(halt(m"cataclysm: the property name must be a literal"))
        val property = name.tt.uncamel.kebab
        propertyIssue(property, topicOf(convertible)).let(halt(_))

        '{(${Expr(property)}, $convertible.value($value))} :: recur(tail)

      case _ =>
        Nil

    properties match
      case Varargs(exprs) => '{Css.Style.of(${Expr.ofList(recur(exprs))})}
      case _              => '{Css.Style.of(Nil)}

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
