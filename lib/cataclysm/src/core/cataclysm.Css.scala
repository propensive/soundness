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

import language.dynamics

import anticipation.*
import contextual.*
import gesticulate.*
import gossamer.*
import parasite.*
import prepositional.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Css:
  // Controls how a `Css` tree is serialized. `newlines` puts each rule and declaration on its own
  // indented line; `spaces` adds the cosmetic spaces (after `:` and before `{`). Bundled as
  // `formatting.standardCssFormatting` and `formatting.compactCssFormatting`.
  object Formatting:
    def apply(newlines: Boolean, spaces: Boolean): Formatting = Basic(newlines, spaces)
    private case class Basic(newlines: Boolean, spaces: Boolean) extends Formatting

  trait Formatting extends zephyrine.Formatting:
    def newlines: Boolean
    def spaces: Boolean

  enum Node derives CanEqual:
    case Rule(selector: SelectorList, body: List[Node])
    case Declaration(property: Text, value: Text)
    case At(name: Text, prelude: Text, body: Optional[List[Node]])


  given streamable: (Monitor, Probate, Formatting) => Css is Streamable by Text = css =>
    val producer = Producer[Text](4096)

    async:
      write(css)(producer.put(_))
      producer.finish()

    producer.iterator.to(LazyList)

  given showable: Formatting => Css is Showable = css =>
    Producer.collect[Text](): producer =>
      write(css)(producer.put(_))

  // Serializes a `Css` tree back to CSS text, driving `put` once per chunk. Shared by the
  // `Showable` (collect) and `Streamable` (lazy producer) instances above — they differ only in
  // how the sink is driven, so a large stylesheet never needs to be held in memory at once. The
  // output style is chosen by the contextual `Css.Formatting`.
  private def write(css: Css)(put: Text => Unit)(using formatter: Css.Formatting): Unit =
    def newline(indent: Int): Unit = if formatter.newlines then put(indentText(indent))

    def block(body: List[Css.Node], indent: Int): Unit =
      put(if formatter.spaces then t" {" else t"{")

      body.foreach: child =>
        newline(indent + 1)
        emitNode(child, indent + 1)

      newline(indent)
      put(t"}")

    def emitNode(node: Css.Node, indent: Int): Unit = node match
      case Css.Node.Rule(selector, body) =>
        put(selector.show)
        block(body, indent)

      case Css.Node.Declaration(property, value) =>
        put(property)
        put(if formatter.spaces then t": " else t":")
        put(value)
        put(t";")

      case Css.Node.At(name, prelude, body) =>
        put(t"@$name")
        if prelude != t"" then put(t" $prelude")

        body.lay(put(t";")): nodes =>
          block(nodes, indent)

    var first = true

    css.rules.foreach: child =>
      if first then first = false else newline(0)
      emitNode(child, 0)

    if formatter.newlines then put(t"\n")

  private def indentText(indent: Int): Text = ("\n" + " ".repeat(2*indent).nn).tt

  // The `css"…"` interpolator: substitutions are checked against the property they
  // sit in (see `internal.expand`). Wired through `contextual` like `x"…"`/`h"…"`.
  // The result type is decided by the content: bare declarations (`css"width: 38px"`)
  // produce a `Css.Style`, a selector-bearing or at-rule body (`css"a { … }"`) a `Css`.
  inline given interpolator: (Css | Css.Style) is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Css | Css.Style =

      ${cataclysm.internal.expand[parts, origins]('insertions)}

  // Stylesheets concatenate their rule lists, so `css"a { … }" + css"b { … }"` is one
  // stylesheet of both rules.
  given addable: Css is Addable by Css to Css =
    Addable: (left, right) =>
      Css(left.rules ++ right.rules)

  // Serve a stylesheet as an HTTP `text/css` response body (paired with the
  // `Streamable` instance above).
  given media: Css is Media:
    extension (value: Css)
      def mediaType: MediaType = media"text/css"(charset = "UTF-8")

  // A set of declarations for an inline `style="…"` attribute, built with a typed
  // dynamic constructor: `Css.Style(borderWidth = 4.0*Px, color = rgb)`. Property
  // names are camelCase (converted to kebab-case) and each value is checked
  // against the property's grammar at compile time (see `internal.style`).
  object Style extends Dynamic:
    // The private primary constructor suppresses the synthetic constructor proxy,
    // so `Css.Style(borderWidth = …)` routes to `applyDynamicNamed` rather than
    // failing against a positional `apply`. `of` is the factory the macro emits.
    def of(properties: List[(Text, Text)]): Style = new Style(properties)
    def applyDynamic(method: "apply")(): Style = of(Nil)

    inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Style =
      ${cataclysm.internal.style('properties)}

    // Inline-style sets concatenate their property lists, so two `Css.Style`s (or two
    // bare `css"…"`s) join into one.
    given addable: Style is Addable by Style to Style =
      Addable: (left, right) =>
        Style.of(left.properties ++ right.properties)

  class Style private (val properties: List[(Text, Text)]):
    def text: Text = properties.map { (name, value) => t"$name: $value" }.join(t"; ")

  // A typed CSS value tagged with its value-definition-syntax type (e.g.
  // `Css.Value of "length"`). Native types convert in via `CssConvertible`; the
  // type is `into`, so a colour or quantity is accepted wherever a value of the
  // matching VDS type is expected.
  object Value:
    def apply(text: Text): Value =
      val text0 = text

      new Value:
        def text: Text = text0

    given converter: [value] => (convertible: value is CssConvertible)
    =>  Conversion[value, Value of convertible.Topic] = instance =>
      Value(convertible.value(instance)).asInstanceOf[Value of convertible.Topic]

  // Render a number for CSS, dropping a redundant `.0` (so `12px`, not `12.0px`).
  // Values are rounded to six decimal places first, so unit conversions (e.g. a
  // length in metres scaled to `mm`) don't leak binary floating-point noise like
  // `30.000000000000004`.
  private[cataclysm] def number(value: Double): Text =
    if !value.isFinite then value.toString.tt
    else
      val rounded = Math.rint(value*1000000.0)/1000000.0
      if rounded == rounded.floor then rounded.toLong.show else rounded.toString.tt

  // The CSS-wide keywords (valid for every property) and the colour keywords, as
  // typed values for `Css.Style(…)` and `css"…"`, e.g. `Css.Style(color = Css.inherit)`
  // or `css"a { color: ${Css.transparent} }"`. The lowercase accessors avoid the
  // clash a bare `Unset` would have with `vacuous.Unset`.
  enum Keyword derives CanEqual:
    case Inherit, Initial, Unset, Revert, RevertLayer

  enum ColorKeyword derives CanEqual:
    case Transparent, CurrentColor

  val inherit: Keyword = Keyword.Inherit
  val initial: Keyword = Keyword.Initial
  val unset: Keyword = Keyword.Unset
  val revert: Keyword = Keyword.Revert
  val revertLayer: Keyword = Keyword.RevertLayer
  val transparent: ColorKeyword = ColorKeyword.Transparent
  val currentColor: ColorKeyword = ColorKeyword.CurrentColor

  into trait Value extends Topical:
    def text: Text

case class Css(rules: List[Css.Node]) derives CanEqual
