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
package cataclysm

import scala.language.dynamics

import proscenium.compat.iterator

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gesticulate.*
import gossamer.*
import parasite.*
import prepositional.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*
import denominative.*
import iridescence.*
import quantitative.*

object Css:
  // Reading a stylesheet accumulates every `Css.Error` (unknown property, invalid
  // or unsupported value, …) instead of stopping at the first: the parse runs
  // inside a `track`, and any errors are folded into a single `Css.Errors` raised
  // at the end. A fully-valid stylesheet yields the `Css` with nothing raised. In
  // the companion, so aggregation resolves through implicit scope with no import.
  given aggregable: (Tactic[Css.Errors], Diagnostics) => Css is Aggregable by Text = source =>
    track[Text](Css.Errors(Nil)):
      case error: Css.Error => accrual + error

    . protect:
        CssParser.parse(source.iterator)

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

  given streamable: (Monitor, Probate, Formatting) => Css is Streamable by Text over Credit = css =>
    val producer = Producer[Text](4096)

    async:
      write(css)(producer.put(_))
      producer.finish()

    Stream(producer.iterator)

  given showable: Formatting => Css is Showable = css =>
    Producer.collect[Text](): producer => write(css)(producer.put(_))

  // Serializes a `Css` tree back to CSS text, driving `put` once per chunk. Shared by the
  // `Showable` (collect) and `Streamable` (lazy producer) instances above — they differ only in
  // how the sink is driven, so a large stylesheet never needs to be held in memory at once. The
  // output style is chosen by the contextual `Css.Formatting`.
  private def write(css: Css)(put: Text => Unit)(using formatter: Css.Formatting): Unit =
    def newline(indent: Int): Unit = if formatter.newlines then put(indentText(indent))

    def block(body: List[Css.Node], indent: Int): Unit =
      put(if formatter.spaces then t" {" else t"{")

      body.stdlib.foreach: child =>
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

        body.lay(put(t";")): nodes => block(nodes, indent)

    var first = true

    css.rules.stdlib.foreach: child =>
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
      Css(List.of(left.rules.stdlib ++ right.rules.stdlib))

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
        Style.of(List.of(left.properties.stdlib ++ right.properties.stdlib))

  class Style private (val properties: List[(Text, Text)]):
    def text: Text = List.of(properties.stdlib.map { (name, value) => t"$name: $value" }).join(t"; ")

  // A typed CSS value tagged with its value-definition-syntax type (e.g.
  // `Css.Value of "length"`). Native types convert in via `Css.Convertible`; the
  // type is `into`, so a colour or quantity is accepted wherever a value of the
  // matching VDS type is expected.
  object Value:
    def apply(text: Text): Value =
      val text0 = text

      new Value:
        def text: Text = text0

    given converter: [value] => (convertible: value is Css.Convertible)
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

  // CssError → Css.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case UnterminatedComment        => m"a comment was not terminated"
        case UnterminatedString         => m"a string literal was not terminated"
        case UnexpectedEnd              => m"the input ended before a rule was closed"
        case UnexpectedChar(char)       => m"the character $char was not expected here"
        case EmptySelector              => m"a selector was expected but none was found"
        case UnknownProperty(name)      => m"$name is not a recognized CSS property"
        case BadValue(property, value)  => m"$value is not a valid value for the $property property"
        case UnsupportedValue(name, _)  => m"the value of $name uses an unsupported type"
        case InvalidName(name)          => m"$name is not a valid CSS identifier"

    enum Reason(val number: Int) extends Clarification:
      case UnterminatedComment         extends Reason(1)
      case UnterminatedString          extends Reason(2)
      case UnexpectedEnd               extends Reason(3)
      case UnexpectedChar(char: Char)  extends Reason(4)
      case EmptySelector               extends Reason(5)
      case UnknownProperty(name: Text) extends Reason(6)
      case BadValue(property: Text, value: Text) extends Reason(7)
      case UnsupportedValue(property: Text, types: List[Text]) extends Reason(8)
      case InvalidName(name: Text) extends Reason(9)

  case class Error(reason: Css.Error.Reason, line: Ordinal, column: Ordinal)(using Diagnostics)
  extends fulminate.Error(251, reason.number)(m"invalid CSS at line ${line.n1} column ${column.n1}: $reason")

  // CssErrors → Css.Errors
  // The aggregate of every `Css.Error` accumulated while reading a stylesheet.
  // `read[Css]` collects all errors rather than stopping at the first, raising
  // this once at the end (or returning the `Css` if there were none).
  case class Errors(errors: List[Css.Error])(using Diagnostics)
  extends fulminate.Error(m"the CSS contained ${errors.stdlib.length} errors"):
    def + (error: Css.Error): Css.Errors = Css.Errors(List.of(errors.stdlib :+ error))

  // CssConvertible → Css.Convertible
  // Records that a native Scala type renders to a CSS value of the value-definition
  // type `Topic` (e.g. `"length"`, `"color"`). A single generic `Conversion` (in
  // `Css.Value`) lifts any such type into a `Css.Value of Topic`, so a new
  // convertible type costs one instance here, not one given per CSS property.
  object Convertible:
    given pixels: (Quantity[Pixels[1]] is Css.Convertible of "length") = q => t"${number(q.value)}px"
    given ems: (Quantity[Ems[1]] is Css.Convertible of "length") = q => t"${number(q.value)}em"
    given rems: (Quantity[Rems[1]] is Css.Convertible of "length") = q => t"${number(q.value)}rem"
    given exs: (Quantity[Exs[1]] is Css.Convertible of "length") = q => t"${number(q.value)}ex"
    given chs: (Quantity[Chs[1]] is Css.Convertible of "length") = q => t"${number(q.value)}ch"

    given vws: (Quantity[ViewportWidths[1]] is Css.Convertible of "length") =
      q => t"${number(q.value)}vw"

    given vhs: (Quantity[ViewportHeights[1]] is Css.Convertible of "length") =
      q => t"${number(q.value)}vh"

    given vmins: (Quantity[ViewportMins[1]] is Css.Convertible of "length") =
      q => t"${number(q.value)}vmin"

    given vmaxes: (Quantity[ViewportMaxes[1]] is Css.Convertible of "length") =
      q => t"${number(q.value)}vmax"

    // Any physical length (Quantitative's `Distance` dimension) renders in `mm`; CSS
    // reads `cm`/`mm`/etc. consistently, so the rendered unit need not match how the
    // value was written. `q.value` is in metres, hence the factor of 1000.
    given metres: (Quantity[Metres[1]] is Css.Convertible of "length") =
      q => t"${number(q.value*1000)}mm"

    given inches: (Quantity[Inches[1]] is Css.Convertible of "length") = q => t"${number(q.value)}in"
    given points: (Quantity[Points[1]] is Css.Convertible of "length") = q => t"${number(q.value)}pt"
    given picas: (Quantity[Picas[1]] is Css.Convertible of "length") = q => t"${number(q.value)}pc"

    given percents: (Quantity[Percents[1]] is Css.Convertible of "percentage") =
      q => t"${number(q.value)}%"

    given srgb: (Srgb is Css.Convertible of "color") =
      color => hex((color.red*255).toInt, (color.green*255).toInt, (color.blue*255).toInt)

    given chroma: (Chroma is Css.Convertible of "color") =
      color => hex(color.red, color.green, color.blue)

    // Likewise any time (Quantitative's `Seconds`) renders in `ms`; `q.value` is in
    // seconds, hence the factor of 1000.
    given seconds: (Quantity[Seconds[1]] is Css.Convertible of "time") =
      q => t"${number(q.value*1000)}ms"

    given degrees: (Quantity[Degrees[1]] is Css.Convertible of "angle") = q => t"${number(q.value)}deg"
    given radians: (Quantity[Radians[1]] is Css.Convertible of "angle") = q => t"${number(q.value)}rad"

    given turns: (Quantity[Turns[1]] is Css.Convertible of "angle") =
      q => t"${number(q.value)}turn"

    given flexes: (Quantity[Flexes[1]] is Css.Convertible of "flex") = q => t"${number(q.value)}fr"

    given integer: (Int is Css.Convertible of "integer") = _.show
    given decimal: (Double is Css.Convertible of "number") = Css.number(_)

    given keyword: (Css.Keyword is Css.Convertible of "*") = _ match
      case Css.Keyword.Inherit     => t"inherit"
      case Css.Keyword.Initial     => t"initial"
      case Css.Keyword.Unset       => t"unset"
      case Css.Keyword.Revert      => t"revert"
      case Css.Keyword.RevertLayer => t"revert-layer"

    given colorKeyword: (Css.ColorKeyword is Css.Convertible of "color") = _ match
      case Css.ColorKeyword.Transparent  => t"transparent"
      case Css.ColorKeyword.CurrentColor => t"currentcolor"

    private def number(value: Double): Text = Css.number(value)

    private def hex(red: Int, green: Int, blue: Int): Text =
      def channel(value: Int): Text = String.format("%02x", value.max(0).min(255)).nn.tt
      t"#${channel(red)}${channel(green)}${channel(blue)}"

  trait Convertible extends Typeclass, Topical:
    def value(self: Self): Text

case class Css(rules: List[Css.Node]) derives CanEqual
