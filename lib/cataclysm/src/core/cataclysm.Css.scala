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

import language.dynamics

import anticipation.*
import gesticulate.*
import gossamer.*
import parasite.*
import prepositional.*
import spectacular.*
import turbulence.*
import vacuous.*

object Css:
  enum Node derives CanEqual:
    case Rule(selector: SelectorList, body: List[Node])
    case Declaration(property: Text, value: Text)
    case At(name: Text, prelude: Text, body: Optional[List[Node]])

  given streamable: (Monitor, Codicil, CssFormatter) => Css is Streamable by Text =
    CssSerializer.emit(_).to(Stream)

  given showable: CssFormatter => Css is Showable = CssSerializer.render(_)

  // Serve a stylesheet as an HTTP `text/css` response body (paired with the
  // `Streamable` instance above).
  given media: Css is Media = _ => media"text/css"(charset = "UTF-8")

  // A set of declarations for an inline `style="…"` attribute, built with a typed
  // dynamic constructor: `Css.Style(borderWidth = 4.0*Px, color = rgb)`. Property
  // names are camelCase (converted to kebab-case) and each value is checked
  // against the property's grammar at compile time (see `StyleMacros`).
  object Style extends Dynamic:
    // The private primary constructor suppresses the synthetic constructor proxy,
    // so `Css.Style(borderWidth = …)` routes to `applyDynamicNamed` rather than
    // failing against a positional `apply`. `of` is the factory the macro emits.
    def of(properties: List[(Text, Text)]): Style = new Style(properties)
    def applyDynamic(method: "apply")(): Style = of(Nil)

    inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Style =
      ${StyleMacros.style('properties)}

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
  private[cataclysm] def number(value: Double): Text =
    if value.isFinite && value == value.floor then value.toLong.show else value.toString.tt

  into trait Value extends Topical:
    def text: Text

case class Css(rules: List[Css.Node]) derives CanEqual
