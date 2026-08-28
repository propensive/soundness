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
package harlequin

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import stenography.*
import vacuous.*

object Token:
  val Newline: Token = Token("\n", Accent.Unparsed)
  given showable: Token is Showable = _.text

  // The `Showable` above is the token's text alone, which is what a rendered source listing
  // needs, but it hides the accent, span and role — the state a reader of highlighted source
  // is usually checking. Inspection shows each field, in product form. A `Meta` wraps a
  // `Syntax`, whose textual form needs an `Imports` context which inspection cannot supply, so
  // a present `Meta` is shown as `｢Meta｣`, without its type.
  given inspectable: [token <: Token] => token is Inspectable = token =>
    val meta = if token.meta.present then t"｢Meta｣" else t"○"
    val role = token.role.lay(t"○"): role => t"｢${role.inspect}｣"

    val fields =
      t"text:${token.text.inspect} ╱ accent:${token.accent.inspect} ╱ meta:$meta"

    t"Token($fields ╱ span:${token.span.inspect} ╱ role:$role)"

  case class Meta(tpe: Syntax)

// `span` locates the token in its `SourceCode`: a `Line`-mode `Span` carrying the
// token's 0-based line and column and its length. It is `Span.empty` until the
// token is placed into a `SourceCode`'s line grid. `role` distinguishes a binding from a
// usage for term (`Term`) and type (`Typal`) tokens, and is `Unset` for all others.
case class Token
  ( text:   Text,
    accent: Accent,
    meta:   Optional[Token.Meta] = Unset,
    span:   Span                 = Span.empty,
    role:   Optional[Role]       = Unset ):

  def length: Int = text.length

  def snip(point: Int): (Token, Token) =
    ( Token(text.keep(point), accent, meta, span, role),
      Token(text.skip(point), accent, meta, span, role) )
