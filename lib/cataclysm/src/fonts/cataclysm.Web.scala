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

import scala.compiletime.constValue

import anticipation.*
import contingency.*
import phoenicia.*
import prepositional.*
import vacuous.*

object Web:
  // The instance the factories below return; a named class gives it a stable type.
  class Provision[family <: Label](val sources: List[Typesettable.Source], val coverage: Coverage)
  extends Typesettable:
    type Self = Typeface of family
    type Form = Web

  private def generic[family <: Label]: Typeface of family is Typesettable in Web =
    Provision[family](List(Typesettable.Source.Generic), Coverage.Unknown)

  // The CSS generic families: a browser sets each from whatever it has, so they are provided on
  // the web without anything being declared. These live here, in the medium's companion, which
  // is in the implicit scope of `(Typeface of "sans-serif") is Typesettable in Web`.
  given sansSerif: (Typeface of "sans-serif") is Typesettable in Web = generic
  given serif: (Typeface of "serif") is Typesettable in Web = generic
  given monospace: (Typeface of "monospace") is Typesettable in Web = generic
  given cursive: (Typeface of "cursive") is Typesettable in Web = generic
  given fantasy: (Typeface of "fantasy") is Typesettable in Web = generic
  given systemUi: (Typeface of "system-ui") is Typesettable in Web = generic

  // A font file the browser fetches from a URL, declared with the faces it offers, since the
  // file is not to hand to be read.
  def linked[family <: Label, url: Abstractable across Urls to Text]
    ( url: url, coverage: Coverage, format: Optional[Typesettable.Source.Format] = Unset )
  :   Typeface of family is Typesettable in Web =

    Provision[family](List(Typesettable.Source.Linked(url.generic, format)), coverage)

  // A font the reader is expected to have installed, matched by the typeface's name. Nothing
  // guarantees they do, so this is the weakest provision, and its coverage says only what is
  // declared.
  inline def local[family <: Label](coverage: Coverage = Coverage.Unknown)
  :   Typeface of family is Typesettable in Web =

    Provision[family](List(Typesettable.Source.Local(constValue[family].tt)), coverage)

  // A stylesheet, such as a font service's, that declares the typeface's `@font-face` rules
  // itself; the page imports it.
  def imported[family <: Label, url: Abstractable across Urls to Text](url: url, coverage: Coverage)
  :   Typeface of family is Typesettable in Web =

    Provision[family](List(Typesettable.Source.Imported(url.generic)), coverage)

  // The generic sans-serif as a font for the web, for a default that can never fail its coverage
  // check, since a generic family admits every face.
  val sansSerifFont: Font in Web = new Font(Typeface.SansSerif.face, sansSerif) { type Form = Web }

  // A font for the web, where the expected type does not name the medium itself. Inline, so
  // that the face's request reaches the compile-time check.
  inline def font[family <: Label](inline face: Face of family)
    ( using provision: Typeface of family is Typesettable in (? >: Web),
            tactic:    Tactic[Font.Error] )
  :   Font in Web =

    Font.of[Web](face)

// The web as a medium: text set by a browser from CSS, where a typeface is available if the
// page carries a `@font-face` for it, imports a stylesheet that does, or names a generic family.
trait Web extends Medium
