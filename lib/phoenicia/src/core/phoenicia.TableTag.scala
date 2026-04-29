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
package phoenicia

import anticipation.*
import contingency.*
import gossamer.*
import proscenium.*
import vacuous.*

sealed trait TableTag:
  def text: Text

object OtfTag extends Extractor[Text, OtfTag]:
  def extract(text: Text): Optional[OtfTag] = text match
    case t"OS/2" => Os2
    case t"CFF " => Cff
    case other   => safely(OtfTag.valueOf(other.lower.capitalize.s))

object TtfTag extends Extractor[Text, TtfTag]:
  def extract(text: Text): Optional[TtfTag] = text match
    case t"cvt " => Cvt
    case other   => safely(TtfTag.valueOf(other.lower.capitalize.s))

enum TtfTag extends TableTag:
  case
    Avar, Cmap, Cvar, Cvt, Fpgm, Fvar, Gasp, Glyf, Gvar, Hdmx, Head, Hhea, Hmtx, Kern, Loca, Maxp,
    Meta, Name, Post, Prep, Sbix, Vhea, Vmtx

  def text: Text = this match
    case Cvt   => t"cvt "
    case table => table.toString.tt.lower

enum OtfTag extends TableTag:
  case
    Base, Cbdt, Cblc, Cff, Cff2, Colr, Cpal, Dsig, Ebdt, Eblc, Ebsc, Gdef, Gpos, Gsub, Hvar, Jstf,
    Ltsh, Math, Merg, Mvar, Os2, Pclt, Stat, Svg, Vdmx, Vorg, Vvar

  def text: Text = this match
    case Os2   => t"OS/2"
    case Cff   => t"CFF "
    case table => table.toString.tt.upper
