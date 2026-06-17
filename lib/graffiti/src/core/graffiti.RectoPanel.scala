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
package graffiti

import cataclysm.*
import honeycomb.*
import honeycomb.doms.html.whatwg.*
import nomenclature.*
import nomenclature.CssClass.nominative
import prepositional.*
import quantitative.*
import symbolism.*

object RectoPanel:
  val layoutClass: Name[CssClass] = n"graffiti-recto-layout"
  val rectoClass: Name[CssClass] = n"graffiti-recto"
  val contentClass: Name[CssClass] = n"graffiti-recto-content"

// A layout trait that places a side panel on the inline-end edge (the right in left-to-right text,
// the left in right-to-left text — the "recto" page of a spread) beside the main content. The
// panel's material is supplied through the `recto` slot, which defaults to empty.
//
// The source order is content-then-recto, so the panel lands on the inline-end column; like its
// verso counterpart it uses only logical properties, so `direction = Rtl` mirrors it for free.
trait RectoPanel extends Archetype:
  // The panel's content; empty by default (the slot renders nothing until filled).
  def recto: Html of (? <: Flow) = Fragment[Flow]()

  // The panel's inline size, and the gap between content and panel.
  def rectoWidth: Quantity[Rems[1]] = 16.0*Rem
  def rectoGap: Quantity[Rems[1]] = 1.0*Rem

  // How content and panel are arranged; override to change the container.
  protected def rectoArrangement
    ( content: Html of (? <: Flow), panel: Html of (? <: Flow) )
  :   Html of (? <: Flow) =

    Fragment[Flow](Div(`class` = RectoPanel.layoutClass)(content, panel))

  // This feature's own rules; override to restyle the panel and its grid.
  protected def rectoStyles: Css =
    val layout = RectoPanel.layoutClass
    css"$layout { display: grid; grid-template-columns: 1fr auto; gap: ${rectoGap} }" +
      css"${RectoPanel.rectoClass} { inline-size: ${rectoWidth} }"

  protected override def frame: Html of (? <: Flow) =
    val content = Div(`class` = RectoPanel.contentClass)(super.frame)
    val panel = Aside(`class` = RectoPanel.rectoClass)(recto)
    rectoArrangement(content, panel)

  protected override def styles: Css = super.styles + rectoStyles
