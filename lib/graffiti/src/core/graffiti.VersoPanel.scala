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
package graffiti

import cataclysm.*
import honeycomb.*
import honeycomb.doms.html.whatwg.*
import nomenclature.*
import nomenclature.CssClass.nominative
import prepositional.*
import quantitative.*
import symbolism.*

object VersoPanel:
  val layoutClass: Name[CssClass] = n"graffiti-verso-layout"
  val versoClass: Name[CssClass] = n"graffiti-verso"
  val contentClass: Name[CssClass] = n"graffiti-verso-content"

// A layout trait that places a side panel on the inline-start edge (the left in left-to-right text,
// the right in right-to-left text — the "verso" page of a spread) beside the main content. The
// panel's material is supplied through the `verso` slot, which defaults to empty.
//
// Implemented as a two-column grid whose source order is verso-then-content; as the grid follows
// the writing direction, no `left`/`right` ever appears and `direction = Rtl` mirrors it for free.
trait VersoPanel extends Archetype:
  // The panel's content; empty by default (the slot renders nothing until filled).
  def verso: Html of (? <: Flow) = Fragment[Flow]()

  // The panel's inline size, and the gap between panel and content.
  def versoWidth: Quantity[Rems[1]] = 16.0*Rem
  def versoGap: Quantity[Rems[1]] = 1.0*Rem

  // How panel and content are arranged; override to change the container.
  protected def versoArrangement
    ( panel: Html of (? <: Flow), content: Html of (? <: Flow) )
  :   Html of (? <: Flow) =

    Fragment[Flow](Div(`class` = VersoPanel.layoutClass)(panel, content))

  // This feature's own rules; override to restyle the panel and its grid.
  protected def versoStyles: Css =
    val layout = VersoPanel.layoutClass
    css"$layout { display: grid; grid-template-columns: auto 1fr; gap: ${versoGap} }" +
      css"${VersoPanel.versoClass} { inline-size: ${versoWidth} }"

  protected override def frame: Html of (? <: Flow) =
    val panel = Aside(`class` = VersoPanel.versoClass)(verso)
    val content = Div(`class` = VersoPanel.contentClass)(super.frame)
    versoArrangement(panel, content)

  protected override def styles: Css = super.styles + versoStyles
