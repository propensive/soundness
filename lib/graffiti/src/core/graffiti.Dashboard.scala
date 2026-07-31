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
package graffiti

import anticipation.*
import cataclysm.*
import honeycomb.*
import honeycomb.doms.html.whatwg.*
import nomenclature.*
import nomenclature.CssClass.nominative
import prepositional.*
import symbolism.*

object Dashboard:
  val gridClass: Name[CssClass] = n"graffiti-dashboard"
  val cardClass: Name[CssClass] = n"graffiti-card"

  // One tile of a dashboard: a heading and its own flow content.
  case class Card(heading: Text, body: Html of (? <: Flow))

// A typical dashboard view, composed from the `Mainstay` (a `<main>` landmark) and `Masthead`
// features. It lays the cards out in a responsive grid as the page's main content and shows the
// `brand` in the masthead. A concrete dashboard supplies just the details left abstract below.
trait Dashboard extends Archetype, Mainstay, Masthead:
  // The details a concrete dashboard must provide.
  def brand: Text
  def cards: List[Dashboard.Card]

  // Title the document after the brand.
  override def pageTitle: Text = brand

  // Renders one card; override to restructure it.
  protected def card(entry: Dashboard.Card): Html of (? <: Flow) =
    Article(`class` = Dashboard.cardClass)(H2(entry.heading), entry.body)

  // The masthead shows the brand; the main content is the grid of cards.
  override def masthead: Html of (? <: Flow) = Strong(brand)
  def content: Html of (? <: Flow) = Div(`class` = Dashboard.gridClass)(cards.stdlib.map(card)*)

  // This view's own rules; override to restyle the grid or the cards.
  protected def dashboardStyles: Css =
    val grid = Dashboard.gridClass
    css"$grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(16rem, 1fr)) }" +
      css"$grid { gap: 1rem }" +
      css"${Dashboard.cardClass} { padding: 1rem; border: 1px solid; border-radius: 0.5rem }"

  protected override def styles: Css = super.styles + dashboardStyles
