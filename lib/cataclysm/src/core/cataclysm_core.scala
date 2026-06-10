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

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import nomenclature.*
import prepositional.*
import turbulence.*
import vacuous.*

// The `css"…"` typed CSS interpolator, wired through `contextual` like xylophone's
// `x"…"` and honeycomb's `h"…"`.
extension (inline context: StringContext)
  transparent inline def css: Interpolation = interpolation[Css](context)

// Reading a stylesheet accumulates every `CssError` (unknown property, invalid
// or unsupported value, …) instead of stopping at the first: the parse runs
// inside a `track`, and any errors are folded into a single `CssErrors` raised
// at the end. A fully-valid stylesheet yields the `Css` with nothing raised.
given cssAggregable: (Tactic[CssErrors], Diagnostics) => Css is Aggregable by Text = source =>
  track[Text](CssErrors(Nil)):
    case error: CssError => accrual + error

  . within:
      CssParser.parse(source.iterator)

// The class and id names referenced anywhere in a stylesheet, including inside
// nested rules and the selector-list arguments of `:is()`/`:not()`/`:nth-…(of)`.
extension (css: Css)
  def classes: Set[Name[CssClass]] =
    simples(css.rules).collect { case Simple.Class(name) => name }.to(Set)

  def ids: Set[Name[DomId]] = simples(css.rules).collect { case Simple.Id(name) => name }.to(Set)

private def simples(nodes: List[Css.Node]): List[Simple] =
  nodes.flatMap:
    case Css.Node.Rule(selector, body) => listSimples(selector) ++ simples(body)
    case Css.Node.At(_, _, body)       => body.lay(Nil)(simples)
    case Css.Node.Declaration(_, _)    => Nil

private def listSimples(list: SelectorList): List[Simple] =
  list.selectors.flatMap: selector =>
    (selector.head :: selector.rest.map(_(1))).flatMap(compoundSimples)

private def compoundSimples(compound: Compound): List[Simple] =
  compound.parts.flatMap:
    case simple@ Simple.PseudoClass(_, argument)   => simple :: argumentSimples(argument)
    case simple@ Simple.PseudoElement(_, argument) => simple :: argumentSimples(argument)
    case simple                                    => List(simple)

private def argumentSimples(argument: Optional[PseudoArgument]): List[Simple] =
  argument.lay(Nil):
    case PseudoArgument.Selectors(list) => listSimples(list)
    case PseudoArgument.Nth(_, _, of)   => of.lay(Nil)(listSimples)
    case PseudoArgument.Raw(_)          => Nil

package cssFormatters:
  given standard: CssFormatter = CssFormatter(newlines = true, spaces = true)
  given compact: CssFormatter = CssFormatter(newlines = false, spaces = false)
