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

import proscenium.compat.*

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import nomenclature.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

// The `css"…"` typed CSS interpolator, wired through `contextual` like xylophone's
// `x"…"` and honeycomb's `h"…"`. The transparent result is a `Css` (stylesheet) or a
// `Css.Style` (inline style set), decided by the content (see `internal.expand`).
extension (inline context: StringContext)
  transparent inline def css: Interpolation = interpolation[Css | Css.Style](context)

// The class and id names referenced anywhere in a stylesheet, including inside
// nested rules and the selector-list arguments of `:is()`/`:not()`/`:nth-…(of)`.
extension (css: Css)
  def classes: Set[Name[CssClass]] =
    Set.from(simples(css.rules).stdlib.collect { case Simple.Class(name) => name })

  def ids: Set[Name[DomId]] =
    Set.from(simples(css.rules).stdlib.collect { case Simple.Id(name) => name })

private def simples(nodes: List[Css.Node]): List[Simple] =
  nodes.bind:
    case Css.Node.Rule(selector, body) =>
      List.of(listSimples(selector).stdlib ++ simples(body).stdlib)
    case Css.Node.At(_, _, body)       => body.lay(Nil)(simples)
    case Css.Node.Declaration(_, _)    => Nil

private def listSimples(list: SelectorList): List[Simple] =
  list.selectors.bind: selector =>
    ((selector.head :: selector.rest.map(_(1))): List[Compound]).bind(compoundSimples)

private def compoundSimples(compound: Compound): List[Simple] =
  compound.parts.bind:
    case simple@ Simple.PseudoClass(_, argument)   =>
      (simple :: argumentSimples(argument)): List[Simple]
    case simple@ Simple.PseudoElement(_, argument) =>
      (simple :: argumentSimples(argument)): List[Simple]
    case simple                                    => List(simple)

private def argumentSimples(argument: Optional[PseudoArgument]): List[Simple] =
  argument.lay(Nil):
    case PseudoArgument.Selectors(list) => listSimples(list)
    case PseudoArgument.Nth(_, _, of)   => of.lay(Nil)(listSimples)
    case PseudoArgument.Raw(_)          => Nil

package formatting:
  given standardCssFormatting: Css.Formatting = Css.Formatting(newlines = true, spaces = true)
  given compactCssFormatting: Css.Formatting = Css.Formatting(newlines = false, spaces = false)
