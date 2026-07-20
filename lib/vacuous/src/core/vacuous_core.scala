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
package vacuous

import java.util as ju

import scala.compiletime.*

import anticipation.*
import fulminate.*

import errorDiagnostics.stackTracesDiagnostics

inline def default[value](using default: Default[value]): value = default()

inline def optimizable[value](lambda: Optional[value] => Optional[value]): Optional[value] =
  lambda(Unset)

// Scoped acknowledgements of expensive operations: `trek(list.size)` grants the O(n) pass,
// `exhaust(stream.size)` grants forcing a lazy structure, within the block only. The context
// parameter is erased at consuming positions, so neither leaves any runtime residue.
inline def trek[result](inline block: (erased Trek) ?=> result): result = block(using Trek)

inline def exhaust[result](inline block: (erased Exhaust) ?=> result): result =
  block(using Exhaust)

// Blanket, per-file enablement of expensive operations: `import vacuous.expenditures.given`.
package expenditures:
  given trek: Trek = Trek
  given exhaust: Exhaust = Exhaust

export vacuous.Optional.Unset

type Optional[value] = Unset | value

extension [value](inline optional: Optional[value])
  inline def or(inline value: => value): value = ${vacuous.internal.optimizeOr('optional, 'value)}

extension [value](value: value)
  def per[value2](optional: Optional[value2])(lambda: (value, value2) => value): value =
    optional.lay(value)(lambda(value, _))

transparent inline def invite[entity]: Optional[entity] = summonFrom:
  case value: `entity` => value
  case _               => Unset

extension [value](iterable: Iterable[Optional[value]])
  transparent inline def compact: Iterable[value] =
    iterable.filter(!_.absent).map(_.vouch)

extension [value](option: Option[value])
  // Not `inline`: inlining a union `Optional[value]` result re-infers it per call site, where capture
  // checking stamps a spurious `^` when `value` is (or contains) a pure type such as `Text`.
  def optional: Optional[value] = option.getOrElse(Unset)

extension [value](value: value)
  def puncture(point: value): Optional[value] = if value == point then Unset else value

  // The partial function may capture a capability (e.g. a `case x /: y =>` whose extractor raises an
  // error captures the ambient `Tactic`), so it is accepted as a capturing value (`^`).
  def only[value2](partial: (PartialFunction[value, value2])^): Optional[value2] =
    if partial.isDefinedAt(value) then partial(value) else Unset

  def unless(predicate: (value: value) => Boolean): Optional[value] =
    if predicate(value) then Unset else value

  def unless(predicate: Boolean): Optional[value] = if predicate then Unset else value

extension [value](java: ju.Optional[value])
  def optional: Optional[value] = if java.isEmpty then Unset else java.get.nn

extension [value](optional: Optional[value])(using Optionality[optional.type])
  inline def absent: Boolean = optional == Unset
  inline def present: Boolean = optional != Unset

  inline def vouch: value = optional.or(panic(m"a value was vouched but was absent"))

  inline def mask(predicate: value => Boolean): Optional[value] =
    optional.let: value => if predicate(value) then Unset else value

  def javaOptional: ju.Optional[value] =
    optional.lay(ju.Optional.empty[value].nn)(ju.Optional.of(_).nn)

  def presume(using default: Default[value]): value = optional.or(default())
  def option: Option[value] = if absent then None else Some(vouch)
  def assume(using absentValue: CanThrow[UnsetError]): value = optional.or(throw UnsetError())

  inline def lay[value2](inline alternative: => value2)(inline lambda: value => value2): value2 =

    if absent then alternative else lambda(vouch)


  inline def layGiven[value2](inline alternative: => value2)(inline block: value ?=> value2)
  :   value2 =

    if absent then alternative else block(using vouch)

  def let[value2](lambda: value => value2): Optional[value2] =
    if absent then Unset else lambda(vouch)

  inline def letGiven[value2](inline block: value ?=> value2): Optional[value2] =
    if absent then Unset else block(using vouch)
