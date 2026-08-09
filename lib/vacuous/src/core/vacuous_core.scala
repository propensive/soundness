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
package vacuous

import java.util as ju

import scala.compiletime.*

import anticipation.*
import fulminate.*

import errorDiagnostics.stackTracesDiagnostics

inline def default[value](using default: Default[value]): value = default()

inline def optimizable[value](lambda: Optional[value] => Optional[value]): Optional[value] =
  lambda(Unset)

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
    iterable.filter(!_.absent).map(_.or(panic(m"the absent elements were filtered out")))

  // All-or-nothing: the whole list if every element is present, or `Unset` if any is
  // absent. One presence sweep, recorded by the result's type.
  def entire: Optional[List[value]] =
    if iterable.exists(_.absent) then Unset
    else List.from(iterable.map(_.or(panic(m"absence was excluded by the sweep above"))))

// As `compact` for a streaming source: absent elements are dropped as the iterator advances.
extension [value](iterator: Iterator[Optional[value]])
  transparent inline def compact: Iterator[value] =
    iterator.filter(!_.absent).map(_.or(panic(m"the absent elements were filtered out")))

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

  // The foundation extraction, private to these combinators: after an `absent` test, the
  // union's only remaining inhabitant is a `value`, so the cast is a no-op at runtime. Every
  // combinator below discharges absence through this one point; user code never needs it,
  // since `or`/`let`/`lay` express every honest consumption.
  private inline def unsafeGet: value = optional.asInstanceOf[value]


  inline def mask(predicate: value => Boolean): Optional[value] =
    optional.let: value => if predicate(value) then Unset else value

  def javaOptional: ju.Optional[value] =
    optional.lay(ju.Optional.empty[value].nn)(ju.Optional.of(_).nn)

  def presume(using default: Default[value]): value = optional.or(default())
  def option: Option[value] = if absent then None else Some(unsafeGet)
  def assume(using absentValue: CanThrow[UnsetError]): value = optional.or(throw UnsetError())

  inline def lay[value2](inline alternative: => value2)(inline lambda: value => value2): value2 =

    if absent then alternative else lambda(unsafeGet)


  inline def layGiven[value2](inline alternative: => value2)(inline block: value ?=> value2)
  :   value2 =

    if absent then alternative else block(using unsafeGet)

  def let[value2](lambda: value => value2): Optional[value2] =
    if absent then Unset else lambda(unsafeGet)

  inline def letGiven[value2](inline block: value ?=> value2): Optional[value2] =
    if absent then Unset else block(using unsafeGet)
