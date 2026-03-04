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
package vacuous

import java.util as ju

import scala.compiletime.*

import anticipation.*
import fulminate.*
import proscenium.*

import errorDiagnostics.stackTraces

inline def default[value]: value = infer[Default[value]]()

inline def optimizable[value](lambda: Optional[value] => Optional[value]): Optional[value] =
  lambda(Unset)

erased val Unsafe: Unsafe = caps.unsafe.unsafeErasedValue

type Optional[value] = Unset.type | value

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
  inline def optional: Unset.type | value = option.getOrElse(Unset)

extension [value](value: value)
  def puncture(point: value): Optional[value] = if value == point then Unset else value

  def only[value2](partial: PartialFunction[value, value2]): Optional[value2] =
    (partial.orElse { _ => Unset })(value)

  def unless(predicate: (value: value) => Boolean) = if predicate(value) then Unset else value

extension [value](java: ju.Optional[value])
  def optional: Optional[value] = if java.isEmpty then Unset else java.get.nn

private given realm: Realm = realm"vacuous"

extension [value](optional: Optional[value])(using Optionality[optional.type])
  inline def absent: Boolean = optional == Unset
  inline def present: Boolean = optional != Unset

  inline def vouch: value = optional.or(panic(m"a value was vouched but was absent"))

  inline def mask(predicate: value => Boolean): Optional[value] =
    optional.let { value => if predicate(value) then Unset else value }

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
