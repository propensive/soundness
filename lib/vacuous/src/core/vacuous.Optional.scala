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
┃    Soundness, version 0.39.0.                                                                    ┃
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

import language.experimental.pureFunctions

import java.util as ju

import scala.quoted.*

import anticipation.*
import fulminate.*

import errorDiagnostics.stackTraces

object Unset:
  override def toString(): String = "∅"

type Optional[value] = Unset.type | value

extension [value](inline optional: Optional[value])
  inline def or(inline value: => value): value = ${Vacuous.optimizeOr('optional, 'value)}

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
  : value2 =

      if absent then alternative else block(using vouch)


  def let[value2](lambda: value => value2): Optional[value2] =
    if absent then Unset else lambda(vouch)

  inline def letGiven[value2](inline block: value ?=> value2): Optional[value2] =
    if absent then Unset else block(using vouch)

object Optional:
  inline def apply[value](value: value | Null): Optional[value] =
    if value == null then Unset else value

  inline def check[value]: Unit = ${Vacuous.check[value]}

object Optionality:
  inline given default: [value] => Optionality[value] = ${Vacuous.check[value]}

erased trait Optionality[-value]
