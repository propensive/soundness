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
package polyvinyl

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*

object Intensional:
  // An instance whose reading can fail: the specification's macro supplies the `Tactic` for
  // its `Error` when the field is read, so a record's field has the type `Result raises Error`,
  // and a named tuple's element the type `Result`, its `Tactic` found where the tuple is built.
  // Fallibility is declared here, rather than in an `Intensional`'s `Result`, so that no
  // instance need name a context function type: under capture checking, a `raises` result
  // cannot implement an abstract method.
  trait Fallible extends Typeclass, Resultant, Original, Formal:
    type Self <: Label
    type Error <: Hazard

    def transform(data: Origin, params: List[Text])(using Tactic[Error]): Result

  // An instance which ignores its parameters. The instance retains the accessor, so it captures
  // whatever the accessor does: an instance over a pure function is itself pure.
  def apply[name <: Label, form, origin, value](accessor: origin => value)
  :   (name is Intensional in form from origin to value)^{accessor} =

    new Intensional:
      type Self = name
      type Origin = origin
      type Form = form
      type Result = value

      def transform(data: origin, params: List[Text]): value = accessor(data)

  // An instance whose reading depends on the member's parameters, such as a bound or a pattern.
  def parametric[name <: Label, form, origin, value](accessor: (origin, List[Text]) => value)
  :   (name is Intensional in form from origin to value)^{accessor} =

    new Intensional:
      type Self = name
      type Origin = origin
      type Form = form
      type Result = value

      def transform(data: origin, params: List[Text]): value = accessor(data, params)

trait Intensional extends Typeclass, Resultant, Original, Formal:
  type Self <: Label

  def transform(data: Origin, params: List[Text]): Result
