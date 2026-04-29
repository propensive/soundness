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
package guillotine

import language.experimental.pureFunctions

import anticipation.*
import contextual.*
import fulminate.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*

import errorDiagnostics.empty

object Sh:
  enum Context:
    case Awaiting, Unquoted, Quotes2, Quotes1

  case class State(current: Context, escape: Boolean, arguments: List[Text])
  case class Parameters(params: Text*)

  object Prefix extends Interpolator[Parameters, State, Command]:
    import Context.*

    def complete(state: State): Command =
      val arguments = state.current match
        case Quotes2 =>
          throw InterpolationError(m"the double quotes have not been closed")

        case Quotes1 =>
          throw InterpolationError(m"the single quotes have not been closed")

        case _ if state.escape =>
          throw InterpolationError(m"cannot terminate with an escape character")

        case _ =>
          state.arguments

      Command(arguments*)

    def initial: State = State(Awaiting, false, Nil)
    def skip(state: State): State = insert(state, Parameters(t"x"))

    def insert(state: State, value: Parameters): State = value.params.to(List) match
      case head :: tail =>
        if state.escape
        then throw InterpolationError
          (  m"escaping with '\\' is not allowed immediately before a substitution" )

        state.absolve match
          case State(Awaiting, false, arguments) =>
            State(Unquoted, false, arguments ++ (head :: tail))

          case State(Unquoted, false, arguments :+ last) =>
            State(Unquoted, false, arguments ++ (t"$last$head" :: tail))

          case State(Quotes1, false, arguments :+ last) =>
            State(Quotes1, false, arguments :+ (t"$last$head" :: tail).join(t" "))

          case State(Quotes2, false, arguments :+ last) =>
            State(Quotes2, false, arguments :+ (t"$last$head" :: tail).join(t" "))

      case _ =>
        state

    def parse(current: State, text: Text): State = text.chars.to(List).fuse(current):
      (state, next).absolve match
        case (State(Awaiting, _, arguments), ' ') => State(Awaiting, false, arguments)

          case (State(Quotes1, false, more :+ current), '\\') =>
            State(Quotes1, false, more :+ t"$current\\")

          case (State(context, false, arguments), '\\') =>
            State(context, true, arguments)

          case (State(Unquoted, _, arguments), ' ') =>
            State(Awaiting, false, arguments)

          case (State(Quotes1, _, arguments), '\'') =>
            State(Unquoted, false, arguments)

          case (State(Quotes2, false, arguments), '"') =>
            State(Unquoted, false, arguments)

          case (State(Unquoted, false, arguments), '"') =>
            State(Quotes2, false, arguments)

          case (State(Unquoted, false, arguments), '\'') =>
            State(Quotes1, false, arguments)

          case (State(Awaiting, false, arguments), '"') =>
            State(Quotes2, false, arguments :+ t"")

          case (State(Awaiting, false, arguments), '\'') =>
            State(Quotes1, false, arguments :+ t"")

          case (State(Awaiting, _, arguments), char) =>
            State(Unquoted, false, arguments :+ t"$char")

          case (State(context, _, Nil), char) =>
            State(context, false, List(t"$char"))

          case (State(context, _, more :+ current), char) =>
            State(context, false, more :+ t"$current$char")

  given nothing: Insertion[Parameters, Nothing] = value => Parameters(t"")
  given text: Insertion[Parameters, Text] = value => Parameters(value)
  given list: Insertion[Parameters, List[Text]] = xs => Parameters(xs*)
  given command: Insertion[Parameters, Command] = command => Parameters(command.arguments*)


  given parameterizable: [parameterizable: Parameterizable]
  =>  Insertion[Parameters, parameterizable] =

    value => Parameters(parameterizable.show(value))
