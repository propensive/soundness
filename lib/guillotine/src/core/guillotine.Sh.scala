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
package guillotine

import scala.language.experimental.pureFunctions

import anticipation.*
import contextual.*
import fulminate.*
import gossamer.*
import scala.collection.immutable as sci
import scala.collection.immutable.::
import scala.collection.{`:+`, `+:`}

import proscenium.compat.*

import rudiments.*
import vacuous.*
import spectacular.*

object Sh:
  enum Context:
    case Awaiting, Unquoted, Quotes2, Quotes1

  // `offset` counts characters consumed so far in "value space", where each substitution
  // occupies exactly one position (matching `Runtime.skip`); `quoteStart` records the offset of
  // the opening quote of the current `Quotes1`/`Quotes2` context, so that errors over
  // unterminated quotes can be positioned at the quote that was never closed.
  case class State
    ( current: Context,
      escape: Boolean,
      arguments: sci.List[Text],
      offset: Int = 0,
      quoteStart: Int = -1 )

  case class Parameters(params: Text*)

  case class ShError(detail: Message, offset: Int)
  extends Exception(s"guillotine: ${detail.text.s}")

  object Runtime:
    import unsafeExceptions.canThrowAny
    import Context.*

    def complete(state: State): Command =
      val arguments = state.current match
        case Quotes2 =>
          throw ShError(m"this double quote is never closed", state.quoteStart)

        case Quotes1 =>
          throw ShError(m"this single quote is never closed", state.quoteStart)

        case _ if state.escape =>
          throw ShError(m"a command cannot end with an escape character", state.offset - 1)

        case _ =>
          state.arguments

      Command(arguments*)

    def initial: State = State(Awaiting, false, Nil.stdlib)
    def skip(state: State): State = insert(state, Parameters(t"x"))

    def insert(state: State, value: Parameters): State = value.params.toList match
      case head :: tail =>
        if state.escape then
          throw ShError
            ( m"an escape character cannot appear immediately before a substitution",
              state.offset - 1 )

        val state2 = state.absolve match
          case State(Awaiting, false, arguments, _, _) =>
            State(Unquoted, false, arguments ++ ((head :: tail): sci.List[Text]))

          case State(Unquoted, false, arguments :+ last, _, _) =>
            State(Unquoted, false, arguments ++ ((t"$last$head" :: tail): sci.List[Text]))

          case State(Quotes1, false, arguments :+ last, _, _) =>
            State(Quotes1, false, arguments :+ ((t"$last$head" :: tail): sci.List[Text]).join(t" "))

          case State(Quotes2, false, arguments :+ last, _, _) =>
            State(Quotes2, false, arguments :+ ((t"$last$head" :: tail): sci.List[Text]).join(t" "))

        state2.copy(offset = state.offset + 1, quoteStart = state.quoteStart)

      case _ =>
        state.copy(offset = state.offset + 1)

    private def chars(text: Text): scala.Seq[Char] = text.chars.toSeq

    def parse(current: State, text: Text): State = chars(text).fuse(current):
      val step = (state, next).absolve match
        case (State(Awaiting, _, arguments, _, _), ' ') =>
          State(Awaiting, false, arguments)

        case (State(Quotes1, false, more :+ current, _, _), '\\') =>
          State(Quotes1, false, more :+ t"$current\\")

        case (State(context, false, arguments, _, _), '\\') =>
          State(context, true, arguments)

        case (State(Unquoted, _, arguments, _, _), ' ') =>
          State(Awaiting, false, arguments)

        case (State(Quotes1, _, arguments, _, _), '\'') =>
          State(Unquoted, false, arguments)

        case (State(Quotes2, false, arguments, _, _), '"') =>
          State(Unquoted, false, arguments)

        case (State(Unquoted, false, arguments, _, _), '"') =>
          State(Quotes2, false, arguments)

        case (State(Unquoted, false, arguments, _, _), '\'') =>
          State(Quotes1, false, arguments)

        case (State(Awaiting, false, arguments, _, _), '"') =>
          State(Quotes2, false, arguments :+ t"")

        case (State(Awaiting, false, arguments, _, _), '\'') =>
          State(Quotes1, false, arguments :+ t"")

        case (State(Awaiting, _, arguments, _, _), char) =>
          State(Unquoted, false, arguments :+ t"$char")

        case (State(context, _, sci.Nil, _, _), char) =>
          State(context, false, sci.List(t"$char"))

        case (State(context, _, more :+ current, _, _), char) =>
          State(context, false, more :+ t"$current$char")

      val entering =
        (step.current == Quotes1 || step.current == Quotes2) && step.current != state.current

      step.copy
        ( offset = state.offset + 1,
          quoteStart = if entering then state.offset else state.quoteStart )

  given nothing: Insertion[Parameters, Nothing] = value => Parameters(t"")
  given text: Insertion[Parameters, Text] = value => Parameters(value)
  given list: Insertion[Parameters, List[Text]] = xs => Parameters(xs*)
  given command: Insertion[Parameters, Command] = command => Parameters(command.arguments*)

  given parameterizable: [parameterizable: Parameterizable]
  =>  Insertion[Parameters, parameterizable] =

    value => Parameters(parameterizable.show(value))
