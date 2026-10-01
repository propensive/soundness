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
package turbulence

import scala.compiletime
import scala.language.experimental.captureChecking

import anticipation.*
import hieroglyph.*
import prepositional.*
import zephyrine.{stream as _, *}

object Loadable:
  // The dispatch behind `load`, one method per stream operand: the instance of the same
  // operand is preferred, and the other is reached through the charset bridge, with the
  // `Charset`/`Codepage` and `Buffering` resolved only on that path.
  inline def fromData[result <: Documentary](consume stream: (Stream[Data] over Credit)^)
  :   Document[result] =

    compiletime.summonFrom:
      case loadable: ((`result` is Loadable by Data)^) => loadable.load(stream)

      case loadable: ((`result` is Loadable by Text)^) =>
        given Buffering = compiletime.summonInline[Buffering]
        loadable.load(stream.via(compiletime.summonInline[Charset]))

      case _ =>
        compiletime.error("turbulence: the result type has no `Loadable` instance")

  // A whole-value source's bytes as a one-chunk stream, crossing to the consuming loader as a
  // neutral reference (the `accept` convention).
  def whole[source](source: source)(using readable: (source is Readable to Data)^)
  :   (Stream[Data] over Credit)^ =

    Stream(readable.read(source)).asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^]

  inline def fromText[result <: Documentary](consume stream: (Stream[Text] over Credit)^)
  :   Document[result] =

    compiletime.summonFrom:
      case loadable: ((`result` is Loadable by Text)^) => loadable.load(stream)

      case loadable: ((`result` is Loadable by Data)^) =>
        given Buffering = compiletime.summonInline[Buffering]
        loadable.load(stream.via(compiletime.summonInline[Codepage]))

      case _ =>
        compiletime.error("turbulence: the result type has no `Loadable` instance")

trait Loadable extends Typeclass:
  type Self <: Documentary
  type Operand

  def load(stream: (Stream[Operand] over Credit)^): Document[Self]
