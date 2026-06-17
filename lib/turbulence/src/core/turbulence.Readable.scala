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
package turbulence

import scala.annotation.implicitNotFound

import anticipation.*
import hieroglyph.*
import prepositional.*

// `Readable` composes a `Streamable` for the source type and an `Aggregable`
// for the result type into a single resolvable instance, bridging with a
// `CharDecoder`/`CharEncoder` when the two operate on different operands
// (`Data` vs `Text`). Expressing the four pipelines as prioritised `given`s
// (rather than a macro that summons combinations) means a missing instance is
// an ordinary failed implicit search — so Frontier's `explainMissingContext`,
// when in scope, lists the candidates and what each still requires.
//
// Priority (highest first): same-operand pipelines before the bridged ones,
// so a directly-streamable/aggregable pairing is preferred when both apply.

trait Readable3:
  given textToData: [source, result]
  =>  ( streamable: source is Streamable by Text )
  =>  ( aggregable: result is Aggregable by Data )
  =>  ( encoder: CharEncoder )
  =>  source is Readable to result =
    value => aggregable.aggregate(encoder.encoded(streamable.stream(value)))

trait Readable2 extends Readable3:
  given dataToText: [source, result]
  =>  ( streamable: source is Streamable by Data )
  =>  ( aggregable: result is Aggregable by Text )
  =>  ( decoder: CharDecoder )
  =>  source is Readable to result =
    value => aggregable.aggregate(decoder.decoded(streamable.stream(value)))

trait Readable1 extends Readable2:
  given textToText: [source, result]
  =>  ( streamable: source is Streamable by Text )
  =>  ( aggregable: result is Aggregable by Text )
  =>  source is Readable to result =
    value => aggregable.aggregate(streamable.stream(value))

object Readable extends Readable1:
  given dataToData: [source, result]
  =>  ( streamable: source is Streamable by Data )
  =>  ( aggregable: result is Aggregable by Data )
  =>  source is Readable to result =
    value => aggregable.aggregate(streamable.stream(value))

@implicitNotFound("turbulence: the source cannot be read as the target type; this needs a "+
    "`Streamable` instance for the source and an `Aggregable` instance for the target, plus a "+
    "`CharDecoder` or `CharEncoder` if their operands (Data/Text) differ")
trait Readable extends Typeclass, Resultant:
  def read(value: Self): Result
