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
package turbulence

import scala.annotation.implicitNotFound

import anticipation.*
import hieroglyph.*
import prepositional.*
import zephyrine.*

// `Readable` composes a `Source` for the source type and an `Aggregable` for
// the result type into a single resolvable instance, bridging with a
// `CharDecoder`/`CharEncoder` duct when the two operate on different operands
// (`Data` vs `Text`) — so a bridged read transcodes incrementally, in bounded
// buffers, on the reading thread. Expressing the four pipelines as
// prioritised `given`s (rather than a macro that summons combinations) means
// a missing instance is an ordinary failed implicit search — so Frontier's
// `explainMissingContext`, when in scope, lists the candidates and what each
// still requires.
//
// Priority (highest first): same-operand pipelines before the bridged ones,
// so a directly-streamable/aggregable pairing is preferred when both apply.

trait Readable3:
  given textToData: [source, result]
  =>  ( source0: (source is Streamable by Text over Credit)^ )
  =>  ( aggregable: (result is Aggregable by Data)^ )
  =>  ( encoder: CharEncoder, buffering: Buffering )
  =>  ((source is Readable to result)^{source0, aggregable}) =
    value => aggregable.accept(source0.stream(value).via(encoder))

trait Readable2 extends Readable3:
  given dataToText: [source, result]
  =>  ( source0: (source is Streamable by Data over Credit)^ )
  =>  ( aggregable: (result is Aggregable by Text)^ )
  =>  ( decoder: CharDecoder, buffering: Buffering )
  =>  ((source is Readable to result)^{source0, aggregable}) =
    value => aggregable.accept(source0.stream(value).via(decoder))

trait Readable1 extends Readable2:
  given textToText: [source, result]
  =>  ( source0: (source is Streamable by Text over Credit)^ )
  =>  ( aggregable: (result is Aggregable by Text)^ )
  =>  ((source is Readable to result)^{source0, aggregable}) =
    value => aggregable.accept(source0.stream(value))

object Readable extends Readable1:
  // Direct, whole-`Data` instances: when the entire content is already in hand as one `Data`,
  // decode it in a single step rather than wrapping it in a one-element `Stream` and aggregating.
  // These are concrete (`Self = Data`), so they take precedence over the generic composed pipelines
  // by specificity — a whole-file `path.read[Text]` is then a genuinely direct read.
  given dataData: Data is Readable to Data = identity(_)

  given dataText: (decoder: CharDecoder) => ((Data is Readable to Text)) =
    decoder.decoded(_)

  given dataToData: [source, result]
  =>  ( source0: (source is Streamable by Data over Credit)^ )
  =>  ( aggregable: (result is Aggregable by Data)^ )
  =>  ((source is Readable to result)^{source0, aggregable}) =
    value => aggregable.accept(source0.stream(value))

@implicitNotFound("turbulence: the source cannot be read as the target type; this needs a "+
    "`Source` instance for the source and an `Aggregable` instance for the target, plus a "+
    "`CharDecoder` or `CharEncoder` if their operands (Data/Text) differ")
trait Readable extends Typeclass, Resultant:
  def read(value: Self): Result
