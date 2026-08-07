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
package facsimile

import proscenium.compat.*

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// A pipeline stage that gathers its whole input and transforms it during flush: the
// streaming fallback for the textual filters (ASCIIHex, ASCII85, RunLength, LZW,
// predictors), whose payloads are small in practice. The genuinely large payloads — raw
// ranges, terminal image codecs, Flate — never pass through it, streaming incrementally
// through their own stages instead.
private[facsimile] class Gathering(transform: Data => Data) extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private val gathered: scala.collection.mutable.ArrayBuffer[Byte] =
    scala.collection.mutable.ArrayBuffer()

  private var result: Optional[Data] = Unset
  private var delivered: Int = 0

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  update def step(source: Region[Data])(range: Interval in source.type)
    ( target: Slate[Data] )(space: Interval in target.type)
  :   Duct.Progress =

    source.visit(range) { index => gathered += source(index) }
    Duct.Progress((range: Interval).size, 0)

  override update def flush(target: Slate[Data])(space: Interval in target.type): Int =
    val data = result.or:
      val transformed = transform(Array.unsafeFrozen(gathered.toArray))
      result = transformed
      transformed

    val count = (space: Interval).size.min(data.length - delivered)
    var index = 0

    target.visit(space.capped(count)): ordinal =>
      target(ordinal) = data.readUnchecked(delivered + index)
      index += 1

    delivered += count
    count
