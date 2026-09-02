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
package anticipation

object HttpStreams:
  // A minimal demand-aware pull protocol for HTTP message bodies: each call
  // to `next` produces at most `limit` bytes as one chunk, or `null` at the
  // end of the stream, and may block until input is available. Deliberately
  // dependency-free; richer streaming types (turbulence's `Source`,
  // zephyrine's `Stream`) adapt to and from it.
  object Body:
    val empty: Body = limit => null

    // A whole-value body, delivered in `limit`-bounded slices.
    def apply(data: Array[Byte]^{}): Body = new Body:
      @scala.caps.unsafe.untrackedCaptures
      private var position: Int = 0

      def next(limit: Int): Array[Byte]^{} | Null =
        if position >= data.length then null else
          val end = data.length.min(position + limit)
          val chunk = Array.frozen(data.readable.slice(position, end))
          position = end
          chunk

    // A chunked body from a legacy iterator; `limit` cannot bound the
    // chunks' own sizes.
    def apply(chunks: Iterator[Array[Byte]^{}]): Body = limit =>
      if chunks.hasNext then chunks.next() else null

    // A chunked body from a lazy chain of chunks, forced one cell per call;
    // as above, `limit` cannot bound the chunks' own sizes.
    def apply(chain: Chain[Array[Byte]^{}]): Body = new Body:
      @scala.caps.unsafe.untrackedCaptures
      private var remaining: Chain[Array[Byte]^{}] = chain

      def next(limit: Int): Array[Byte]^{} | Null = remaining match
        case chunk #:: tail =>
          remaining = tail
          chunk

        case _ =>
          null

  trait Body:
    def next(limit: Int): Array[Byte]^{} | Null

  type Content = (Text, Body)

sealed trait HttpStreams
