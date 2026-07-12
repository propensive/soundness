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
package zephyrine

import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*

// The push endpoint of a streaming pipeline: a mutable buffer whose writable
// window is exposed zero-copy to exactly one producer, and which reports its
// current `demand` — the reactive message telling that producer how much it
// can accept. An `Intake` is transformed into a differently-typed `Intake`
// with `accepting`. `Intake` extends `Producer`, so producing code written
// against `put`/`push` (serializers) drives a pipeline unchanged.
//
// The primitive protocol is `reserve`/`buffer`/`mark`/`commit`: a writer
// reserves contiguous space, writes directly into the intake's storage at
// `mark`, then commits. `put`, `push` and `absorb` are final loops over that
// protocol, so implementations provide only the window and its lifecycle.
// The intake inherits Producer's capability classification: writes require an exclusive
// reference; `demand` and `mark` are read-only queries.
trait Intake[medium](using val addressable: medium is Addressable) extends Producer[medium]:
  type Operand = addressable.Operand
  type Transport

  // The current demand this intake reports to whoever writes to it. Computed
  // on request; never cached by callers. Advisory for bounding upstream
  // production — `reserve` is what actually blocks.
  def demand: Transport

  // Ensure contiguous writable space, blocking if necessary, and return the
  // available space, always at least one element. `min` is a request: an
  // implementation may return less than was asked for (writers loop over
  // `reserve`/`commit`, so a short reservation only means another iteration),
  // but may also use a large request as a sizing hint — a `Conduit` mints a
  // correspondingly larger transfer block, so bulk writes cross the
  // asynchronous boundary in fewer synchronized hand-offs.
  update def reserve(min: Int): Int

  // Zero-copy view of this intake's buffer; elements from `mark` are writable
  // up to the last `reserve`d space. Valid only until the next `commit`,
  // `flush` or `finish` — single-owner discipline, as `Stream.window`.
  // Implementations provide the untyped `buffer0`; since `Addressable`
  // instances are unique per medium, the cast in `buffer` is sound.
  final def buffer(using Unsafe): addressable.Storage =
    buffer0.asInstanceOf[addressable.Storage]

  protected def buffer0: AnyRef
  def mark: Int

  // Declare `count` elements written at `mark`; may synchronously cascade
  // them through downstream stages.
  update def commit(count: Int): Unit

  // Propagate buffered data downstream now, without ending the stream.
  update def flush(): Unit = ()

  // End-of-stream: flush, emit any terminal stage state, release downstream.
  // Called exactly once.
  update def finish(): Unit

  // Abort: propagate failure downstream so stages can release resources. The
  // error is rethrown to the reader at an asynchronous boundary.
  update def fail(error: Throwable): Unit = finish()

  final update def absorb(source: addressable.Storage, offset: Int, count: Int): Unit =
    var done: Int = 0

    while done < count do
      val free = reserve(count - done)
      val size = free.min(count - done)
      addressable.transfer(source, offset + done, buffer(using Unsafe), mark, size)
      commit(size)
      done += size

  final update def put(source: medium): Unit = put(source, Prim, addressable.length(source))

  // Overridable: the default loops the reserve/commit protocol, copying, but an
  // asynchronous boundary may transfer a large immutable chunk by reference
  // (via `Addressable.backing`) instead — see `Conduit`.
  update def put(source: medium, offset: Ordinal, size: Int): Unit =
    var done: Int = 0

    while done < size do
      val free = reserve(size - done)
      val count = free.min(size - done)
      addressable.copyChunk(source, offset.n0 + done, buffer(using Unsafe), mark, count)
      commit(count)
      done += count

  final update def push(operand: Operand): Unit =
    reserve(1)
    addressable.storageUpdate(buffer(using Unsafe), mark, operand)
    commit(1)
