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
package zephyrine

import scala.annotation.targetName

import anticipation.Data
import anticipation.Text
import contingency.*
import fulminate.Diagnostics
import vacuous.Unsafe

// Safe, allocation-free single-element peek. Returns `Datum.End` when the
// cursor is finished; otherwise the current byte (unsigned, `0..255`) or
// char wrapped as a `Datum`. The `Datum` opaque type (backed by `Int`)
// is deliberately distinct from raw `Int` so that arithmetic or
// `Byte`/`Char` confusion can't happen silently — comparison with the
// expected literal is via the dedicated `Datum.==` overloads, which still
// compile to a single `int == int`. Two extensions disambiguated by
// `@targetName` (they erase to the same JVM signature) — callers just
// write `cursor.peek` without caring whether the cursor is byte- or
// char-based.
extension (cursor: Cursor[Data])
  @targetName("peekByte")
  inline def peek: Datum =
    if cursor.finished then Datum.End
    else Datum.fromRaw(cursor.buffer(using Unsafe)(cursor.unsafePos(using Unsafe)) & 0xff)

extension (cursor: Cursor[Text])
  @targetName("peekChar")
  inline def peek: Datum =
    if cursor.finished then Datum.End
    else Datum.fromRaw(cursor.buffer(using Unsafe)(cursor.unsafePos(using Unsafe)).toInt)

// Match the cursor's current operand against `target`. On a match, advance
// past it; on a mismatch (or EOF), raise `failure` via the ambient
// `Tactic`. Replaces the hand-rolled `if cursor.peek != X then raise(…);
// cursor.next()` pair that every header / framer parser was writing. The
// target is `Char` for both variants so callers don't need `'X'.toByte`
// on `Cursor[Data]`; for ASCII targets the `Datum`-vs-`Char` comparison
// compiles to a single primitive `int == int`.
extension (cursor: Cursor[Data])
  @targetName("expectByte")
  inline def expect[error <: Exception](target: Char)
    ( inline failure: Diagnostics ?=> error )
    ( using Tactic[error] )
  :   Unit =

    if cursor.peek == target then cursor.next() else raise(failure)

extension (cursor: Cursor[Text])
  @targetName("expectChar")
  inline def expect[error <: Exception](target: Char)
    ( inline failure: Diagnostics ?=> error )
    ( using Tactic[error] )
  :   Unit =

    if cursor.peek == target then cursor.next() else raise(failure)

// Run `action` inside a hold and always restore the cursor position to
// where it was on entry, regardless of the result — i.e. a non-consuming
// lookahead. Marks taken inside the action are available via the implicit
// `Cursor.Held`; the outer cursor is `cue`d back on exit so subsequent
// processing sees the same bytes. Replaces the explicit
// `hold { val mk = mark; … cue(mk); result }` idiom that parsers like
// Multipart's boundary detector and Zeppelin's ZIP signature scanner
// were writing.
extension [data](cursor: Cursor[data])
  inline def lookahead[result](inline action: Cursor.Held ?=> result): result =
    cursor.hold:
      val saved = cursor.mark
      val outcome: result = action
      cursor.cue(saved)
      outcome

// Typed, allocation-free view of the cursor's current backing storage.
// Returns the same array as `unsafeBuffer` but with its concrete element
// type, so parsers that snapshot the buffer for a hot-loop scan don't have
// to write `cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]`
// themselves — the unsafe cast lives in one place inside zephyrine.
// `Unsafe` is still required: the returned reference is only valid until
// the next cursor operation that may compact or grow the buffer.
extension (cursor: Cursor[Data])
  @targetName("dataBuffer")
  inline def buffer(using erased Unsafe): Array[Byte] =
    cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]]

extension (cursor: Cursor[Text])
  @targetName("textBuffer")
  inline def buffer(using erased Unsafe): Array[Char] =
    cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Char]]

package lineation:
  inline given linefeedChars: Lineation:
    type Operand = Char

    inline def active: Boolean = true
    inline def track(datum: Char): Boolean = datum == '\n'

  inline given carriageReturnChar: Lineation:
    type Operand = Char

    inline def active: Boolean = true
    inline def track(datum: Char): Boolean = datum == '\r'

  inline given linefeedByte: Lineation:
    type Operand = Byte

    inline def active: Boolean = true
    inline def track(datum: Byte): Boolean = datum == 10

  inline given carriageReturnByte: Lineation:
    type Operand = Byte

    inline def active: Boolean = true
    inline def track(datum: Byte): Boolean = datum == 13

export Cursor.{Mark, Offset}
