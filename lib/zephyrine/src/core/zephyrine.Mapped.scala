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
package zephyrine

import anticipation.*

// A read-only view of bytes held in memory that is not the heap -- a memory-mapped file, above
// all -- from which values are *loaded* in place rather than read into a fresh array: the
// zero-copy cousin of `Expanse`, whose every `read` allocates and copies. Formats designed for
// random access (an index, a table of fixed-width rows) are parsed directly out of a `Mapped`,
// so that several processes mapping one file share a single copy of it in the page cache.
//
// Integers are little-endian, whatever the platform, and may be loaded from any offset; no
// alignment is required. An offset or extent outside `0` to `size` is a programming error, and
// is met with an exception rather than an out-of-bounds load.
//
// A view is lent, not given: whoever provides one owns the memory behind it, and passes the
// view as a tracked `Mapped^` for a scope, at whose end the memory is unmapped. Capture
// checking keeps the view -- and every `slice` of it, which captures it -- from outliving that
// scope; code which receives one declares it as `Mapped^` likewise. Being read-only, a view
// may be shared between threads.
//
// The bytes are as stable as their source: a file which is replaced by renaming a new one over
// it (and never rewritten in place) presents the same bytes for as long as it stays mapped, and
// a provider need not defend against anything else.
trait Mapped:
  def size: Long

  def u8(offset: Long): Int
  def u16(offset: Long): Int
  def u32(offset: Long): Long
  def u64(offset: Long): Long

  // The 32-bit word at `offset` as a signed `Int`: `u32` without the widening.
  def i32(offset: Long): Int

  // A view of `length` bytes from `offset`, sharing this view's memory and its lifetime.
  def slice(offset: Long, length: Long): Mapped^{this}

  // Copies `length` bytes from `offset` onto the heap, for when they must become a value which
  // outlives the view -- a `Text`, say.
  def copy(offset: Long, length: Int): Data

  // Whether the bytes from `offset` are those of `data`, compared in place.
  def same(offset: Long, data: Data): Boolean
