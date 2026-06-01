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
package xenophile

import java.lang.foreign.*, ValueLayout.*

import anticipation.*
import prepositional.*

// The C / native ecosystem. Foreign values are represented directly by the Java Foreign Function
// and Memory API's `MemorySegment` (no intermediate encoding), and grammars are read from C header
// files by `CHeaderDialect`. Scalar `Interoperable`s box their value into a GC-managed segment via
// the matching `ValueLayout`, so no explicit `Arena` need be threaded through the API.
object Native:
  private def auto: Arena = Arena.ofAuto().nn

  private val ints: ValueLayout.OfInt = JAVA_INT.nn
  private val longs: ValueLayout.OfLong = JAVA_LONG.nn
  private val doubles: ValueLayout.OfDouble = JAVA_DOUBLE.nn
  private val floats: ValueLayout.OfFloat = JAVA_FLOAT.nn
  private val bools: ValueLayout.OfBoolean = JAVA_BOOLEAN.nn

  // Allocates a GC-managed segment for a single value and writes it with the given layout.
  private inline def boxed(layout: ValueLayout)(write: MemorySegment => Unit): MemorySegment =
    val segment = auto.allocate(layout).nn
    write(segment)
    segment

  given int: (Int is Interoperable in Native of "int" by MemorySegment) =
    Interoperable(n => boxed(ints)(_.set(ints, 0L, n)), _.get(ints, 0L))

  given long: (Long is Interoperable in Native of "long" by MemorySegment) =
    Interoperable(n => boxed(longs)(_.set(longs, 0L, n)), _.get(longs, 0L))

  given double: (Double is Interoperable in Native of "double" by MemorySegment) =
    Interoperable(x => boxed(doubles)(_.set(doubles, 0L, x)), _.get(doubles, 0L))

  given float: (Float is Interoperable in Native of "float" by MemorySegment) =
    Interoperable(x => boxed(floats)(_.set(floats, 0L, x)), _.get(floats, 0L))

  given boolean: (Boolean is Interoperable in Native of "bool" by MemorySegment) =
    Interoperable(b => boxed(bools)(_.set(bools, 0L, b)), _.get(bools, 0L))

  // A C string (`char*` / `const char*`) is an allocated, NUL-terminated UTF-8 segment.
  given string: (Text is Interoperable in Native of "string" by MemorySegment) =
    Interoperable(text => auto.allocateFrom(text.s).nn, _.getString(0L).nn.tt)

trait Native extends Ecosystem:
  type Operand = MemorySegment
  type Grammar = CHeaderDialect.type
