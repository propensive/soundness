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
package proscenium

import scala.caps
import scala.reflect.ClassTag

// A `Buffer` is a fixed-size mutable array whose access rights are tracked by separation
// checking: any reference can read, but only an exclusive (`^`) reference can write, aliased
// writers are rejected, and `freeze` consumes the buffer to yield an immutable `IArray`
// without copying -- sound because `consume` statically retires every writer. An opaque
// alias rather than a wrapper class: the mutalias compiler patch classifies the alias as
// mutable wherever it appears, so hiding `Array` costs nothing at runtime.
//
// The total, `Optional`-returning read (`at`) lives in `vacuous`, which sits above this
// module; it is built from `length` and the bounds-partial `readUnchecked` below.
object Buffer:
  def apply[element: ClassTag](size: Int): Buffer[element]^ = new Array[element](size)

  // A bare JVM array, allocated through the companion so that interior scratch (the
  // fields of `caps.Mutable` state machines, and other code below the reach of `Buffer`'s
  // exclusivity discipline) shares this vocabulary without spelling `new`. Named
  // `scratch`, not `raw`, because `raw` is the exclusive-reference interop view below.
  inline def scratch[element: ClassTag](size: Int): scala.Array[element]^ =
    new scala.Array[element](size)

  def freeze[element](consume buffer: Buffer[element]^): IArray[element] =
    buffer.asInstanceOf[IArray[element]]

  // Linear growth for accumulating builders: consumes the old buffer, so the idiom is
  // recursion threading the buffer through `consume` parameters -- a `var` cannot hold an
  // exclusive buffer.
  def grow[element: ClassTag](consume buffer: Buffer[element]^, size: Int): Buffer[element]^ =
    val count = buffer.length.min(size)
    val bigger: Array[element]^ = new Array[element](size)
    java.lang.System.arraycopy(buffer, 0, bigger, 0, count)
    bigger

  extension [element, C^](buffer: Buffer[element]^{C})
    def length: Int = buffer.length

    // The bounds-partial shared read: exists for layering (`vacuous`'s total `at` wraps
    // it in a bounds check), not for use at call sites, which should prefer `at` or an
    // exclusive reference's `apply`. Reading through a shared reference is sound because
    // separation excludes live writers wherever readers alias.
    def readUnchecked(index: Int): element = buffer(index)

  extension [element](buffer: Buffer[element]^)
    // An exclusive reference has sole ownership, so it may also read without the
    // `Optional` guard: nobody else can have resized or replaced the content.
    def apply(index: Int): element = buffer(index)

    def update(index: Int, value: element): Unit = buffer(index) = value

    def fill(value: element): Unit =
      var index = 0

      while index < buffer.length do
        buffer(index) = value
        index += 1

    def copyFrom(source: IArray[element], sourceStart: Int, targetStart: Int, count: Int)
    :   Unit =
      java.lang.System.arraycopy(source, sourceStart, buffer, targetStart, count)

    def copyFromBuffer
      ( source: Buffer[element]^{caps.any.rd}, sourceStart: Int, targetStart: Int, count: Int )
    :   Unit =
      java.lang.System.arraycopy(source, sourceStart, buffer, targetStart, count)

    // The underlying array, exclusively: the escape for JDK interop (`random.nextBytes`,
    // `stream.read`, `System.arraycopy` from external sources). The result aliases the
    // buffer, so it shares the buffer's exclusivity rather than escaping it.
    def raw: Array[element]^ = buffer

opaque type Buffer[element] = Array[element]
