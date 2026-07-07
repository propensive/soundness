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
package xenophile

import java.lang.foreign.*
import java.lang.invoke.MethodHandle

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

// A loaded native library, paired with the C signatures `CHeaderDialect` read from
// a header. This is the runtime half of the `native` ecosystem — the Foreign /
// Memory (FFM) "evaluator" that turns a navigated foreign function into an actual
// downcall. The compile-time `Foreign` navigator type-checks calls against the same
// header; this executes them.
object ForeignLibrary:
  private def linker: Linker = Linker.nativeLinker.nn

  // Maps a C type (as canonicalised by `CHeaderDialect`) to an FFM memory layout.
  // Every pointer and string is an `ADDRESS`; the numeric primitives map to their
  // same-width FFM value layout. Opaque/unknown named types are assumed to be
  // passed by pointer (the `extern "C"` convention for handles like `EVP_PKEY*`).
  def layout(tpe: Foreign.Type): MemoryLayout = tpe match
    case Foreign.Type.Named(t"int")    => ValueLayout.JAVA_INT.nn
    case Foreign.Type.Named(t"long")   => ValueLayout.JAVA_LONG.nn
    case Foreign.Type.Named(t"short")  => ValueLayout.JAVA_SHORT.nn
    case Foreign.Type.Named(t"char")   => ValueLayout.JAVA_BYTE.nn
    case Foreign.Type.Named(t"double") => ValueLayout.JAVA_DOUBLE.nn
    case Foreign.Type.Named(t"float")  => ValueLayout.JAVA_FLOAT.nn
    case Foreign.Type.Named(t"bool")   => ValueLayout.JAVA_BOOLEAN.nn
    case _                             => ValueLayout.ADDRESS.nn

  def descriptor(signature: Prototype): FunctionDescriptor =
    val parameters = signature.parameters.or(Nil).map(layout)

    signature.result match
      case Foreign.Type.Named(t"void") => FunctionDescriptor.ofVoid(parameters*).nn
      case result                      => FunctionDescriptor.of(layout(result), parameters*).nn

  // Loads the first of `paths` that resolves as a symbol lookup bound to `arena`,
  // pairing it with the function signatures parsed from `header`.
  def apply(header: Text, paths: List[Text])(using arena: Arena): ForeignLibrary =
    def attempt(remaining: List[Text]): SymbolLookup = remaining match
      case path :: rest =>
        try SymbolLookup.libraryLookup(path.s, arena).nn catch case _: Throwable => attempt(rest)

      case Nil =>
        throw IllegalArgumentException(t"no native library could be loaded from $paths".s)

    val signatures = CHeaderDialect.parse(header).getOrElse(CHeaderDialect.library, Map())
    new ForeignLibrary(attempt(paths), signatures)

  // The process-wide default lookup (the C standard library and already-loaded
  // images); useful for `libc` symbols without naming a library file.
  def system(header: Text): ForeignLibrary =
    val signatures = CHeaderDialect.parse(header).getOrElse(CHeaderDialect.library, Map())
    new ForeignLibrary(linker.defaultLookup.nn, signatures)

  // Copies bytes into freshly-allocated native memory in `arena`.
  def segment(bytes: Data)(using arena: Arena): MemorySegment =
    val source = bytes.mutable(using Unsafe)
    val target = arena.allocate(bytes.length.toLong).nn
    MemorySegment.copy(source, 0, target, ValueLayout.JAVA_BYTE, 0L, bytes.length)
    target

  // Reads `length` bytes back out of a native segment.
  def bytes(segment: MemorySegment, length: Int): Data =
    val array = new Array[Byte](length)
    MemorySegment.copy(segment, ValueLayout.JAVA_BYTE, 0L, array, 0, length)
    array.immutable(using Unsafe)

class ForeignLibrary(lookup: SymbolLookup, signatures: Map[Text, Prototype]):
  // A bound `MethodHandle` for the named function, built from its parsed C
  // signature. Invoke it with `invokeWithArguments`, passing `MemorySegment`s for
  // pointer parameters and boxed primitives for the rest.
  def handle(function: Text): MethodHandle =
    val signature = signatures.getOrElse(function, panic(m"no such foreign function: $function"))
    val symbol = lookup.find(function.s).nn.orElseThrow().nn
    ForeignLibrary.linker.downcallHandle(symbol, ForeignLibrary.descriptor(signature)).nn
