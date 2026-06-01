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
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

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

  private val address: AddressLayout = ADDRESS.nn

  // An evaluator that performs real FFM downcalls against the platform's default (libc) symbol
  // lookup. Each function's call descriptor is built from its signature, read by re-parsing
  // `header`. Function application with scalar and pointer arguments and scalar results is
  // supported; struct field reads are not yet evaluated at runtime.
  def evaluator(header: Text): Evaluator in Native by MemorySegment =
    val functions =
      CHeaderDialect.parse(header).at(CHeaderDialect.library).or(Map[Text, Signature]())

    val linker = Linker.nativeLinker().nn
    val lookup = linker.defaultLookup().nn

    def layout(foreign: ForeignType): MemoryLayout = foreign match
      case ForeignType.Named(name) =>
        if name == t"int" then ints
        else if name == t"long" || name == t"size_t" then longs
        else if name == t"double" then doubles
        else if name == t"float" then floats
        else if name == t"bool" then bools
        else address

      case _ =>
        address

    def argument(foreign: ForeignType, segment: MemorySegment): Any = foreign match
      case ForeignType.Named(name) =>
        if name == t"int" then segment.get(ints, 0L)
        else if name == t"long" || name == t"size_t" then segment.get(longs, 0L)
        else if name == t"double" then segment.get(doubles, 0L)
        else if name == t"float" then segment.get(floats, 0L)
        else if name == t"bool" then segment.get(bools, 0L)
        else segment

      case _ =>
        segment

    def result(foreign: ForeignType, value: Any): MemorySegment = foreign match
      case ForeignType.Named(name) =>
        if name == t"int" then boxed(ints)(_.set(ints, 0L, value.asInstanceOf[Int]))
        else if name == t"float" then boxed(floats)(_.set(floats, 0L, value.asInstanceOf[Float]))
        else if name == t"bool" then boxed(bools)(_.set(bools, 0L, value.asInstanceOf[Boolean]))
        else if name == t"long" || name == t"size_t"
        then boxed(longs)(_.set(longs, 0L, value.asInstanceOf[Long]))
        else if name == t"double"
        then boxed(doubles)(_.set(doubles, 0L, value.asInstanceOf[Double]))
        else value.asInstanceOf[MemorySegment]

      case _ =>
        value.asInstanceOf[MemorySegment]

    new Evaluator:
      type Form = Native
      type Operand = MemorySegment

      def evaluate(expr: ForeignExpr): MemorySegment = expr match
        case ForeignExpr.Literal(value) =>
          value.asInstanceOf[MemorySegment]

        case ForeignExpr.Apply(ForeignExpr.Select(_, function), arguments) =>
          val signature = functions.at(function).or:
            throw RuntimeException(t"xenophile: unknown native function $function".s)

          val parameters = signature.parameters.or(Nil)
          val layouts = parameters.map(layout)

          val descriptor = signature.result match
            case ForeignType.Named(name) if name == t"void" =>
              FunctionDescriptor.ofVoid(layouts*).nn

            case other =>
              FunctionDescriptor.of(layout(other), layouts*).nn

          val symbol = lookup.find(function.s).nn.orElseThrow().nn
          val handle = linker.downcallHandle(symbol, descriptor).nn

          val values = parameters.zip(arguments).map: (kind, expr) =>
            argument(kind, evaluate(expr))

          result(signature.result, handle.invokeWithArguments(values*))

        case _ =>
          throw RuntimeException(t"xenophile: this native expression cannot be evaluated".s)

trait Native extends Ecosystem:
  type Operand = MemorySegment
  type Grammar = CHeaderDialect.type
