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
package hypotenuse

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import hellenism.*, classloaders.threadContextClassloader
import hypotenuse.arithmeticOptions.uncheckedDivision, hypotenuse.arithmeticOptions.uncheckedOverflow
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import superlunary.embeddings.automaticEmbedding
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

// Whether an operator on a fixed-width type costs more than the primitive it wraps. Each
// row reduces the same array with the same operator, once through the opaque type's
// typeclass instance and once on the primitive; the instances are objects which the inliner
// drops, so the two should be indistinguishable.
enum Engine:
  case Hypotenuse, Primitive

object Benchmarks extends Suite("arithmetic-bench", m"Hypotenuse arithmetic benchmarks"):
  given device: BenchmarkDevice = LocalhostDevice

  val size: Int = 4096

  // Deterministic, mostly small values, so that products do not overflow the checked rows.
  val ints: Array[Int]^{} = Array.tabulate(size) { index => (index*7919)%1000 + 1 }
  val longs: Array[Long]^{} = Array.tabulate(size) { index => ((index*7919L)%1000) + 1 }

  val doubles: Array[Double]^{} =
    Array.tabulate(size) { index => ((index*7919)%1000).toDouble/8.0 + 1.0 }

  // The reductions are kept in named methods (not written in the staged bodies) so that
  // `run()` can check that each pair agrees before it is timed.
  def sumS32(): Int =
    var total: S32 = S32(0.bits)
    var index = 0

    while index < size do
      total = total + S32(ints(index).bits)
      index += 1

    total.int

  def sumInt(): Int =
    var total = 0
    var index = 0

    while index < size do
      total = total + ints(index)
      index += 1

    total

  def productS32(): Int =
    var total: S32 = S32(1.bits)
    var index = 0

    while index < size do
      total = total*S32(ints(index).bits)
      index += 1

    total.int

  def productInt(): Int =
    var total = 1
    var index = 0

    while index < size do
      total = total*ints(index)
      index += 1

    total

  def sumU64(): U64 =
    var total: U64 = U64(0L.bits)
    var index = 0

    while index < size do
      total = total + U64(longs(index).bits)
      index += 1

    total

  def sumLong(): Long =
    var total = 0L
    var index = 0

    while index < size do
      total = total + longs(index)
      index += 1

    total

  def quotientS64(): Long =
    var total: S64 = S64(Long.MaxValue.bits)
    var index = 0

    while index < size do
      total = total/S64(longs(index).bits) + S64(longs(index).bits)
      index += 1

    total.long

  def quotientLong(): Long =
    var total = Long.MaxValue
    var index = 0

    while index < size do
      total = total/longs(index) + longs(index)
      index += 1

    total

  def sumF64(): Double =
    var total: F64 = F64(0.0)
    var index = 0

    while index < size do
      total = total + F64(doubles(index))
      index += 1

    total.double

  def sumDouble(): Double =
    var total = 0.0
    var index = 0

    while index < size do
      total = total + doubles(index)
      index += 1

    total

  // A `Double` operand, converted by the `Widening` instance rather than wrapped first.
  def scaleF64(): Double =
    var total: F64 = F64(1.0)
    var index = 0

    while index < size do
      total = total*doubles(index)/8.0
      index += 1

    total.double

  def scaleDouble(): Double =
    var total = 1.0
    var index = 0

    while index < size do
      total = total*doubles(index)/8.0
      index += 1

    total

  // Checked: the Soundness side raises through the tactic in scope, the primitive side
  // through `Math.addExact`, which throws.
  def checkedSumS32(): Int =
    import arithmeticOptions.checkedOverflow
    var total: S32 = S32(0.bits)
    var index = 0

    while index < size do
      total = total + S32(ints(index).bits)
      index += 1

    total.int

  def exactSumInt(): Int =
    var total = 0
    var index = 0

    while index < size do
      total = Math.addExact(total, ints(index))
      index += 1

    total

  def run(): Unit =
    assert(sumS32() == sumInt(), "S32 sum differs from Int sum")
    assert(productS32() == productInt(), "S32 product differs from Int product")
    assert(sumU64() == U64(sumLong().bits), "U64 sum differs from Long sum")
    assert(quotientS64() == quotientLong(), "S64 quotient differs from Long quotient")
    assert(sumF64() == sumDouble(), "F64 sum differs from Double sum")
    assert(scaleF64() == scaleDouble(), "F64 scaling differs from Double scaling")
    assert(checkedSumS32() == exactSumInt(), "checked S32 sum differs from Math.addExact sum")

    val bench = Bench()

    bench(m"Sum 4096 32-bit signed integers")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.sumS32() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.sumInt() }

    bench(m"Multiply 4096 32-bit signed integers")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.productS32() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.productInt() }

    bench(m"Sum 4096 64-bit unsigned integers")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.sumU64() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.sumLong() }

    bench(m"Divide and add 4096 64-bit signed integers")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.quotientS64() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.quotientLong() }

    bench(m"Sum 4096 64-bit floats")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.sumF64() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.sumDouble() }

    bench(m"Multiply and divide 4096 64-bit floats by Double operands")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.scaleF64() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.scaleDouble() }

    bench(m"Sum 4096 32-bit signed integers, checked for overflow")
      ( target = 1*Second,
        baseline = Engine.Primitive,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.Hypotenuse => '{ hypotenuse.Benchmarks.checkedSumS32() }
        case Engine.Primitive  => '{ hypotenuse.Benchmarks.exactSumInt() }
