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

import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

object Benchmarks extends Suite(m"Zephyrine benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // ─── inputs ───────────────────────────────────────────────────────────────

  // A 10 KB single-block string of `'a'`s. Used to measure linear-iteration
  // throughput when the cursor never crosses a block boundary.
  lazy val text10k: Text = Text("a".repeat(10000).nn)

  // The same 10 KB total, split into 100 blocks of 100 chars. Forces the
  // cursor's slow path (`forward`) to fire 99 times per pass.
  lazy val text10kFragments: List[Text] =
    List.tabulate(100)(_ => Text("a".repeat(100).nn))

  // A 10 KB block of bytes (all 'A') for `Cursor[Data]` linear iteration.
  lazy val data10k: Data =
    val arr = new scala.Array[Byte](10000)
    var i = 0
    while i < arr.length do { arr(i) = 0x41.toByte; i += 1 }
    Array.unsafeFrozen(arr)

  // The same 10 KB of bytes as 100 × 100-byte blocks, pulled through a `Stream`
  // by the stream-backed cursor factory.
  lazy val data10kFragments: List[Data] =
    List.tabulate(100): _ =>
      val arr = new scala.Array[Byte](100)
      var i = 0
      while i < arr.length do { arr(i) = 0x41.toByte; i += 1 }
      Array.unsafeFrozen(arr)

  // A 10 KB string with a single space at offset 9000. Used to drive `seek`
  // through 9000 non-matching positions before returning.
  lazy val textWithSpace: Text =
    val sb = new _root_.java.lang.StringBuilder(10000)
    var i = 0; while i < 9000 do { sb.append('a'); i += 1 }
    sb.append(' ')
    i = 0; while i < 999 do { sb.append('a'); i += 1 }
    Text(sb.toString.nn)

  // A short input whose first three chars match the literal "xml" used by
  // `Cursor.consume` — measures the inline `consume` macro on a hit.
  lazy val xmlInput: Text = Text("xml...........................................")

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    val text10kSize:        Quantity[Bytes[1]] = 10000*Byte
    val xmlInputSize:       Quantity[Bytes[1]] = 47*Byte
    val textWithSpaceSize:  Quantity[Bytes[1]] = 10000*Byte

    suite(m"Linear iteration"):
      bench(m"java.lang.String charAt loop (baseline)")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val s = zephyrine.Benchmarks.text10k.s
            val n = s.length
            var i = 0
            var acc = 0
            while i < n do { acc ^= s.charAt(i); i += 1 }
            acc
        }

      bench(m"Cursor.next, single 10 KB block")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor(Iterator(zephyrine.Benchmarks.text10k))
            var n = 0
            while c.next() do n += 1
            n
        }

      bench(m"Cursor.next + linefeed tracking, single 10 KB block")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            import zephyrine.lineation.linefeedChar
            val c = Cursor(Iterator(zephyrine.Benchmarks.text10k))
            var n = 0
            while c.next() do n += 1
            n
        }

      bench(m"Cursor.next, 100 × 100-char fragmented blocks")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor(zephyrine.Benchmarks.text10kFragments.stdlib.iterator)
            var n = 0
            while c.next() do n += 1
            n
        }

      bench(m"Cursor[Data].next, 10 KB single block")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor[Data](Iterator(zephyrine.Benchmarks.data10k))
            var n = 0
            while c.next() do n += 1
            n
        }

      // A cursor over a pull endpoint exercises the stream-backed factory's refill path (the
      // window is transferred into the cursor's buffer once per fill).
      bench(m"Cursor[Data].next over Stream, 100 × 100-byte blocks")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor[Data](zephyrine.Benchmarks.data10kFragments.stdlib.iterator.stream)
            var n = 0
            while c.next() do n += 1
            n
        }

    suite(m"Hold and capture"):
      bench(m"empty hold {} × 1000 (Held alloc)")
        ( target = 1*Second ):
        '{
            val c = Cursor(Iterator(zephyrine.Benchmarks.text10k))
            var i = 0
            while i < 1000 do { c.hold(()); i += 1 }
            i
        }

      bench(m"hold + mark + grab 16 chars in-block × 100")
        (target = 1*Second):
        '{
            val c = Cursor(Iterator(zephyrine.Benchmarks.text10k))
            var acc = 0
            var i = 0

            while i < 100 do
              c.hold:
                val mk = c.mark
                var k = 0
                while k < 16 do { c.next(); k += 1 }
                acc ^= c.grab(mk, c.mark).s.length

              i += 1

            acc
        }

      bench(m"hold + mark + grab cross-block (350 chars across 4 blocks)")
        (target = 1*Second):
        '{
            val c = Cursor(zephyrine.Benchmarks.text10kFragments.stdlib.iterator)

            c.hold:
              val mk = c.mark
              var k = 0
              while k < 350 do { c.next(); k += 1 }
              c.grab(mk, c.mark).s.length
        }

    suite(m"Primitives"):
      bench(m"consume(\"xml\") match")
        ( target = 1*Second, operationSize = xmlInputSize ):
        '{
            val c: Cursor[Text, ?] = Cursor(Iterator(zephyrine.Benchmarks.xmlInput))
            var matched = 0
            c.consume({ matched = -1 })("xml")
            matched
        }

      bench(m"seek to delimiter at offset 9000")
        ( target = 1*Second, operationSize = textWithSpaceSize ):
        '{
            val c = Cursor(Iterator(zephyrine.Benchmarks.textWithSpace))
            c.seek(' '.asInstanceOf[c.addressable.Operand])
        }

      bench(m"take(64)")(target = 1*Second):
        '{
            val c = Cursor(Iterator(zephyrine.Benchmarks.text10k))
            c.take(t"")(64).s.length
        }

    // The safe `peek` extension against the hand-rolled `if finished then -1 else
    // unsafeDatum(using Unsafe) & 0xff` pattern; both should produce the same inner loop.
    suite(m"Safe peek"):
      bench(m"datum + manual sentinel loop, 10 KB bytes (baseline)")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor[Data](Iterator(zephyrine.Benchmarks.data10k))
            var acc = 0

            while !c.finished do
              val b = c.unsafeDatum(using Unsafe).asInstanceOf[Byte] & 0xff
              acc ^= b
              c.advance()

            acc
        }

      bench(m"peek loop, 10 KB bytes")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor[Data](Iterator(zephyrine.Benchmarks.data10k))
            var acc = 0
            while !c.finished do { acc ^= c.peek.asInt; c.advance() }
            acc
        }

      bench(m"peek loop, 10 KB chars")
        ( target = 1*Second, operationSize = text10kSize ):
        '{
            val c = Cursor[Text](Iterator(zephyrine.Benchmarks.text10k))
            var acc = 0
            while !c.finished do { acc ^= c.peek.asInt; c.advance() }
            acc
        }
