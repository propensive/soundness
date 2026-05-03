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

import scala.quoted.*

import ambience.*, environments.java, systems.java
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContext
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.system
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
    val arr = new Array[Byte](10000)
    var i = 0
    while i < arr.length do { arr(i) = 0x41.toByte; i += 1 }
    arr.immutable(using Unsafe)

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

  // ─── helpers (called from quoted bench bodies) ────────────────────────────

  def stringCharAtSum(text: Text): Int =
    val s = text.s
    val n = s.length
    var i = 0
    var acc = 0
    while i < n do { acc ^= s.charAt(i); i += 1 }
    acc

  def cursorNextSingleBlock(text: Text): Int =
    val c = Cursor(Iterator(text))
    var n = 0
    while c.next() do n += 1
    n

  def cursorNextWithLinefeeds(text: Text): Int =
    import zephyrine.lineation.linefeedChars
    val c = Cursor(Iterator(text))
    var n = 0
    while c.next() do n += 1
    n

  def cursorNextFragmented(blocks: List[Text]): Int =
    val c = Cursor(blocks.iterator)
    var n = 0
    while c.next() do n += 1
    n

  def cursorNextData(data: Data): Int =
    val c = Cursor[Data](Iterator(data))
    var n = 0
    while c.next() do n += 1
    n

  def cursorEmptyHoldLoop(text: Text, count: Int): Int =
    val c = Cursor(Iterator(text))
    var i = 0
    while i < count do { c.hold(()); i += 1 }
    i

  def cursorHoldMarkGrabInBlock(text: Text, repeats: Int, span: Int): Int =
    val c = Cursor(Iterator(text))
    var acc = 0
    var i = 0
    while i < repeats do
      c.hold:
        val mk = c.mark
        var k = 0
        while k < span do { c.next(); k += 1 }
        acc ^= c.grab(mk, c.mark).s.length

      i += 1
    acc

  def cursorHoldMarkGrabCrossBlock(blocks: List[Text], span: Int): Int =
    val c = Cursor(blocks.iterator)
    c.hold:
      val mk = c.mark
      var k = 0
      while k < span do { c.next(); k += 1 }
      c.grab(mk, c.mark).s.length

  def cursorConsumeXml(text: Text): Int =
    val c: Cursor[Text] = Cursor(Iterator(text))
    var matched = 0
    c.consume({ matched = -1 })("xml")
    matched

  def cursorSeekSpace(text: Text): Boolean =
    val c = Cursor(Iterator(text))
    c.seek(' ')

  def cursorTake64(text: Text): Int =
    val c = Cursor(Iterator(text))
    c.take(t"")(64).s.length

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    val text10kSize:        Quantity[Bytes[1]] = 10000*Byte
    val xmlInputSize:       Quantity[Bytes[1]] = 47*Byte
    val textWithSpaceSize:  Quantity[Bytes[1]] = 10000*Byte

    suite(m"Linear iteration"):
      bench(m"java.lang.String charAt loop (baseline)")
       (target = 1*Second, operationSize = text10kSize, baseline = Baseline(compare = Min)):
        '{ zephyrine.Benchmarks.stringCharAtSum(zephyrine.Benchmarks.text10k) }

      bench(m"Cursor.next, single 10 KB block")
       (target = 1*Second, operationSize = text10kSize):
        '{ zephyrine.Benchmarks.cursorNextSingleBlock(zephyrine.Benchmarks.text10k) }

      bench(m"Cursor.next + linefeed tracking, single 10 KB block")
       (target = 1*Second, operationSize = text10kSize):
        '{ zephyrine.Benchmarks.cursorNextWithLinefeeds(zephyrine.Benchmarks.text10k) }

      bench(m"Cursor.next, 100 × 100-char fragmented blocks")
       (target = 1*Second, operationSize = text10kSize):
        '{ zephyrine.Benchmarks.cursorNextFragmented(zephyrine.Benchmarks.text10kFragments) }

      bench(m"Cursor[Data].next, 10 KB single block")
       (target = 1*Second, operationSize = text10kSize):
        '{ zephyrine.Benchmarks.cursorNextData(zephyrine.Benchmarks.data10k) }

    suite(m"Hold and capture"):
      bench(m"empty hold {} × 1000 (Held alloc)")
       (target = 1*Second, baseline = Baseline(compare = Min)):
        '{ zephyrine.Benchmarks.cursorEmptyHoldLoop(zephyrine.Benchmarks.text10k, 1000) }

      bench(m"hold + mark + grab 16 chars in-block × 100")
       (target = 1*Second):
        '{ zephyrine.Benchmarks.cursorHoldMarkGrabInBlock(zephyrine.Benchmarks.text10k, 100, 16) }

      bench(m"hold + mark + grab cross-block (350 chars across 4 blocks)")
       (target = 1*Second):
        '{
            zephyrine.Benchmarks.cursorHoldMarkGrabCrossBlock
             (zephyrine.Benchmarks.text10kFragments, 350)
          }

    suite(m"Primitives"):
      bench(m"consume(\"xml\") match")
       (target = 1*Second, operationSize = xmlInputSize):
        '{ zephyrine.Benchmarks.cursorConsumeXml(zephyrine.Benchmarks.xmlInput) }

      bench(m"seek to delimiter at offset 9000")
       (target = 1*Second, operationSize = textWithSpaceSize):
        '{ zephyrine.Benchmarks.cursorSeekSpace(zephyrine.Benchmarks.textWithSpace) }

      bench(m"take(64)")(target = 1*Second):
        '{ zephyrine.Benchmarks.cursorTake64(zephyrine.Benchmarks.text10k) }
