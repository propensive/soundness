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
package punctuation

import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charDecoders.utf8Decoder, textSanitizers.strictSanitizer
import prepositional.*
import probably.*
import spectacular.show
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*
import denominative.dysasymptotics.linearSize
import denominative.*

object Benchmarks extends Suite(m"Punctuation benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // ─── inputs ───────────────────────────────────────────────────────────────
  // Loaded once from the bench module's classpath. `lazy val` means the cost
  // of the read isn't paid until the first benchmark touches the input, and
  // the same `Text` instance is reused across iterations so neither GC nor
  // I/O contaminates the timing.

  lazy val small:           Text = cp"/punctuation/small.md".read[Text]
  lazy val medium:          Text = cp"/punctuation/medium.md".read[Text]
  lazy val stressMix:       Text = cp"/punctuation/stress-mix.md".read[Text]
  lazy val emphasisStress:  Text = cp"/punctuation/emphasis-stress.md".read[Text]

  // ─── helpers ──────────────────────────────────────────────────────────────
  // Each helper returns an `Int` derived from the parse result so the JIT
  // cannot dead-code the call. `children.length` is cheap and structurally
  // exercises the entire parse — anything elided would zero this out.

  def parseNative(text: Text): Int = Parser.parse(text).children.size

  // Parsed once, outside the timed region, so the serialization rows measure
  // `.show` alone rather than a parse-and-show round trip.
  lazy val mediumDoc:    Markdown of Layout = Parser.parse(medium)
  lazy val stressMixDoc: Markdown of Layout = Parser.parse(stressMix)

  def serializeNative(doc: Markdown of Layout): Int = doc.show.s.length

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    val smallSize:          Quantity[Bytes[1]] = small.s.getBytes("UTF-8").nn.length*Byte
    val mediumSize:         Quantity[Bytes[1]] = medium.s.getBytes("UTF-8").nn.length*Byte
    val stressMixSize:      Quantity[Bytes[1]] = stressMix.s.getBytes("UTF-8").nn.length*Byte
    val emphasisStressSize: Quantity[Bytes[1]] = emphasisStress.s.getBytes("UTF-8").nn.length*Byte

    // Serialization had no coverage at all, though `escapeTextual` runs once per
    // character of every textual span — the counterpart of the parse side's
    // batched `isSpecial` scan, and the place a character-class test on this
    // side actually costs something.
    suite(m"Markdown serialization throughput"):
      bench(m"serialize medium (~19 KB, real README)")
        ( target = 1*Second, operationSize = mediumSize ):
        '{ punctuation.Benchmarks.serializeNative(punctuation.Benchmarks.mediumDoc) }

      bench(m"serialize stress-mix (~38 KB, every feature)")
        ( target = 1*Second, operationSize = stressMixSize ):
        '{ punctuation.Benchmarks.serializeNative(punctuation.Benchmarks.stressMixDoc) }

    suite(m"Markdown parsing throughput"):
      bench(m"parse small (~2 KB, mixed prose)")
        ( target = 1*Second, operationSize = smallSize ):
        '{ punctuation.Benchmarks.parseNative(punctuation.Benchmarks.small) }

      bench(m"parse medium (~19 KB, real README)")
        ( target = 1*Second, operationSize = mediumSize ):
        '{ punctuation.Benchmarks.parseNative(punctuation.Benchmarks.medium) }

      bench(m"parse stress-mix (~38 KB, every feature)")
        ( target = 1*Second, operationSize = stressMixSize ):
        '{ punctuation.Benchmarks.parseNative(punctuation.Benchmarks.stressMix) }

      bench(m"parse emphasis-stress (~4 KB, inline-dense)")
        ( target = 1*Second, operationSize = emphasisStressSize ):
        '{ punctuation.Benchmarks.parseNative(punctuation.Benchmarks.emphasisStress) }
