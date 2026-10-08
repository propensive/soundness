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
package polysyllabic

import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import escritoire.*
import escritoire.columnar.Paragraph
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import hieroglyph.*, charsets.utf8Charset, textMetrics.uniformMetric, textSanitizers.strictSanitizer
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*

import hyphenations.englishHyphenation
import denominative.dysasymptotics.linearSize
import denominative.*

object Benchmarks extends Suite(m"Polysyllabic benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // The full text of `War and Peace` (Project Gutenberg #2600), with the
  // header/footer stripped and every run of whitespace collapsed to a single
  // space. `lazy val` so the I/O cost is paid once and the same `Text` is
  // reused across iterations — neither GC nor the read contaminates the
  // timing.
  lazy val warAndPeace: Text = cp"/polysyllabic/warandpeace.txt".read[Text]

  def run(): Unit =
    val bench = Bench()
    val size: Quantity[Bytes[1]] = warAndPeace.s.getBytes("UTF-8").nn.length*Byte

    suite(m"Hyphenation throughput"):
      // Wraps the whole text to 80 columns with English hyphenation in scope. The returned line
      // count keeps the JIT honest — anything dead-coded would collapse this to zero.
      bench(m"wrap War and Peace at 80 cols with English hyphenation")
        ( target = 5*Second, operationSize = size ):
        '{
            Paragraph.fit[Text](Array(polysyllabic.Benchmarks.warAndPeace), 80, TextAlignment.Left)
            . size
        }

      // Inserts soft-hyphens at every admissible break point in every word, exercising the Liang
      // algorithm on every word regardless of column width: a tighter signal for changes to the
      // algorithm or its data structure than wrapping, which only hyphenates overflow words.
      bench(m"hyphenate every word in War and Peace")
        ( target = 5*Second, operationSize = size ):
        '{ polysyllabic.Benchmarks.warAndPeace.hyphenate(hyphen = '-').s.length }
