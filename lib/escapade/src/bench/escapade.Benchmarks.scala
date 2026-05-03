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
package escapade

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
import sedentary.*
import symbolism.*
import temporaryDirectories.system
import vacuous.*

object Benchmarks extends Suite(m"Escapade benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Chars[Power <: Nat] extends Units[Power, Information]
  val Char: MetricUnit[Chars[1]] = MetricUnit(1.0)

  given charDesignation: Designation[Chars[1]] = () => t"ch"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // ─── inputs ───────────────────────────────────────────────────────────────

  // A short styled fragment used for chained-append benchmarks.
  lazy val fragment: Teletype = e" ${Bold}(quick) brown ${Italic}(fox) "

  // A long styled paragraph (~1k chars) for slice/render benchmarks.
  lazy val paragraph: Teletype =
    val sb = new _root_.java.lang.StringBuilder
    var i = 0
    while i < 100 do
      sb.append("the quick brown fox jumps over the lazy dog ")
      i += 1
    val plain: Text = sb.toString.nn.tt
    e"${Bold}(prefix) $plain ${Italic}(suffix)"

  // A "many small spans" Teletype: each word styled differently.
  lazy val rainbow: Teletype =
    val red    = Chroma(0xff5555)
    val yellow = Chroma(0xffff55)
    val green  = Chroma(0x55ff55)
    val cyan   = Chroma(0x55ffff)
    val blue   = Chroma(0x5555ff)
    val magenta = Chroma(0xff55ff)
    var built: Teletype = Teletype.empty
    var i = 0
    while i < 200 do
      val color = (i % 6) match
        case 0 => red
        case 1 => yellow
        case 2 => green
        case 3 => cyan
        case 4 => blue
        case _ => magenta
      built = built.append(e"${Fg(color)}(word$i) ")
      i += 1
    built

  // ─── helpers used inside quoted bench bodies ──────────────────────────────

  def appendN(base: Teletype, suffix: Teletype, n: Int): Teletype =
    var acc = base
    var i = 0
    while i < n do
      acc = acc.append(suffix)
      i += 1
    acc

  def buildInterpolation(): Teletype =
    e"${Bold}(${Fg(Chroma(0xffaa00))}(header)): the ${Italic}(quick) ${Bold}(brown)"
    + e" ${Fg(Chroma(0x55aaff))}(fox) jumps over the ${Underline}(lazy ${Strike}(dog))"

  def renderTrueColor(t: Teletype): Text =
    t.render(termcapDefinitions.xtermTrueColor)

  def renderXterm256(t: Teletype): Text =
    t.render(termcapDefinitions.xterm256)

  def render(input: Teletype): Text = renderTrueColor(input)

  def run(): Unit =
    val bench = Bench()

    val fragSize  = fragment.plain.length*Char
    val paraSize  = paragraph.plain.length*Char
    val rainSize  = rainbow.plain.length*Char

    // ─── concat ─────────────────────────────────────────────────────────────

    suite(m"Concatenate 10 fragments"):
      bench(m"append Teletype × 10")
       (target = 1*Second, operationSize = fragSize*10, baseline = Baseline(compare = Min)):
        '{ escapade.Benchmarks.appendN(escapade.Benchmarks.fragment, escapade.Benchmarks.fragment, 10) }

    suite(m"Concatenate 100 fragments"):
      bench(m"append Teletype × 100")
       (target = 1*Second, operationSize = fragSize*100, baseline = Baseline(compare = Min)):
        '{ escapade.Benchmarks.appendN(escapade.Benchmarks.fragment, escapade.Benchmarks.fragment, 100) }

    suite(m"Concatenate 1000 fragments"):
      bench(m"append Teletype × 1000")
       (target = 1*Second, operationSize = fragSize*1000, baseline = Baseline(compare = Min)):
        '{ escapade.Benchmarks.appendN(escapade.Benchmarks.fragment, escapade.Benchmarks.fragment, 1000) }

    // ─── slice ──────────────────────────────────────────────────────────────

    suite(m"Slice (dropChars / takeChars on long styled text)"):
      bench(m"dropChars(100) on a ~4.5k-char paragraph")
       (target = 1*Second, operationSize = paraSize, baseline = Baseline(compare = Min)):
        '{ escapade.Benchmarks.paragraph.dropChars(100) }

      bench(m"takeChars(half) on a ~4.5k-char paragraph")
       (target = 1*Second, operationSize = paraSize):
        '{ escapade.Benchmarks.paragraph.takeChars(escapade.Benchmarks.paragraph.plain.length/2) }

      bench(m"dropChars(100) on a 200-coloured-words rainbow")
       (target = 1*Second, operationSize = rainSize):
        '{ escapade.Benchmarks.rainbow.dropChars(100) }

    // ─── render ─────────────────────────────────────────────────────────────

    suite(m"Render (Teletype → Text)"):
      bench(m"render long paragraph (true colour)")
       (target = 1*Second, operationSize = paraSize, baseline = Baseline(compare = Min)):
        '{ escapade.Benchmarks.renderTrueColor(escapade.Benchmarks.paragraph) }

      bench(m"render long paragraph (xterm-256)")(target = 1*Second, operationSize = paraSize):
        '{ escapade.Benchmarks.renderXterm256(escapade.Benchmarks.paragraph) }

      bench(m"render 200-coloured-words rainbow (true colour)")
       (target = 1*Second, operationSize = rainSize):
        '{ escapade.Benchmarks.renderTrueColor(escapade.Benchmarks.rainbow) }

    // ─── build (interpolator) ───────────────────────────────────────────────

    suite(m"Build (e\"...\" interpolation)"):
      bench(m"interpolate a nested-markup expression")
       (target = 1*Second, baseline = Baseline(compare = Min)):
        '{ escapade.Benchmarks.buildInterpolation() }
