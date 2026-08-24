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
package kaleidoscope

import scala.quoted.*

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import praxinoscope.*
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

enum RegexEngine:
  case JavaUtilRegex, PikeVm, StaticFsa, Re2j

enum Corpus:
  case Timestamp, Keywords, Uuid, Pathological

enum SeekEngine:
  case JavaUtilRegex, PraxinoscopeSeek, Re2j

enum SeekCorpus:
  case LiteralPrefix, ClassPrefix

object Benchmarks extends Suite(m"Kaleidoscope regex benchmarks"):
  given decimalizer: Decimalizer     = Decimalizer(2)
  given device:      BenchmarkDevice = LocalhostDevice

  // Each corpus is a (pattern, input) pair exercising a different regex profile: literal-heavy
  // scanning, wide alternation, bounded repetition, and a pathological case on which a
  // backtracking engine is exponential while the RE2 engines stay linear. All patterns are
  // RE2-safe, so the same pattern text drives all four arms. The pathological input does NOT
  // match, which is what forces a backtracker to explore every path.
  val patterns: scala.IArray[Text] = scala.IArray
    ( t"\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} (?:INFO|WARN|ERROR) [a-z.]+ - .*",
      t"(?:(?:alpha|bravo|charlie|delta|echo|foxtrot|golf|hotel|india|juliett|kilo|lima|mike|november|oscar|papa|quebec|romeo|sierra|tango) ?)+",
      t"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",
      t"(?:a|a)*c" )

  val inputs: scala.IArray[Text] = scala.IArray
    ( t"2026-08-23 14:31:07 INFO server.dispatch - accepted connection from 10.0.0.7",
      t"alpha bravo charlie delta echo foxtrot golf hotel india juliett kilo lima mike november oscar papa quebec romeo sierra tango alpha bravo charlie delta",
      t"f81d4fae-7dec-41d0-a765-00a0c91e6bf6",
      ("a".repeat(22).nn).tt )

  val jurPatterns: scala.IArray[java.util.regex.Pattern] =
    scala.IArray.from(patterns.map { pattern => java.util.regex.Pattern.compile(pattern.s).nn })

  val motifs: scala.IArray[Motif] =
    scala.IArray.from(patterns.map { pattern => Motif.parse(pattern) })

  // The rival: Google's re2j, written as its own users would use it.
  val re2jPatterns: scala.IArray[com.google.re2j.Pattern] =
    scala.IArray.from(patterns.map { pattern => com.google.re2j.Pattern.compile(pattern.s).nn })

  def jurMatches(corpus: Int): Boolean =
    jurPatterns(corpus).matcher(inputs(corpus).s).nn.matches

  def pikeMatches(corpus: Int): Boolean = motifs(corpus).matches(inputs(corpus))

  def re2jMatches(corpus: Int): Boolean =
    re2jPatterns(corpus).matcher(inputs(corpus).s).nn.matches

  // The statically-compiled arm: each literal below was compiled during THIS file's
  // compilation — under `regexBackends.re2`, the macro validated it as RE2, compiled it to a
  // praxinoscope program, and (being captureless) generated a dedicated DFA matcher, so the
  // match below runs generated code with no runtime regex machinery at all. The literals must
  // stay in sync with `patterns`; `run()` asserts the arms agree before anything is timed.
  object staticFsa:
    import regexBackends.re2

    def timestamp(text: Text): Boolean = text match
      case r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (?:INFO|WARN|ERROR) [a-z.]+ - .*" => true
      case _                                                                       => false

    def keywords(text: Text): Boolean = text match
      case r"(?:(?:alpha|bravo|charlie|delta|echo|foxtrot|golf|hotel|india|juliett|kilo|lima|mike|november|oscar|papa|quebec|romeo|sierra|tango) ?)+" => true
      case _ => false

    def uuid(text: Text): Boolean = text match
      case r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" => true
      case _                                                               => false

    def pathological(text: Text): Boolean = text match
      case r"(?:a|a)*c" => true
      case _            => false

  // Seek corpora: the match sits at the end of a ~4kB haystack containing neither an `E` nor a
  // digit, so an engine which skips (by literal prefix or first-symbol class) races through the
  // filler while a position-at-a-time engine restarts at all ~4000 offsets. `java.util.regex`
  // has its own literal-prefix scan (Boyer-Moore-style), so the first column is skip-vs-skip.
  // There is no static-FSA arm: the generated matcher is a whole-input test, and seeking runs
  // the Pike VM with the literal-prefix (memchr-style, via the `String.indexOf` intrinsic) or
  // first-symbol-class jump.
  val seekFiller: String = "lorem ipsum dolor sit amet ".repeat(150).nn

  val seekPatterns: scala.IArray[Text] = scala.IArray
    ( t"ERROR [a-z.]+",
      t"\\d{4}-\\d{2}-\\d{2}" )

  val seekInputs: scala.IArray[Text] = scala.IArray
    ( (seekFiller + "ERROR server.overload").tt,
      (seekFiller + "2026-08-24 etc").tt )

  val seekJur: scala.IArray[java.util.regex.Pattern] =
    scala.IArray.from(seekPatterns.map { pattern => java.util.regex.Pattern.compile(pattern.s).nn })

  val seekMotifs: scala.IArray[Motif] =
    scala.IArray.from(seekPatterns.map { pattern => Motif.parse(pattern) })

  val seekRe2j: scala.IArray[com.google.re2j.Pattern] =
    scala.IArray.from(seekPatterns.map { pattern => com.google.re2j.Pattern.compile(pattern.s).nn })

  def jurSeek(corpus: Int): Boolean = seekJur(corpus).matcher(seekInputs(corpus).s).nn.find()
  def pikeSeek(corpus: Int): Boolean = seekMotifs(corpus).seek(seekInputs(corpus)).present
  def re2jSeek(corpus: Int): Boolean = seekRe2j(corpus).matcher(seekInputs(corpus).s).nn.find()

  def fsaMatches(corpus: Int): Boolean = corpus match
    case 0 => staticFsa.timestamp(inputs(0))
    case 1 => staticFsa.keywords(inputs(1))
    case 2 => staticFsa.uuid(inputs(2))
    case _ => staticFsa.pathological(inputs(3))

  def run(): Unit =
    var corpus = 0

    while corpus < 4 do
      val expected = jurMatches(corpus)
      assert(pikeMatches(corpus) == expected, "Pike VM disagrees on corpus "+corpus)
      assert(fsaMatches(corpus) == expected, "static FSA disagrees on corpus "+corpus)
      assert(re2jMatches(corpus) == expected, "re2j disagrees on corpus "+corpus)
      corpus += 1

    var seekCorpus = 0

    while seekCorpus < 2 do
      assert(jurSeek(seekCorpus), "jur seek missed corpus "+seekCorpus)
      assert(pikeSeek(seekCorpus), "praxinoscope seek missed corpus "+seekCorpus)
      assert(re2jSeek(seekCorpus), "re2j seek missed corpus "+seekCorpus)
      seekCorpus += 1

    val bench = Bench()

    bench(m"Whole-input regex match")
      ( target = 1*Second,
        baseline = RegexEngine.JavaUtilRegex,
        comparison = Baseline(compare = Min) )

    . over(RegexEngine, Corpus):
        case (engine, corpus) =>
          val index = corpus.ordinal

          engine match
            case RegexEngine.JavaUtilRegex =>
              '{ kaleidoscope.Benchmarks.jurMatches(${Expr(index)}) }

            case RegexEngine.PikeVm =>
              '{ kaleidoscope.Benchmarks.pikeMatches(${Expr(index)}) }

            case RegexEngine.StaticFsa =>
              '{ kaleidoscope.Benchmarks.fsaMatches(${Expr(index)}) }

            case RegexEngine.Re2j =>
              '{ kaleidoscope.Benchmarks.re2jMatches(${Expr(index)}) }

    bench(m"Seek within a long line")
      ( target = 1*Second,
        baseline = SeekEngine.JavaUtilRegex,
        comparison = Baseline(compare = Min) )

    . over(SeekEngine, SeekCorpus):
        case (engine, corpus) =>
          val index = corpus.ordinal

          engine match
            case SeekEngine.JavaUtilRegex =>
              '{ kaleidoscope.Benchmarks.jurSeek(${Expr(index)}) }

            case SeekEngine.PraxinoscopeSeek =>
              '{ kaleidoscope.Benchmarks.pikeSeek(${Expr(index)}) }

            case SeekEngine.Re2j =>
              '{ kaleidoscope.Benchmarks.re2jSeek(${Expr(index)}) }
