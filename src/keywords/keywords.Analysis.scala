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
package keywords

import java.nio.file as jnf

import scala.collection.mutable as scm
import scala.jdk.CollectionConverters.*

import anticipation.*
import denominative.*
import gossamer.*
import harlequin.*
import prophesy.*
import vacuous.*

// A development-time tool (not published; see the `keywords` module in build.mill and the
// `make keywords` target): tokenizes a corpus of Scala source with Harlequin, maps it onto
// Prophesy's lexeme alphabet via `Lexis`, and analyzes the reversed ≤5-lexeme context of
// every keyword occurrence. For each context it reports the observed keyword set and its
// *settling depth* — the smallest lookback at which looking further back no longer refines
// the set — and emits a draft `KeywordPattern` tree to curate into
// `prophesy.ScalaKeywords`. With `--check`, it instead measures the committed pattern tree's
// recall: every corpus keyword occurrence should be predicted by its own context.
//
// Roots are given as arguments (default `lib`); if `SOUNDNESS_PROSCALA_SRC` names a proscala
// checkout, its compiler and library sources and its *positive* test corpora (`tests/pos`,
// `tests/run` — never the deliberately failing `tests/neg`) are included.
object Analysis:
  private val depth: Int = 5
  private val minCount: Int = 10

  private def scalaFiles(root: String): List[jnf.Path] =
    val path = jnf.Path.of(root).nn

    if !jnf.Files.isDirectory(path) then Nil else
      jnf.Files.walk(path).nn.iterator.nn.asScala
      . filter { p => p.toString.endsWith(".scala") && jnf.Files.isRegularFile(p) }
      . toList

  private def render(lexeme: Lexeme): String = lexeme match
    case Lexeme.Keyword(word)  => s"`$word`"
    case Lexeme.Symbol(glyph)  => s"'$glyph'"
    case Lexeme.Open(bracket)  => s"open-$bracket"
    case Lexeme.Close(bracket) => s"close-$bracket"
    case Lexeme.Term           => "TERM"
    case Lexeme.Typal          => "TYPE"
    case Lexeme.Literal        => "LIT"
    case Lexeme.Break          => "BREAK"
    case Lexeme.Start          => "START"
    case Lexeme.Error          => "ERR"

  private def renderContext(context: List[Lexeme]): String =
    if context.isEmpty then "<empty>" else context.map(render).mkString(" :: ")

  private def code(lexeme: Lexeme): String = lexeme match
    case Lexeme.Keyword(word)  => s"Lexeme.Keyword(t\"$word\")"
    case Lexeme.Symbol(glyph)  => s"Lexeme.Symbol(t\"$glyph\")"
    case Lexeme.Open(bracket)  => s"Lexeme.Open(Bracket.$bracket)"
    case Lexeme.Close(bracket) => s"Lexeme.Close(Bracket.$bracket)"
    case other                 => s"Lexeme.$other"

  // In --simulate mode, every `sampleRate`th keyword occurrence is checked the way
  // completion time would see it: the source is truncated at the keyword and the context
  // recomputed by `Lexis.context` — so tokenization differences on incomplete input (error
  // recovery, accent overlays) are measured, not assumed away.
  private val sampleRate: Int = 50

  def main(args: Array[String]): Unit =
    val check = args.exists(_ == "--check")
    val simulate = args.exists(_ == "--simulate")
    val given0 = args.toList.filter(!_.startsWith("--"))
    val given1 = if given0.isEmpty then List("lib") else given0

    val proscala = Option(System.getenv("SOUNDNESS_PROSCALA_SRC")).toList.flatMap: home =>
      List(s"$home/compiler/src", s"$home/library/src", s"$home/tests/pos", s"$home/tests/run")
      . filter { dir => jnf.Files.isDirectory(jnf.Path.of(dir)) }

    val roots = given1 ++ proscala
    println(s"corpus roots: ${roots.mkString(", ")}")

    val files = roots.flatMap(scalaFiles)
    println(s"corpus files: ${files.length}")

    // Full-depth context -> keyword -> count. Contexts are reversed, nearest-first, exactly
    // as `Lexis.context` produces them at completion time.
    val contexts = scm.HashMap[List[Lexeme], scm.HashMap[Text, Int]]()
    var occurrences = 0
    var failures = 0

    // In --check mode, misses by (keyword, context).
    val misses = scm.HashMap[(Text, List[Lexeme]), Int]()
    var checked = 0
    var hits = 0

    // Simulation state: sampled truncated-context checks.
    val simMisses = scm.HashMap[(Text, List[Lexeme]), Int]()
    var simChecked = 0
    var simHits = 0
    var occurrenceIndex = 0

    // Absolute character offsets are reconstructed from per-line token widths, which only
    // agree with the raw text when no tab expansion or CR stripping occurred.
    def simulateFile(text: Text): Unit =
      if text.s.indexOf('\t') < 0 && text.s.indexOf('\r') < 0 then
        val code = SourceCode(Scala, text, Unset)
        var lineStart = 0

        code.lines.foreach: line =>
          line.foreach: token =>
            Lexis.lexeme(token) match
              case Lexeme.Keyword(word) =>
                occurrenceIndex += 1

                if occurrenceIndex % sampleRate == 0 then
                  val offset = lineStart + token.span.startColumn.lay(0)(_.n0)
                  simChecked += 1

                  val (_, context) = Lexis.context(text, offset.z)

                  if ScalaKeywords.pattern(context).keywords.contains(word) then simHits += 1
                  else
                    simMisses.updateWith((word, context.take(depth))):
                      _.map(_ + 1).orElse(Some(1))

              case _ =>
                ()

          lineStart += line.map(_.length).sum + 1

    files.foreach: file =>
      try
        val text = String(jnf.Files.readAllBytes(file), "UTF-8").tt

        if simulate then simulateFile(text)

        val lexemes = Lexis.lexemes(SourceCode(Scala, text, Unset))
        var before: List[Lexeme] = Nil

        lexemes.foreach: lexeme =>
          lexeme match
            case Lexeme.Keyword(word) =>
              occurrences += 1

              if check then
                checked += 1
                val context = before.take(8)

                if ScalaKeywords.pattern(context).keywords.contains(word) then hits += 1
                else misses.updateWith((word, before.take(depth)))(_.map(_ + 1).orElse(Some(1)))
              else
                val context = before.take(depth)
                val counts = contexts.getOrElseUpdate(context, scm.HashMap())
                counts(word) = counts.getOrElse(word, 0) + 1

            case _ =>
              ()

          before = lexeme :: before

      // Throwable, not NonFatal: a pathological corpus file (e.g. a generated test source
      // deep enough to overflow the scanner's stack) should be skipped, not end the run.
      catch case _: Throwable => failures += 1

    println(s"keyword occurrences: $occurrences (unreadable files: $failures)")

    if simulate then
      val recall = if simChecked == 0 then 0.0 else simHits.toDouble / simChecked * 100
      println(f"simulated recall (every $sampleRate%dth occurrence, caret-truncated): " +
        f"$simHits/$simChecked ($recall%.2f%%)")
      println("simulated top misses:")

      simMisses.toList.sortBy(-_(1)).take(40).foreach:
        case ((word, context), count) =>
          println(f"  $count%7d  $word%-12s after ${renderContext(context)}")

    if check then
      val recall = if checked == 0 then 0.0 else hits.toDouble / checked * 100
      println(f"recall: $hits/$checked ($recall%.2f%%)")
      println("top misses:")

      misses.toList.sortBy(-_(1)).take(60).foreach:
        case ((word, context), count) =>
          println(f"  $count%7d  $word%-12s after ${renderContext(context)}")
    else if !simulate then
      report(contexts)

  private type Counts = Map[Text, Int]

  private def merge(left: Counts, right: Counts): Counts =
    right.foldLeft(left) { case (acc, (word, count)) => acc.updated(word, acc.getOrElse(word, 0) + count) }

  private def report(full: scm.HashMap[List[Lexeme], scm.HashMap[Text, Int]]): Unit =
    val contexts: Map[List[Lexeme], Counts] = full.view.mapValues(_.toMap).toMap

    // Truncations: depth d -> context prefix -> merged counts.
    val byDepth: Map[Int, Map[List[Lexeme], Counts]] =
      (1 to depth).map { d => d -> contexts.groupMapReduce(_(0).take(d))(_(1))(merge) }.toMap

    // A depth-d prefix is settled when every depth-(d+1) extension observes the same keyword
    // set — looking further back refines nothing.
    def settled(prefix: List[Lexeme], d: Int): Boolean =
      d >= depth || {
        val expected = byDepth(d)(prefix).keySet

        byDepth(d + 1).forall: (extension, counts) =>
          extension.take(d) != prefix || counts.keySet == expected
      }

    val keywordTotals = contexts.values.foldLeft(Map[Text, Int]())(merge)
    println(s"distinct keywords: ${keywordTotals.size}")
    println(s"distinct depth-$depth contexts: ${contexts.size}")
    println()
    println("== keyword frequencies ==")

    keywordTotals.toList.sortBy(-_(1)).foreach: (word, count) =>
      println(f"  $count%8d  $word")

    println()
    println("== settling depths (contexts with >= " + minCount + " occurrences) ==")

    (1 to depth).foreach: d =>
      val prefixes = byDepth(d).filter(_(1).values.sum >= minCount)
      val done = prefixes.count { (prefix, _) => settled(prefix, d) }
      println(s"  depth $d: ${prefixes.size} contexts, $done settled")

    println()
    println("== draft pattern tree ==")

    def emit(prefix: List[Lexeme], d: Int, indent: String): Unit =
      val counts = byDepth(d)(prefix)
      val words = counts.keySet.toList.sortBy(_.s).map { w => s"t\"$w\"" }.mkString(", ")
      val branch = s"Element.Exact(${code(prefix.last)})"
      val total = counts.values.sum

      if settled(prefix, d) || d == depth then
        println(s"$indent$branch -> KeywordPattern(Keywords(Set($words))),  // $total")
      else
        println(s"$indent$branch -> KeywordPattern(Keywords(Set($words)), List(  // $total")

        byDepth(d + 1).toList
        . filter { (extension, counts) => extension.take(d) == prefix && counts.values.sum >= minCount }
        . sortBy(-_(1).values.sum)
        . foreach { (extension, _) => emit(extension, d + 1, indent + "  ") }

        println(s"$indent)),")

    byDepth(1).toList
    . filter(_(1).values.sum >= minCount)
    . sortBy(-_(1).values.sum)
    . foreach { (prefix, _) => emit(prefix, 1, "  ") }
