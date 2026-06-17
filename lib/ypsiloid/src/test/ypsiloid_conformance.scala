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
package ypsiloid

// Conformance runner against the YAML 1.2 test suite
// (https://github.com/yaml/yaml-test-suite).
//
// The Mill `yamlTestSuite` task on `ypsiloid.test` clones the suite at a
// pinned commit and exposes its path via the `ypsiloid.yaml-test-suite.path`
// system property (set on `forkArgs`). When invoked outside Mill, the suite
// path can be passed as the first program argument; falls back to
// `/tmp/yaml-test-suite` if neither is set.
//
// Each subdirectory of the test suite contains `in.yaml`, an optional
// `error` marker, optional `in.json`, and the suite's `tags/<category>/<id>`
// symlink forest classifies each test by feature.

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

import anticipation.Text
import contingency.{Tactic, strategies}
import hieroglyph.charEncoders
import jacinta.Json
import turbulence.read

import strategies.throwUnsafely
import charEncoders.utf8Encoder

object Conformance:
  // Tags whose test cases exercise YAML features outside the Ypsiloid
  // subset. Any test carrying one of these tags is reported as
  // out-of-scope: a known limitation rather than a bug.
  val excludedTags: Set[String] = Set(
    "directive",      // %TAG / %YAML directives
    "complex-key",    // ? complex keys
    "explicit-key",   // ? explicit keys (depends on complex-key support)
    "empty-key",      // empty keys (depends on ? syntax)
    "1.3-err",        // YAML 1.3-only error cases
    "1.3-mod",        // YAML 1.3-only modifications
    "local-tag",      // local !tag handling
    "unknown-tag",    // strict unknown-tag rejection
    "duplicate-key",  // duplicate-key detection
    "footer"          // ... document-end footer rules
  )

  enum Outcome:
    case Passed
    case ShouldHaveErrored
    case Mismatch(actual: String, expected: String)
    case UnexpectedError(message: String)

  case class TestCase
                ( id:           String,
                  description:  String,
                  yamlText:     String,
                  isError:      Boolean,
                  expectedJson: Option[String],
                  tags:         Set[String] )
  :
    def inScope: Boolean = !tags.exists(excludedTags.contains)

  case class Result(testCase: TestCase, outcome: Outcome):
    def passed: Boolean = outcome match
      case Outcome.Passed => true
      case _              => false

  def suitePath: String =
    val sysProp = System.getProperty("ypsiloid.yaml-test-suite.path")
    if sysProp != null && sysProp.nonEmpty then sysProp
    else "/tmp/yaml-test-suite"

  def available(rootStr: String = suitePath): Boolean =
    Files.isDirectory(Paths.get(rootStr).nn)

  def loadTestCases(rootStr: String = suitePath): List[TestCase] =
    val root = Paths.get(rootStr).nn

    if !Files.isDirectory(root) then
      throw new RuntimeException(s"Test suite not found at: $rootStr")

    val tagsByTest = loadTagsByTest(root.resolve("tags").nn)

    Files.list(root).nn.iterator.nn.asScala
      .filter(p => Files.isDirectory(p) && p.getFileName.nn.toString != "tags")
      .toList
      .sortBy(_.getFileName.nn.toString)
      .flatMap: dir =>
        val id = dir.getFileName.nn.toString
        val inYaml = dir.resolve("in.yaml")
        if !Files.isReadable(inYaml) then None
        else
          val description =
            try new String(Files.readAllBytes(dir.resolve("===")).nn).trim.nn
            catch case _: IOException => "?"

          val isError = Files.isReadable(dir.resolve("error"))
          val yamlText = new String(Files.readAllBytes(inYaml).nn, "UTF-8")

          val expectedJson =
            val inJson = dir.resolve("in.json")
            if Files.isReadable(inJson)
            then Some(new String(Files.readAllBytes(inJson).nn, "UTF-8"))
            else None

          Some(TestCase(id, description, yamlText, isError, expectedJson,
                        tagsByTest.getOrElse(id, Set.empty)))

  private def loadTagsByTest(tagsDir: Path): Map[String, Set[String]] =
    if !Files.isDirectory(tagsDir) then Map.empty
    else
      val accumulator = scala.collection.mutable.Map.empty[String, Set[String]]
      Files.list(tagsDir).nn.iterator.nn.asScala.foreach: tagDir =>
        if Files.isDirectory(tagDir) then
          val tagName = tagDir.getFileName.nn.toString
          Files.list(tagDir).nn.iterator.nn.asScala.foreach: testLink =>
            val testId = testLink.getFileName.nn.toString
            accumulator.update(testId, accumulator.getOrElse(testId, Set.empty) + tagName)
      accumulator.toMap

  def runTestCase(testCase: TestCase): Result =
    val outcome: Outcome =
      if testCase.isError then
        try
          YamlParser.parseAll(Text(testCase.yamlText))
          Outcome.ShouldHaveErrored
        catch case _: Throwable => Outcome.Passed
      else
        try
          val docs = YamlParser.parseAll(Text(testCase.yamlText))
          testCase.expectedJson match
            case None => Outcome.Passed
            case Some(expectedText) if expectedText.trim.nn.isEmpty =>
              // Empty in.json: no comparison, pass if YAML parsed without error.
              Outcome.Passed
            case Some(expectedText) =>
              val expectedDocs = parseJsonStream(expectedText)
              val actualDocs = docs.map(yamlAstToJson)
              val actualStr = actualDocs.map(jsonString).mkString(", ")
              val expectedStr = expectedDocs.map(jsonString).mkString(", ")
              // Compare via canonical jsonString rendering rather than
              // Json equality — jacinta's Json.equals has no `null` case
              // and reports `null == null` as false. Rendering produces
              // a stable canonical form for both sides.
              if actualDocs.length == expectedDocs.length && actualStr == expectedStr
              then Outcome.Passed
              else Outcome.Mismatch(actualStr, expectedStr)
        catch case e: Throwable =>
          Outcome.UnexpectedError(e.toString.takeWhile(_ != '\n'))

    Result(testCase, outcome)

  // Parse a sequence of top-level JSON values from a single string.
  // The yaml-test-suite's `in.json` files for multi-document YAML
  // inputs concatenate one JSON value per output document, separated
  // by whitespace.
  private def parseJsonStream(text: String): List[Json] =
    val results = scala.collection.mutable.ArrayBuffer[Json]()
    var i = 0
    val n = text.length

    while i < n do
      while i < n && isJsonWhitespace(text.charAt(i)) do i += 1
      if i >= n then return results.toList

      val start = i
      val c = text.charAt(i)
      if c == '{' || c == '[' then
        val close: Char = if c == '{' then '}' else ']'
        var depth = 1
        var inString = false
        var escaped = false
        i += 1
        while i < n && depth > 0 do
          val ch = text.charAt(i)
          if escaped then escaped = false
          else if inString then
            if ch == '\\' then escaped = true
            else if ch == '"' then inString = false
          else if ch == '"' then inString = true
          else if ch == '{' || ch == '[' then depth += 1
          else if ch == '}' || ch == ']' then depth -= 1
          i += 1
      else if c == '"' then
        i += 1
        var escaped = false
        while i < n && (escaped || text.charAt(i) != '"') do
          if escaped then escaped = false
          else if text.charAt(i) == '\\' then escaped = true
          i += 1
        if i < n then i += 1
      else
        while i < n && !isJsonWhitespace(text.charAt(i)) do i += 1

      val valueText = text.substring(start, i).nn
      results += Text(valueText).read[Json]

    results.toList

  private inline def isJsonWhitespace(c: Char): Boolean =
    c == ' ' || c == '\n' || c == '\t' || c == '\r'

  def main(args: Array[String]): Unit =
    val suiteRoot = args.headOption.filterNot(_.startsWith("--")).getOrElse(suitePath)
    val verbose = args.contains("--verbose")
    val maxFailuresShown = if verbose then Int.MaxValue else 30

    val cases = loadTestCases(suiteRoot)
    val results = cases.map(runTestCase)
    val (inScopeResults, outOfScopeResults) = results.partition(_.testCase.inScope)
    val inScopePassed = inScopeResults.count(_.passed)
    val outOfScopePassed = outOfScopeResults.count(_.passed)
    val overallPassed = inScopePassed + outOfScopePassed
    val overallTotal = results.length

    val divider = "=".repeat(80)
    println()
    println(divider)
    println("YAML Test Suite Conformance")
    println(divider)
    println(f"In-scope:     $inScopePassed%4d / ${inScopeResults.length}%-4d  "
          + f"(${pct(inScopePassed, inScopeResults.length)}%.1f%%)")
    println(f"Out-of-scope: $outOfScopePassed%4d / ${outOfScopeResults.length}%-4d "
          + f"  (${pct(outOfScopePassed, outOfScopeResults.length)}%.1f%%, informational)")
    println(f"Overall:      $overallPassed%4d / $overallTotal%-4d  "
          + f"(${pct(overallPassed, overallTotal)}%.1f%%)")
    println()

    val inScopeFailures = inScopeResults.filterNot(_.passed)
    if inScopeFailures.nonEmpty then
      println(s"In-scope failures (${inScopeFailures.length.min(maxFailuresShown)}"
            + s" of ${inScopeFailures.length} shown):")
      inScopeFailures.take(maxFailuresShown).foreach: result =>
        printFailure(result)

    if args.contains("--out-of-scope") then
      val outFailures = outOfScopeResults.filterNot(_.passed)
      excludedTags.toList.sorted.foreach: tag =>
        val matching = outFailures.filter(_.testCase.tags.contains(tag))
        if matching.nonEmpty then
          println()
          println(s"== Out-of-scope tag '$tag' (${matching.length} failing):")
          matching.foreach: r =>
            val descShort = r.testCase.description.linesIterator.next().take(60)
            val tagsShort = r.testCase.tags.toList.sorted.mkString("{", ",", "}")
            println(s"  ${r.testCase.id} $tagsShort  $descShort")
            r.outcome match
              case Outcome.Mismatch(actual, expected) =>
                println(s"    expected: ${expected.take(120)}")
                println(s"    actual:   ${actual.take(120)}")
              case Outcome.ShouldHaveErrored =>
                println(s"    (should have errored)")
              case Outcome.UnexpectedError(msg) =>
                println(s"    error: ${msg.take(120)}")
              case Outcome.Passed => ()

    if inScopeFailures.nonEmpty then System.exit(1)

  private def pct(n: Int, total: Int): Double =
    if total == 0 then 0.0 else n.toDouble / total * 100

  private def printFailure(result: Result): Unit =
    val descShort = result.testCase.description.linesIterator.next().take(60)
    val tagsShort = if result.testCase.tags.isEmpty then ""
                    else result.testCase.tags.toList.sorted.mkString(" {", ",", "}")
    result.outcome match
      case Outcome.Mismatch(actual, expected) =>
        println(s"  ${result.testCase.id} [$descShort]$tagsShort")
        println(s"    expected: ${expected.take(120)}")
        println(s"    actual:   ${actual.take(120)}")

      case Outcome.ShouldHaveErrored =>
        println(s"  ${result.testCase.id} [$descShort]$tagsShort: should have errored")

      case Outcome.UnexpectedError(msg) =>
        println(s"  ${result.testCase.id} [$descShort]$tagsShort: unexpected error: " + msg)

      case Outcome.Passed => ()

  private def yamlAstToJson(yaml: Yaml.Ast): Json = yaml match
    case Yaml.Ast.Null         => Json.ast(Json.Ast(Json.JsonNull))
    case Yaml.Ast.Bool(b)      => Json.ast(Json.Ast(b))
    case Yaml.Ast.Integer(n)   => Json.ast(Json.Ast(n))
    case Yaml.Ast.Decimal(d)   => Json.ast(Json.Ast(d))
    case Yaml.Ast.Str(s)       => Json.ast(Json.Ast(s.s))

    case Yaml.Ast.Sequence(items) =>
      val converted: IArray[Any] =
        IArray.from(items.map(item => Json.unseal(yamlAstToJson(item)).asInstanceOf[Any]))
      Json.ast(Json.Ast.arr(converted))

    case Yaml.Ast.Mapping(entries) =>
      val pairs = entries.collect:
        case (Yaml.Ast.Str(s), v) => (s.s, Json.unseal(yamlAstToJson(v)))

      val keys: IArray[String] = IArray.from(pairs.map(_._1))
      val values: IArray[Any] = IArray.from(pairs.map(_._2.asInstanceOf[Any]))
      Json.ast(Json.Ast.obj(keys, values))

  private def jsonString(json: Json): String =
    renderAny(Json.unseal(json).asInstanceOf[Any])

  private def renderAny(value: Any): String = value match
    case null              => "null"
    case b: Boolean        => b.toString
    case n: Long           => n.toString
    case d: Double         =>
      // Doubles whose value is a whole integer (e.g. 450.0 from a YAML
      // float literal `450.00`) render in integer form to match the
      // canonical JSON the test fixtures produce.
      if !d.isNaN && !d.isInfinity && d == d.toLong.toDouble then d.toLong.toString
      else d.toString
    case s: String         => "\"" + s + "\""

    case nums: Array[Double] @unchecked =>
      // jacinta stores number-only JSON arrays unboxed as `Array[Double]`.
      // Render as a flat JSON array.
      nums.iterator.map(d => renderAny(d)).mkString("[", ",", "]")

    case items: IArray[?] =>
      // JSON objects and (mixed-type) arrays share the same
      // `IArray[Any]` runtime representation in jacinta: even length is
      // an object (alternating key, value, …), odd length is an array
      // (the last slot may be a sentinel pad when the logical element
      // count is even). Objects are rendered with sorted keys so the
      // comparison is independent of source order, which JSON treats
      // as insignificant.
      val arr = items.asInstanceOf[IArray[Any]]
      val n = arr.length
      if (n & 1) == 0 then
        val pairs = (0 until n/2).map: i =>
          (arr(i*2).asInstanceOf[String], arr(i*2 + 1))
        pairs.sortBy(_._1)
            .map((k, v) => "\"" + k + "\":" + renderAny(v))
            .mkString("{", ",", "}")
      else
        val last = arr(n - 1)
        val effective =
          if last.asInstanceOf[AnyRef] eq jacinta.Json.Ast.arrayPad then n - 1
          else n
        (0 until effective).map(i => renderAny(arr(i))).mkString("[", ",", "]")

    case other => other.toString
