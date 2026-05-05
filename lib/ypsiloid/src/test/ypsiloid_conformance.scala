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
// Usage:
//   git clone --depth 1 --branch data \
//       https://github.com/yaml/yaml-test-suite.git /tmp/yaml-test-suite
//   ./mill ypsiloid.test.runMain ypsiloid.Conformance /tmp/yaml-test-suite
//
// Each subdirectory of the test suite contains `in.yaml`, an optional
// `error` marker (the input must be rejected), and an optional `in.json`
// (the canonical equivalent for value-comparison via Jacinta).

import java.io.IOException
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

import anticipation.Text
import contingency.{Tactic, strategies}
import hieroglyph.charEncoders
import jacinta.{Json, JsonAst}
import turbulence.read

import strategies.throwUnsafely
import charEncoders.utf8

object Conformance:
  enum Outcome:
    case ParsedAndMatched
    case ParsedButMismatch(actual: String, expected: String)
    case ParsedButNoExpected
    case ShouldHaveErrored
    case ErroredAsExpected
    case UnexpectedError(message: String)

  def main(args: Array[String]): Unit =
    val suiteRoot = args.headOption.getOrElse("/tmp/yaml-test-suite")
    val verbose = args.contains("--verbose")
    val maxFailuresShown = if verbose then Int.MaxValue else 30
    val root = Paths.get(suiteRoot).nn

    if !Files.isDirectory(root) then
      System.err.nn.println(s"Test suite not found at: $suiteRoot")
      System.exit(2)
      return

    val testDirs = Files.list(root).nn.iterator.nn.asScala
                        .filter(p => Files.isDirectory(p))
                        .toList.sortBy(_.getFileName.nn.toString.nn)

    var passed = 0
    val failures = scala.collection.mutable.ArrayBuffer[(String, String, Outcome)]()

    testDirs.foreach: dir =>
      val name = dir.getFileName.nn.toString
      val inYaml = dir.resolve("in.yaml").nn
      if !Files.isReadable(inYaml) then ()
      else
        val description =
          try new String(Files.readAllBytes(dir.resolve("===").nn).nn).trim.nn
          catch case _: IOException => "?"

        val isError = Files.isReadable(dir.resolve("error").nn)
        val yamlText = new String(Files.readAllBytes(inYaml).nn, "UTF-8")

        val outcome: Outcome =
          if isError then
            try
              YamlParser.parse(Text(yamlText))
              Outcome.ShouldHaveErrored
            catch case _: Throwable => Outcome.ErroredAsExpected
          else
            val inJson = dir.resolve("in.json").nn
            try
              val ast = YamlParser.parse(Text(yamlText))
              if !Files.isReadable(inJson) then Outcome.ParsedButNoExpected
              else
                val expectedText = new String(Files.readAllBytes(inJson).nn, "UTF-8")
                val expectedJson = parseExpected(expectedText)
                val actualJson = yamlAstToJson(ast)
                if actualJson == expectedJson then Outcome.ParsedAndMatched
                else
                  Outcome.ParsedButMismatch(jsonString(actualJson), jsonString(expectedJson))
            catch case e: Throwable => Outcome.UnexpectedError(e.toString.takeWhile(_ != '\n'))

        outcome match
          case Outcome.ParsedAndMatched | Outcome.ErroredAsExpected | Outcome.ParsedButNoExpected =>
            passed += 1
          case other => failures.append((name, description, other))

    val total = testDirs.count(d => Files.isReadable(d.resolve("in.yaml").nn))
    val percent = if total == 0 then 0.0 else passed.toDouble / total * 100
    val divider = "=".repeat(80)
    println()
    println(divider)
    println("YAML Test Suite Conformance")
    println(divider)
    println(f"PASSED:    $passed / $total ($percent%.1f%%)")
    println(f"FAILED:    ${failures.size}")
    println()

    if failures.nonEmpty then
      println(s"Failures (${failures.size.min(maxFailuresShown)} of ${failures.size} shown):")
      failures.take(maxFailuresShown).foreach: (name, desc, outcome) =>
        val descShort = desc.linesIterator.next.nn.take(60)
        outcome match
          case Outcome.ParsedButMismatch(actual, expected) =>
            println(s"  $name [$descShort]")
            println(s"    expected: ${expected.take(120)}")
            println(s"    actual:   ${actual.take(120)}")
          case Outcome.ShouldHaveErrored =>
            println(s"  $name [$descShort]: should have errored, but parsed")
          case Outcome.UnexpectedError(msg) =>
            println(s"  $name [$descShort]: unexpected error: ${msg.take(120)}")
          case _ => ()

  private def parseExpected(text: String): Json =
    Text(text).read[Json]

  private def yamlAstToJson(yaml: YamlAst): Json = yaml match
    case YamlAst.Null         => Json.ast(JsonAst(null))
    case YamlAst.Bool(b)      => Json.ast(JsonAst(b))
    case YamlAst.Integer(n)   => Json.ast(JsonAst(n))
    case YamlAst.Decimal(d)   => Json.ast(JsonAst(d))
    case YamlAst.Str(s)       => Json.ast(JsonAst(s.s))

    case YamlAst.Sequence(items) =>
      val converted: IArray[Any] = IArray.from(items.map(yamlAstToJson(_).root.asInstanceOf[Any]))
      Json.ast(JsonAst.arr(converted))

    case YamlAst.Mapping(entries) =>
      val pairs = entries.collect:
        case (YamlAst.Str(s), v) => (s.s, yamlAstToJson(v).root)

      val keys: IArray[String] = IArray.from(pairs.map(_._1))
      val values: IArray[Any] = IArray.from(pairs.map(_._2.asInstanceOf[Any]))
      Json.ast(JsonAst.obj(keys, values))

  private def jsonString(json: Json): String =
    val ast = json.root.asInstanceOf[Any]
    renderAny(ast)

  private def renderAny(value: Any): String = value match
    case null              => "null"
    case b: Boolean        => b.toString
    case n: Long           => n.toString
    case d: Double         => d.toString
    case s: String         => "\"" + s + "\""

    case (keys, values) =>
      val ks = keys.asInstanceOf[IArray[String]]
      val vs = values.asInstanceOf[IArray[Any]]
      val pairs = ks.zip(vs).map: (k, v) =>
        "\"" + k + "\":" + renderAny(v)
      pairs.mkString("{", ",", "}")

    case items: IArray[?] =>
      items.asInstanceOf[IArray[Any]].map(renderAny).mkString("[", ",", "]")

    case other => other.toString
