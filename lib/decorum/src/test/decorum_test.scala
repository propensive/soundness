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
package decorum

import fulminate.*
import probably.*

object Tests extends Suite(m"Decorum Tests"):
  def stub(body: String): String =
    val builder = new StringBuilder
    builder.append("/*\n")
    var i = 0
    while i < 30 do { builder.append("x\n"); i += 1 }
    builder.append("*/\n")
    builder.append("package decorum\n")
    builder.append("\n")
    builder.append(body)
    builder.toString

  def violations(body: String): List[Violation] =
    Checker.check("<test>", Some("decorum"), stub(body)).toList

  def rules(body: String): List[String] = violations(body).map(_.rule)

  def run(): Unit =
    suite(m"Phase 1: Universal rules"):

      test(m"Tab character is rejected"):
        rules("def\tfoo(): Int = 1\n")
      . assert(_.contains("1"))

      test(m"Line longer than 100 columns is rejected"):
        rules("val x = "+("a"*120)+"\n")
      . assert(_.contains("2"))

      test(m"Odd indent is rejected"):
        rules("object A:\n   val x = 1\n")
      . assert(_.contains("3"))

      test(m"Trailing whitespace is rejected"):
        rules("val x = 1   \n")
      . assert(_.contains("4"))

      test(m"Three consecutive blank lines are rejected"):
        rules("val a = 1\n\n\n\nval b = 2\n")
      . assert(_.contains("5"))

      test(m"Clean stub produces no universal-rule diagnostics"):
        rules("val x: Int = 1\n")
      . assert(_.forall(r => !Set("1", "2", "3", "4", "5").contains(r)))

    suite(m"Phase 1: License, package, imports"):

      test(m"Wrong package name is rejected"):
        Checker.check("<test>", Some("wrongmodule"), stub("val x = 1\n")).toList.map(_.rule)
      . assert(_.contains("7"))

      test(m"Missing blank after package is rejected"):
        val builder = new StringBuilder
        builder.append("/*\n")
        var i = 0
        while i < 30 do { builder.append("x\n"); i += 1 }
        builder.append("*/\n")
        builder.append("package decorum\n")
        builder.append("import gossamer.*\n")
        Checker.check("<test>", Some("decorum"), builder.toString).toList.map(_.rule)
      . assert(_.contains("8"))

      test(m"Out-of-order import groups are rejected"):
        rules("import gossamer.*\n\nimport scala.collection.mutable\n\nval x = 1\n")
      . assert(_.contains("9.2"))

      test(m"Missing blank between import groups is rejected"):
        rules("import scala.collection.mutable\nimport gossamer.*\n\nval x = 1\n")
      . assert(_.contains("9.3"))

      test(m"Out-of-order imports within a group are rejected"):
        rules("import zephyrine.*\nimport anticipation.*\n\nval x = 1\n")
      . assert(_.contains("9.2"))

      test(m"Missing blank line after imports is rejected"):
        rules("import gossamer.*\nval x = 1\n")
      . assert(_.contains("10"))

    suite(m"Phase 1: Per-token rules"):

      test(m"No space after comma is rejected"):
        rules("def f(a: Int,b: Int): Int = a + b\n")
      . assert(_.contains("11.2"))

      test(m"Space before comma is rejected"):
        rules("def f(a: Int , b: Int): Int = a + b\n")
      . assert(_.contains("11.1"))

      test(m"Space inside parentheses on a single line is rejected"):
        rules("def f( a: Int ): Int = a\n")
      . assert(_.contains("12"))

      test(m"Line comment without space after // is rejected"):
        rules("val x = 1 //foo\n")
      . assert(_.contains("13"))

      test(m"Block comment outside license header is rejected"):
        rules("/* hello */\nval x = 1\n")
      . assert(_.contains("14.1"))

      test(m"Scaladoc comment is rejected"):
        rules("/** hello */\nval x = 1\n")
      . assert(_.contains("14.2"))

      test(m"Annotation followed by blank line is rejected"):
        rules("@deprecated\n\ndef foo(): Int = 1\n")
      . assert(_.contains("15.2"))

    suite(m"Phase 2: Operator and method-name spacing"):

      test(m"Missing space before `=>` is rejected"):
        rules("val f = (x: Int)=> x + 1\n")
      . assert(_.contains("16"))

      test(m"Missing space after `=>` is rejected"):
        rules("val f = (x: Int) =>x + 1\n")
      . assert(_.contains("16"))

      test(m"Missing space around `<:` is rejected"):
        rules("def f[T<:Int](x: T): T = x\n")
      . assert(_.contains("16"))

      test(m"Asymmetric `&` spacing is rejected"):
        rules("type T = Int& String\n")
      . assert(_.contains("16"))

      test(m"Single-char `&` with no spaces is accepted"):
        rules("def f(x: Int): Int = x&255\n")
      . assert(!_.contains("16"))

      test(m"Spaced `=>` is accepted"):
        rules("val f = (x: Int) => x + 1\n")
      . assert(!_.contains("16"))

      test(m"`=>` at line start (continuation) is accepted"):
        rules("val f =\n  (x: Int) =>\n    x + 1\n")
      . assert(!_.contains("16"))

      test(m"Symbolic operator method without space before parens is rejected"):
        rules("infix def +(right: Int): Int = right\n")
      . assert(_.contains("18"))

      test(m"Symbolic operator method with space is accepted"):
        rules("infix def + (right: Int): Int = right\n")
      . assert(!_.contains("18"))

    suite(m"Phase 3: Continuations and shape"):

      test(m"`=>` continuation with wrong space count is rejected"):
        rules("given x: [a]\n=> a is Foo =\n  ???\n")
      . assert(_.contains("25"))

      test(m"`=>` continuation with two spaces is accepted"):
        rules("given x: [a]\n=>  a is Foo =\n\n  ???\n")
      . assert(!_.contains("25"))

      test(m"Heavy-signature `:` continuation with wrong space count is rejected"):
        rules("def foo\n  ( x: Int )\n: Int =\n\n  x\n")
      . assert(_.contains("25"))

      test(m"Missing blank line before heavy-signature body is rejected"):
        rules("def foo\n  ( x: Int )\n:   Int =\n  x\n")
      . assert(_.contains("21"))

      test(m"Heavy-signature with blank line before body is accepted"):
        rules("def foo\n  ( x: Int )\n:   Int =\n\n  x\n")
      . assert(!_.contains("21"))

      test(m"Chain `. method` with same-indent preceded by blank is rejected"):
        rules("val a = foo\n\n. method\n")
      . assert(_.contains("26.2"))

      test(m"Chain `. method` after more-indented line without blank is rejected"):
        rules("val a = foo:\n  bar\n. method\n")
      . assert(_.contains("26.1"))

    suite(m"Phase 4: Cross-cutting"):

      test(m"Companion object after class is rejected"):
        rules("class Foo:\n  val x = 1\n\nobject Foo:\n  val y = 2\n")
      . assert(_.contains("28"))

      test(m"Companion object before class is accepted"):
        rules("object Foo:\n  val y = 2\n\nclass Foo:\n  val x = 1\n")
      . assert(!_.contains("28"))

    suite(m"Phase 3: Match-case rules"):

      test(m"Misaligned `=>` in case run is rejected"):
        rules("x match\n  case Short      => 1\n  case LongerName => 2\n  case Med => 3\n")
      . assert(_.contains("19"))

      test(m"Aligned `=>` in case run is accepted"):
        rules("x match\n  case Short      => 1\n  case LongerName => 2\n  case Medium     => 3\n")
      . assert(!_.contains("19"))

      test(m"Multi-line case without preceding blank line is rejected"):
        rules("x match\n  case Foo => 1\n  case Bar =>\n    bigBody\n")
      . assert(_.contains("20"))

      test(m"Multi-line case as first case (after `match`) is accepted"):
        rules("x match\n  case Bar =>\n    bigBody\n")
      . assert(!_.contains("20"))

      test(m"Multi-line case after blank is accepted"):
        rules("x match\n  case Foo => 1\n\n  case Bar =>\n    bigBody\n")
      . assert(!_.contains("20"))

    suite(m"Phase 3: Sibling padding and using alignment"):

      test(m"Adjacent multi-line defs without blank line is rejected"):
        rules("def a: Int =\n  1\ndef b: Int =\n  2\n")
      . assert(_.contains("27"))

      test(m"Adjacent multi-line defs with blank line is accepted"):
        rules("def a: Int =\n  1\n\ndef b: Int =\n  2\n")
      . assert(!_.contains("27"))

      test(m"First definition in a new indented scope is accepted"):
        rules("class Foo:\n  def a: Int =\n    1\n\n  def b: Int =\n    2\n")
      . assert(!_.contains("27"))

      test(m"Same-indent decls in unrelated scopes don't trigger sibling check"):
        rules("class A:\n  def a: Int = 1\n\nclass B:\n  def b: Int = 2\n")
      . assert(!_.contains("27"))

      test(m"Misaligned param after `using` clause first row is rejected"):
        rules("def f\n  ( using a: A,\n        b: B )\n:   Int = 0\n")
      . assert(_.contains("22"))

      test(m"Aligned `using` clause is accepted"):
        rules("def f\n  ( using a: A,\n          b: B )\n:   Int = 0\n")
      . assert(!_.contains("22"))
