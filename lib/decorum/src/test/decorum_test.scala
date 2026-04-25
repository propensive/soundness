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
      . assert(_.contains("R1-no-tabs"))

      test(m"Line longer than 100 columns is rejected"):
        rules("val x = "+("a"*120)+"\n")
      . assert(_.contains("R2-line-length"))

      test(m"Odd indent is rejected"):
        rules("object A:\n   val x = 1\n")
      . assert(_.contains("R3-indent"))

      test(m"Trailing whitespace is rejected"):
        rules("val x = 1   \n")
      . assert(_.contains("R4-trailing-ws"))

      test(m"Three consecutive blank lines are rejected"):
        rules("val a = 1\n\n\n\nval b = 2\n")
      . assert(_.contains("R5-blank-cap"))

      test(m"Clean stub produces no universal-rule diagnostics"):
        rules("val x: Int = 1\n")
      . assert(_.forall(r => !r.startsWith("R1") && !r.startsWith("R2")
                              && !r.startsWith("R3") && !r.startsWith("R4")
                              && !r.startsWith("R5")))

    suite(m"Phase 1: License, package, imports"):

      test(m"Wrong package name is rejected"):
        Checker.check("<test>", Some("wrongmodule"), stub("val x = 1\n")).toList.map(_.rule)
      . assert(_.contains("R7-package"))

      test(m"Missing blank after package is rejected"):
        val builder = new StringBuilder
        builder.append("/*\n")
        var i = 0
        while i < 30 do { builder.append("x\n"); i += 1 }
        builder.append("*/\n")
        builder.append("package decorum\n")
        builder.append("import gossamer.*\n")
        Checker.check("<test>", Some("decorum"), builder.toString).toList.map(_.rule)
      . assert(_.contains("R8-package-blank"))

      test(m"Out-of-order import groups are rejected"):
        rules("import gossamer.*\n\nimport scala.collection.mutable\n\nval x = 1\n")
      . assert(_.contains("R9-import-order"))

      test(m"Missing blank between import groups is rejected"):
        rules("import scala.collection.mutable\nimport gossamer.*\n\nval x = 1\n")
      . assert(_.contains("R9-import-group-blank"))

      test(m"Out-of-order imports within a group are rejected"):
        rules("import zephyrine.*\nimport anticipation.*\n\nval x = 1\n")
      . assert(_.contains("R9-import-order"))

      test(m"Missing blank line after imports is rejected"):
        rules("import gossamer.*\nval x = 1\n")
      . assert(_.contains("R10-after-imports"))

    suite(m"Phase 1: Per-token rules"):

      test(m"No space after comma is rejected"):
        rules("def f(a: Int,b: Int): Int = a + b\n")
      . assert(_.contains("R11-comma-space-after"))

      test(m"Space before comma is rejected"):
        rules("def f(a: Int , b: Int): Int = a + b\n")
      . assert(_.contains("R11-comma-space-before"))

      test(m"Space inside parentheses on a single line is rejected"):
        rules("def f( a: Int ): Int = a\n")
      . assert(_.contains("R12-bracket-interior"))

      test(m"Line comment without space after // is rejected"):
        rules("val x = 1 //foo\n")
      . assert(_.contains("R13-comment-space"))

      test(m"Block comment outside license header is rejected"):
        rules("/* hello */\nval x = 1\n")
      . assert(_.contains("R14-block-comment"))

      test(m"Scaladoc comment is rejected"):
        rules("/** hello */\nval x = 1\n")
      . assert(_.contains("R14-scaladoc"))

      test(m"Annotation followed by blank line is rejected"):
        rules("@deprecated\n\ndef foo(): Int = 1\n")
      . assert(_.contains("R15-annotation-blank"))

    suite(m"Phase 2: Operator and method-name spacing"):

      test(m"Missing space before `=>` is rejected"):
        rules("val f = (x: Int)=> x + 1\n")
      . assert(_.contains("R16-operator-spacing"))

      test(m"Missing space after `=>` is rejected"):
        rules("val f = (x: Int) =>x + 1\n")
      . assert(_.contains("R16-operator-spacing"))

      test(m"Missing space around `<:` is rejected"):
        rules("def f[T<:Int](x: T): T = x\n")
      . assert(_.contains("R16-operator-spacing"))

      test(m"Missing space around `&` is rejected"):
        rules("type T = Int&String\n")
      . assert(_.contains("R16-operator-spacing"))

      test(m"Spaced `=>` is accepted"):
        rules("val f = (x: Int) => x + 1\n")
      . assert(!_.contains("R16-operator-spacing"))

      test(m"`=>` at line start (continuation) is accepted"):
        rules("val f =\n  (x: Int) =>\n    x + 1\n")
      . assert(!_.contains("R16-operator-spacing"))

      test(m"Symbolic operator method without space before parens is rejected"):
        rules("infix def +(right: Int): Int = right\n")
      . assert(_.contains("R18-symbolic-method"))

      test(m"Symbolic operator method with space is accepted"):
        rules("infix def + (right: Int): Int = right\n")
      . assert(!_.contains("R18-symbolic-method"))

    suite(m"Phase 3: Continuations and shape"):

      test(m"`=>` continuation with wrong space count is rejected"):
        rules("given x: [a]\n=> a is Foo =\n  ???\n")
      . assert(_.contains("R25-hard-space"))

      test(m"`=>` continuation with two spaces is accepted"):
        rules("given x: [a]\n=>  a is Foo =\n\n  ???\n")
      . assert(!_.contains("R25-hard-space"))

      test(m"Heavy-signature `:` continuation with wrong space count is rejected"):
        rules("def foo\n  ( x: Int )\n: Int =\n\n  x\n")
      . assert(_.contains("R25-hard-space"))

      test(m"Missing blank line before heavy-signature body is rejected"):
        rules("def foo\n  ( x: Int )\n:   Int =\n  x\n")
      . assert(_.contains("R21-heavy-body-blank"))

      test(m"Heavy-signature with blank line before body is accepted"):
        rules("def foo\n  ( x: Int )\n:   Int =\n\n  x\n")
      . assert(!_.contains("R21-heavy-body-blank"))

      test(m"Chain `. method` with same-indent preceded by blank is rejected"):
        rules("val a = foo\n\n. method\n")
      . assert(_.contains("R26-chain-blank-forbidden"))

      test(m"Chain `. method` after more-indented line without blank is rejected"):
        rules("val a = foo:\n  bar\n. method\n")
      . assert(_.contains("R26-chain-blank-required"))

    suite(m"Phase 4: Cross-cutting"):

      test(m"Companion object after class is rejected"):
        rules("class Foo:\n  val x = 1\n\nobject Foo:\n  val y = 2\n")
      . assert(_.contains("R28-companion-ordering"))

      test(m"Companion object before class is accepted"):
        rules("object Foo:\n  val y = 2\n\nclass Foo:\n  val x = 1\n")
      . assert(!_.contains("R28-companion-ordering"))

    suite(m"Phase 3: Match-case rules"):

      test(m"Misaligned `=>` in case run is rejected"):
        rules("x match\n  case Short      => 1\n  case LongerName => 2\n  case Med => 3\n")
      . assert(_.contains("R19-case-arrow-align"))

      test(m"Aligned `=>` in case run is accepted"):
        rules("x match\n  case Short      => 1\n  case LongerName => 2\n  case Medium     => 3\n")
      . assert(!_.contains("R19-case-arrow-align"))

      test(m"Multi-line case without preceding blank line is rejected"):
        rules("x match\n  case Foo => 1\n  case Bar =>\n    bigBody\n")
      . assert(_.contains("R20-multiline-case-blank"))

      test(m"Multi-line case as first case (after `match`) is accepted"):
        rules("x match\n  case Bar =>\n    bigBody\n")
      . assert(!_.contains("R20-multiline-case-blank"))

      test(m"Multi-line case after blank is accepted"):
        rules("x match\n  case Foo => 1\n\n  case Bar =>\n    bigBody\n")
      . assert(!_.contains("R20-multiline-case-blank"))

    suite(m"Phase 3: Sibling padding and using alignment"):

      test(m"Adjacent multi-line defs without blank line is rejected"):
        rules("def a: Int =\n  1\ndef b: Int =\n  2\n")
      . assert(_.contains("R27-sibling-padding"))

      test(m"Adjacent multi-line defs with blank line is accepted"):
        rules("def a: Int =\n  1\n\ndef b: Int =\n  2\n")
      . assert(!_.contains("R27-sibling-padding"))

      test(m"First definition in a new indented scope is accepted"):
        rules("class Foo:\n  def a: Int =\n    1\n\n  def b: Int =\n    2\n")
      . assert(!_.contains("R27-sibling-padding"))

      test(m"Same-indent decls in unrelated scopes don't trigger sibling check"):
        rules("class A:\n  def a: Int = 1\n\nclass B:\n  def b: Int = 2\n")
      . assert(!_.contains("R27-sibling-padding"))

      test(m"Misaligned param after `using` clause first row is rejected"):
        rules("def f\n  ( using a: A,\n        b: B )\n:   Int = 0\n")
      . assert(_.contains("R22-using-alignment"))

      test(m"Aligned `using` clause is accepted"):
        rules("def f\n  ( using a: A,\n          b: B )\n:   Int = 0\n")
      . assert(!_.contains("R22-using-alignment"))
