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

      test(m"`if/then/else` does not pool predicate ops with else-clause ops"):
        rules("def f(x: Int, y: Int, a: Int, b: Int): Int = if x == y then a else a^b\n")
      . assert(!_.contains("16"))

      test(m"`else if` chain does not pool ops across branches"):
        rules("def f(x: Boolean, a: Int, b: Int): Int = if x then 1 else if a == b then 2 else a^b\n")
      . assert(!_.contains("16"))

      test(m"Parenthesised `if/then/else` followed by an operator is accepted"):
        rules("def f(x: Int, a: Int, b: Int): Int = (if x == 0 then a else b) + 1\n")
      . assert(!_.contains("16"))

      test(m"`case class` modifier does not flush operator frames spuriously"):
        rules("case class Foo(x: Int, y: Int)\n")
      . assert(!_.contains("16"))

      test(m"Cross-precedence violation is still rejected"):
        rules("def f(a: Int, b: Int, c: Int): Int = a+b * c\n")
      . assert(_.contains("16"))

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

    suite(m"Phase 5: Keyword sequences (R33)"):

      test(m"Compact `if/then/else` is accepted"):
        rules("def f(x: Int): Int = if x > 0 then x else -x\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.2") && !r.contains("33.3"))

      test(m"`if/then/else` with `else` on a new line is accepted"):
        rules("def f(x: Int): Int =\n  if x > 0 then x\n  else -x\n")
      . assert(!_.contains("33.1"))

      test(m"`if/then/else` fully split with aligned keywords is accepted"):
        rules("def f(x: Int): Int =\n  if x > 0\n  then x\n  else -x\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.3"))

      test(m"`if/then/else` with `then` broken but `else` not is rejected"):
        rules("def f(x: Int): Int =\n  if x > 0\n  then x else -x\n")
      . assert(_.contains("33.1"))

      test(m"`if/then/else` with `then` not aligned with `if` is rejected"):
        rules("def f(x: Int): Int =\n  if x > 0\n      then x\n      else -x\n")
      . assert(_.contains("33.3"))

      test(m"`if/then/else` with `then`-body indented but `else`-body inline is rejected"):
        rules("def f(x: Int): Int =\n  if x > 0 then\n    x\n  else -x\n")
      . assert(_.contains("33.2"))

      test(m"`if/then/else` with both bodies indented is accepted"):
        rules("def f(x: Int): Int =\n  if x > 0 then\n    x\n  else\n    -x\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.2") && !r.contains("33.3"))

      test(m"Compact `else if` chain is accepted"):
        rules("def f(x: Int): Int = if x > 0 then 1 else if x < 0 then -1 else 0\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.2") && !r.contains("33.3"))

      test(m"`else if` chain with broken keywords and inline bodies is accepted"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then 1\n"
            +"  else if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(r => !r.contains("33.1") && !r.contains("33.2") && !r.contains("33.3"))

      test(m"`else if` chain with all bodies indented is accepted"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then\n"
            +"    1\n"
            +"  else if x < 0 then\n"
            +"    -1\n"
            +"  else\n"
            +"    0\n" )
      . assert(r => !r.contains("33.1") && !r.contains("33.2") && !r.contains("33.3"))

      test(m"`else if` chain with then-body indented but tail-body inline is rejected"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then\n"
            +"    1\n"
            +"  else if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(_.contains("33.2"))

      test(m"`else if` chain with `else if` misaligned with original `if` is rejected"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then 1\n"
            +"    else if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(_.contains("33.3"))

      test(m"Inner then-only `if` doesn't steal the outer chain's `else`"):
        // The inner `if char >= 0 then ...` (no else) sits inside the
        // outer `then`-body. Without indentation-aware else-matching,
        // findKeyword would absorb the outer `else if esc` as the
        // inner `if`'s else, producing a spurious 33.3 misalignment.
        rules
         ( "def recur(text: String, esc: Boolean): Unit =\n"
            +"  if 1 < text.length\n"
            +"  then\n"
            +"    val char = text.charAt(0)\n"
            +"    if char >= 0 then println(char.toChar)\n"
            +"    recur(text, esc)\n"
            +"  else if esc then throw new RuntimeException(\"\")\n" )
      . assert(r => !r.contains("33.3"))

      test(m"Case-guard `if` in a pattern match doesn't get processed as an if-chain"):
        // `case x if guard => …` looks like an `if` with no `then` to
        // findKeyword unless we bail on `=>` at depth 0. Without the
        // bail, the case-guard `if` would walk forward and pair with
        // an unrelated `then` later in the file, producing spurious
        // 33.3 violations against an `if` column nowhere near the
        // actual chain.
        rules
         ( "def f(x: Int): Int = x match\n"
            +"  case n if n > 0 => 1\n"
            +"  case n if n < 0 => -1\n"
            +"  case _          => 0\n"
            +"\n"
            +"def g(y: Int): Int =\n"
            +"  if y > 0 then y\n"
            +"  else -y\n" )
      . assert(r => !r.contains("33.3"))

      test(m"For-comprehension filter `if` doesn't get processed as an if-chain"):
        // `if filter` inside a for-comprehension has no `then`. Without
        // bailing on `yield`/`do`/`<-`, findKeyword would walk past the
        // filter and pair with an unrelated `then` later in the file.
        rules
         ( "def f(xs: List[Int]): List[Int] = for x <- xs if x > 0 yield x\n"
            +"\n"
            +"def g(y: Int): Int =\n"
            +"  if y > 0 then y\n"
            +"  else -y\n" )
      . assert(r => !r.contains("33.3"))

      test(m"Compact `while/do` is accepted"):
        rules("def f(): Unit = while running() do step()\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.3"))

      test(m"`while/do` with `do` misaligned with `while` is rejected"):
        rules("def f(): Unit =\n  while running()\n      do step()\n")
      . assert(_.contains("33.3"))

      test(m"Compact `try/catch/finally` is accepted"):
        rules("def f: Int = try compute() catch case e: Throwable => 0 finally cleanup()\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.3"))

      test(m"`try/catch/finally` with `catch` broken but `finally` inline is rejected"):
        rules("def f: Int =\n  try compute()\n  catch case e: Throwable => 0 finally cleanup()\n")
      . assert(_.contains("33.1"))

      test(m"Compact `for/yield` is accepted"):
        rules("def f: List[Int] = for x <- List(1, 2) yield x\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.3"))

      test(m"Compact `for/do` is accepted"):
        rules("def f(): Unit = for x <- List(1, 2) do println(x)\n")
      . assert(r => !r.contains("33.1") && !r.contains("33.3"))

    suite(m"Phase 5: For-comprehension generator alignment (R34)"):

      test(m"For-comprehension aligned-LHS style is accepted"):
        rules("val r =\n  for x <- List(1)\n      y <- List(2)\n  yield x + y\n")
      . assert(r => !r.contains("34.1") && !r.contains("34.2") && !r.contains("34.3"))

      test(m"For-comprehension indented-block style is accepted"):
        rules("val r =\n  for\n    x <- List(1)\n    y <- List(2)\n  yield x + y\n")
      . assert(r => !r.contains("34.1") && !r.contains("34.2") && !r.contains("34.3"))

      test(m"For-comprehension with misaligned generator LHS is rejected"):
        rules("val r =\n  for x <- List(1)\n     y <- List(2)\n  yield x + y\n")
      . assert(_.contains("34.2"))

      test(m"For-comprehension with misaligned `<-` operators is rejected"):
        rules("val r =\n  for x  <- List(1)\n      y <- List(2)\n  yield x + y\n")
      . assert(_.contains("34.1"))

      test(m"For-comprehension with `if` filter aligned with `<-` is accepted"):
        rules
         ( "val r =\n  for x  <- List(1)\n         if x > 0\n      y  <- List(2)"
            +"\n  yield x + y\n" )
      . assert(r => !r.contains("34.3"))

      test(m"For-comprehension with `if` filter at LHS column is rejected"):
        rules
         ( "val r =\n  for x  <- List(1)\n      if x > 0\n      y  <- List(2)"
            +"\n  yield x + y\n" )
      . assert(_.contains("34.3"))
