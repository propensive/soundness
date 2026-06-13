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

import soundness.*

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
      . assert(_.contains("135"))

      test(m"Line longer than 100 columns is rejected"):
        rules("val x = "+"a".repeat(120)+"\n")
      . assert(_.contains("230"))

      test(m"Odd indent is rejected"):
        rules("object A:\n   val x = 1\n")
      . assert(_.contains("926"))

      test(m"Trailing whitespace is rejected"):
        rules("val x = 1   \n")
      . assert(_.contains("015"))

      test(m"Three consecutive blank lines are rejected"):
        rules("val a = 1\n\n\n\nval b = 2\n")
      . assert(_.contains("783"))

      test(m"Clean stub produces no universal-rule diagnostics"):
        rules("val x: Int = 1\n")
      . assert(_.forall(r => !Set("135", "230", "926", "015", "783").contains(r)))

    suite(m"Phase 1: License, package, imports"):

      test(m"Wrong package name is rejected"):
        Checker.check("<test>", Some("wrongmodule"), stub("val x = 1\n")).toList.map(_.rule)
      . assert(_.contains("131"))

      test(m"Missing blank after package is rejected"):
        val builder = new StringBuilder
        builder.append("/*\n")
        var i = 0
        while i < 30 do { builder.append("x\n"); i += 1 }
        builder.append("*/\n")
        builder.append("package decorum\n")
        builder.append("import gossamer.*\n")
        Checker.check("<test>", Some("decorum"), builder.toString).toList.map(_.rule)
      . assert(_.contains("658"))

      test(m"Out-of-order import groups are rejected"):
        rules("import gossamer.*\n\nimport scala.collection.mutable\n\nval x = 1\n")
      . assert(_.contains("302.2"))

      test(m"Missing blank between import groups is rejected"):
        rules("import scala.collection.mutable\nimport gossamer.*\n\nval x = 1\n")
      . assert(_.contains("302.3"))

      test(m"Out-of-order imports within a group are rejected"):
        rules("import zephyrine.*\nimport anticipation.*\n\nval x = 1\n")
      . assert(_.contains("302.2"))

      test(m"Missing blank line after imports is rejected"):
        rules("import gossamer.*\nval x = 1\n")
      . assert(_.contains("441"))

    suite(m"Phase 1: Per-token rules"):

      test(m"No space after comma is rejected"):
        rules("def f(a: Int,b: Int): Int = a + b\n")
      . assert(_.contains("529.2"))

      test(m"Space before comma is rejected"):
        rules("def f(a: Int , b: Int): Int = a + b\n")
      . assert(_.contains("529.1"))

      test(m"Single line with extra spaces after comma is rejected"):
        rules("val x = (1,  2)\n")
      . assert(_.contains("529.2"))

      test(m"Genuinely aligned multi-row commas are accepted"):
        // Two rows where the second token after each `,` lands at the
        // same absolute column on both lines — a real alignment run.
        rules
         ( "val a = T(1,   t\"H\",  t\"Hydrogen\")\n"
            +"val b = T(20,  t\"Ne\", t\"Neon\")\n" )
      . assert(!_.contains("529.2"))

      test(m"Fake alignment (extra spaces but cols don't match) is rejected"):
        // `val H` vs `val He` shifts the parens column, so the would-be
        // aligned columns inside don't line up across rows.
        rules
         ( "val H = T(1,   t\"H\",  t\"Hydrogen\")\n"
            +"val He = T(2,   t\"He\", t\"Helium\")\n" )
      . assert(_.contains("529.2"))

      test(m"Space inside parentheses on a single line is rejected"):
        rules("def f( a: Int ): Int = a\n")
      . assert(_.contains("402"))

      test(m"Block comment outside license header is rejected"):
        rules("/* hello */\nval x = 1\n")
      . assert(_.contains("162.1"))

      test(m"Scaladoc comment is rejected"):
        rules("/** hello */\nval x = 1\n")
      . assert(_.contains("162.2"))

      test(m"Annotation followed by blank line is rejected"):
        rules("@deprecated\n\ndef foo(): Int = 1\n")
      . assert(_.contains("551.2"))

      test(m"Param annotation does not arm 551.2 on a following blank line"):
        rules("case class P(@ident x: Int)\n\ncase class Q(y: Int)\n")
      . assert(!_.contains("551.2"))

      test(m"Annotation immediately before a decl, then a blank line, is accepted"):
        rules("@foo\ncase class A(x: Int)\n\ncase class B(y: Int)\n")
      . assert(!_.contains("551.2"))

    suite(m"Phase 2: Operator and method-name spacing"):

      test(m"Missing space before `=>` is rejected"):
        rules("val f = (x: Int)=> x + 1\n")
      . assert(_.contains("376"))

      test(m"Missing space after `=>` is rejected"):
        rules("val f = (x: Int) =>x + 1\n")
      . assert(_.contains("376"))

      test(m"Missing space around `<:` is rejected"):
        rules("def f[T<:Int](x: T): T = x\n")
      . assert(_.contains("376"))

      test(m"Asymmetric `&` spacing is rejected"):
        rules("type T = Int& String\n")
      . assert(_.contains("376"))

      test(m"Single-char `&` with no spaces is accepted"):
        rules("def f(x: Int): Int = x&255\n")
      . assert(!_.contains("376"))

      test(m"Spaced `=>` is accepted"):
        rules("val f = (x: Int) => x + 1\n")
      . assert(!_.contains("376"))

      test(m"`=>` at line start (continuation) is accepted"):
        rules("val f =\n  (x: Int) =>\n    x + 1\n")
      . assert(!_.contains("376"))

      test(m"Missing space before `=` is rejected"):
        rules("val x= 1\n")
      . assert(_.contains("376.1"))

      test(m"Missing space after `=` is rejected"):
        rules("val x =1\n")
      . assert(_.contains("376.1"))

      test(m"Two spaces *after* `=` is rejected"):
        rules("val x =  1\n")
      . assert(_.contains("376.1"))

      test(m"Two spaces before `=` (alignment) is accepted"):
        // Multi-line param blocks often pad before `=` to align the
        // operators vertically; the rule must allow this.
        rules("val x  = 1\n")
      . assert(!_.contains("376.1"))

      test(m"`name_=` setter method declaration is not flagged"):
        rules("def x_=(value: Int): Unit = ()\n")
      . assert(!_.contains("376.1"))

      test(m"Single space around `=` is accepted"):
        rules("val x = 1\n")
      . assert(!_.contains("376.1"))

      test(m"`=` at line end (multi-line RHS) is accepted"):
        rules("val x =\n  1\n")
      . assert(!_.contains("376.1"))

      test(m"Missing space around `+=` is rejected"):
        rules("var x = 0\nx+=1\n")
      . assert(_.contains("376.1"))

      test(m"Single space around `+=` is accepted"):
        rules("var x = 0\nx += 1\n")
      . assert(!_.contains("376.1"))

      test(m"Default parameter `=` with bad spacing is rejected"):
        rules("def f(x: Int=5): Int = x\n")
      . assert(_.contains("376.1"))

      test(m"Symbolic operator method without space before parens is rejected"):
        rules("infix def +(right: Int): Int = right\n")
      . assert(_.contains("013"))

      test(m"Symbolic operator method with space is accepted"):
        rules("infix def + (right: Int): Int = right\n")
      . assert(!_.contains("013"))

      test(m"`if/then/else` does not pool predicate ops with else-clause ops"):
        rules("def f(x: Int, y: Int, a: Int, b: Int): Int = if x == y then a else a^b\n")
      . assert(!_.contains("376"))

      test(m"`else if` chain does not pool ops across branches"):
        rules("def f(x: Boolean, a: Int, b: Int): Int = if x then 1 else if a == b then 2 else a^b\n")
      . assert(!_.contains("376"))

      test(m"Parenthesised `if/then/else` followed by an operator is accepted"):
        rules("def f(x: Int, a: Int, b: Int): Int = (if x == 0 then a else b) + 1\n")
      . assert(!_.contains("376"))

      test(m"`case class` modifier does not flush operator frames spuriously"):
        rules("case class Foo(x: Int, y: Int)\n")
      . assert(!_.contains("376"))

      test(m"Cross-precedence violation is still rejected"):
        rules("def f(a: Int, b: Int, c: Int): Int = a+b * c\n")
      . assert(_.contains("376"))

    suite(m"Phase 3: Continuations and shape"):

      test(m"`=>` continuation with wrong space count is rejected"):
        rules("given x: [a]\n=> a is Foo =\n  ???\n")
      . assert(_.contains("444"))

      test(m"`=>` continuation with two spaces is accepted"):
        rules("given x: [a]\n=>  a is Foo =\n\n  ???\n")
      . assert(!_.contains("444"))

      test(m"Heavy-signature `:` continuation with wrong spaces is rejected"):
        rules("def foo\n  ( x: Int )\n: Int =\n\n  x\n")
      . assert(_.contains("444"))

      test(m"Missing blank line before heavy-signature body is rejected"):
        rules("def foo\n  ( x: Int )\n:   Int =\n  x\n")
      . assert(_.contains("677"))

      test(m"Heavy-signature with blank line before body is accepted"):
        rules("def foo\n  ( x: Int )\n:   Int =\n\n  x\n")
      . assert(!_.contains("677"))

      test(m"Chain `. method` with same-indent preceded by blank is rejected"):
        rules("val a = foo\n\n. method\n")
      . assert(_.contains("163.2"))

      test(m"Chain `. method` after deeper line without blank is rejected"):
        rules("val a = foo:\n  bar\n. method\n")
      . assert(_.contains("163.1"))

    suite(m"Phase 4: Cross-cutting"):

      test(m"Companion object after class is rejected"):
        rules("class Foo:\n  val x = 1\n\nobject Foo:\n  val y = 2\n")
      . assert(_.contains("398"))

      test(m"Companion object before class is accepted"):
        rules("object Foo:\n  val y = 2\n\nclass Foo:\n  val x = 1\n")
      . assert(!_.contains("398"))

    suite(m"Phase 3: Match-case rules"):

      test(m"Misaligned `=>` in case run is rejected"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Short      => 1\n"
            +"  case LongerName => 2\n"
            +"  case Med => 3\n" )
      . assert(_.contains("326"))

      test(m"Aligned `=>` in case run is accepted"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Short      => 1\n"
            +"  case LongerName => 2\n"
            +"  case Medium     => 3\n" )
      . assert(!_.contains("326"))

      test(m"Multi-line case without preceding blank line is rejected"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Foo => 1\n"
            +"  case Bar =>\n"
            +"    bigBody\n" )
      . assert(_.contains("315"))

      test(m"Multi-line case as first case (after `match`) is accepted"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Bar =>\n"
            +"    bigBody\n" )
      . assert(!_.contains("315"))

      test(m"Multi-line case after blank is accepted"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Foo => 1\n"
            +"\n"
            +"  case Bar =>\n"
            +"    bigBody\n" )
      . assert(!_.contains("315"))

      test(m"Operator-continuation line does not trigger SN-315"):
        // Dotty may split `t == 1\n|| t == 2` into two `Block.stats` at
        // the parser level, but inserting a blank line between them
        // would break the operator continuation. The rule must skip
        // when `cur.startLine` begins with an infix operator.
        rules
         ( "def f(c: Int): Boolean =\n"
            +"  val t = c\n"
            +"\n"
            +"  t == 1\n"
            +"  || t == 2\n"
            +"  || t == 3\n" )
      . assert(!_.contains("315"))

      test(m"Multi-line pattern with trailing `=>` is accepted"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Foo\n"
            +"    ( a, b ) =>\n"
            +"    body\n" )
      . assert(!_.contains("R33-multiline-pattern-arrow-position"))

      test(m"Multi-line pattern with `=>` alone on its own line is rejected"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Foo\n"
            +"    ( a, b )\n"
            +"  =>\n"
            +"    body\n" )
      . assert(_.contains("R33-multiline-pattern-arrow-position"))

      test(m"Multi-line pattern with body on same line as `=>` is rejected"):
        rules
         ( "def f(x: Any): Any = x match\n"
            +"  case Foo\n"
            +"    ( a, b ) => body\n" )
      . assert(_.contains("R33-multiline-pattern-body-newline"))

    suite(m"Phase 3: Sibling padding and using alignment"):

      test(m"Adjacent multi-line defs without blank line is rejected"):
        rules("def a: Int =\n  1\ndef b: Int =\n  2\n")
      . assert(_.contains("315"))

      test(m"Adjacent multi-line defs with blank line is accepted"):
        rules("def a: Int =\n  1\n\ndef b: Int =\n  2\n")
      . assert(!_.contains("315"))

      test(m"First definition in a new indented scope is accepted"):
        rules("class Foo:\n  def a: Int =\n    1\n\n  def b: Int =\n    2\n")
      . assert(!_.contains("315"))

      test(m"Same-indent decls in unrelated scopes skip sibling check"):
        rules("class A:\n  def a: Int = 1\n\nclass B:\n  def b: Int = 2\n")
      . assert(!_.contains("315"))

      test(m"Misaligned param after `using` clause first row is rejected"):
        rules("def f\n  ( using a: A,\n        b: B )\n:   Int = 0\n")
      . assert(_.contains("946"))

      test(m"Aligned `using` clause is accepted"):
        rules("def f\n  ( using a: A,\n          b: B )\n:   Int = 0\n")
      . assert(!_.contains("946"))

      test(m"Same-line type-annotation `:` is accepted"):
        rules("def foo: Int = 1\n")
      . assert(!_.contains("833.3"))

      test(m"Type-annotation `:` aligned with `def` on its own line is accepted"):
        rules("def foo\n:   Int =\n  1\n")
      . assert(!_.contains("833.3"))

      test(m"Mis-indented type-annotation `:` on its own line is rejected"):
        rules("def foo\n  : Int =\n    1\n")
      . assert(_.contains("833.3"))

      test(m"`:` inside a parameter list is not flagged by 833.3"):
        rules("def f\n  ( a: Int )\n:   Int =\n  a\n")
      . assert(!_.contains("833.3"))

      test(m"Mis-indented `:` after multi-line param list with type params is rejected"):
        rules
         ( "def f[a, b]\n  ( x: Int,\n    y: Int )\n        :   Int =\n  0\n" )
      . assert(_.contains("833.3"))

      test(m"Mis-indented `:` after param list containing a default value is rejected"):
        rules
         ( "def f\n  ( x: Int = 0,\n    y: Int = 1 )\n        :   Int =\n  0\n" )
      . assert(_.contains("833.3"))

    suite(m"Phase 5: Keyword sequences (R33)"):

      test(m"Compact `if/then/else` is accepted"):
        rules("def f(x: Int): Int = if x > 0 then x else -x\n")
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"`if/then/else` with `else` on a new line is accepted"):
        rules("def f(x: Int): Int =\n  if x > 0 then x\n  else -x\n")
      . assert(!_.contains("833.1"))

      test(m"`if/then/else` fully split with aligned keywords is accepted"):
        rules("def f(x: Int): Int =\n  if x > 0\n  then x\n  else -x\n")
      . assert(r => !r.contains("833.1"))

      test(m"`if/then/else` with `then` broken but `else` not is rejected"):
        rules("def f(x: Int): Int =\n  if x > 0\n  then x else -x\n")
      . assert(_.contains("833.1"))

      test(m"`if/then/else` with `then` not aligned with `if` is rejected"):
        rules("def f(x: Int): Int =\n  if x > 0\n      then x\n      else -x\n")
      . assert(_.contains("833.1"))

      test(m"`if/then/else` with `then` indented, `else` inline is rejected"):
        rules("def f(x: Int): Int =\n  if x > 0 then\n    x\n  else -x\n")
      . assert(_.contains("833.2"))

      test(m"`if/then/else` with both bodies indented is accepted"):
        rules("def f(x: Int): Int =\n  if x > 0 then\n    x\n  else\n    -x\n")
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"Compact `else if` chain is accepted"):
        rules("def f(x: Int): Int = if x > 0 then 1 else if x < 0 then -1 else 0\n")
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"`else if` with broken keywords and inline bodies is accepted"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then 1\n"
            +"  else if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"`else if` chain with all bodies indented is accepted"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then\n"
            +"    1\n"
            +"  else if x < 0 then\n"
            +"    -1\n"
            +"  else\n"
            +"    0\n" )
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"`else if` chain: indented then-body, inline tail-body rejected"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then\n"
            +"    1\n"
            +"  else if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(_.contains("833.2"))

      test(m"`else if` chain misaligned with original `if` is rejected"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then 1\n"
            +"    else if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(_.contains("833.1"))

      test(m"`else` followed by indented inner `if` starts a new anchor"):
        // Newline + indentation between `else` and the next `if` means
        // the inner `if` is not a bridge: the outer chain ends at
        // `else`, and the inner `if` starts a fresh chain whose
        // `then`/`else` align with the inner `if`'s column.
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then x\n"
            +"  else\n"
            +"    if x < 0 then -x\n"
            +"    else 0\n" )
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"Inner `if` as new anchor: misaligned inner `then` is rejected"):
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0 then x\n"
            +"  else\n"
            +"    if x < 0\n"
            +"        then -x\n"
            +"    else 0\n" )
      . assert(_.contains("833.1"))

      test(m"Bridge's internal `then` is part of the unit (not checked)"):
        // The inner `then` of an `else if … then` bridge belongs to
        // the bridge unit. Even though it sits at a column far from
        // the outer `if`'s anchor, the placement rule does not apply
        // to it; only the bridge's leading `else` is checked.
        rules
         ( "def f(x: Int): Int =\n"
            +"  if x > 0\n"
            +"  then 1\n"
            +"  else if x < 0\n"
            +"       then -1\n"
            +"  else 0\n" )
      . assert(r => !r.contains("833.1"))

      test(m"Inner then-only `if` doesn't steal the outer chain's `else`"):
        // The inner `if char >= 0 then ...` (no else) sits inside the
        // outer `then`-body. Without indentation-aware else-matching,
        // findKeyword would absorb the outer `else if esc` as the
        // inner `if`'s else, producing a spurious 833.1 misalignment.
        rules
         ( "def recur(text: String, esc: Boolean): Unit =\n"
            +"  if 1 < text.length\n"
            +"  then\n"
            +"    val char = text.charAt(0)\n"
            +"    if char >= 0 then println(char.toChar)\n"
            +"    recur(text, esc)\n"
            +"  else if esc then throw new RuntimeException(\"\")\n" )
      . assert(r => !r.contains("833.1"))

      test(m"Case-guard `if` in pattern match isn't processed as if-chain"):
        // `case x if guard => …` looks like an `if` with no `then` to
        // findKeyword unless we bail on `=>` at depth 0. Without the
        // bail, the case-guard `if` would walk forward and pair with
        // an unrelated `then` later in the file, producing spurious
        // 833.1 violations against an `if` column nowhere near the
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
      . assert(r => !r.contains("833.1"))

      test(m"For-comprehension filter `if` isn't processed as if-chain"):
        // `if filter` inside a for-comprehension has no `then`. Without
        // bailing on `yield`/`do`/`<-`, findKeyword would walk past the
        // filter and pair with an unrelated `then` later in the file.
        rules
         ( "def f(xs: List[Int]): List[Int] = for x <- xs if x > 0 yield x\n"
            +"\n"
            +"def g(y: Int): Int =\n"
            +"  if y > 0 then y\n"
            +"  else -y\n" )
      . assert(r => !r.contains("833.1"))

      test(m"Compact `while/do` is accepted"):
        rules("def f(): Unit = while running() do step()\n")
      . assert(r => !r.contains("833.1"))

      test(m"`while/do` with `do` misaligned with `while` is rejected"):
        rules("def f(): Unit =\n  while running()\n      do step()\n")
      . assert(_.contains("833.1"))

      test(m"Compact `try/catch/finally` is accepted"):
        rules("def f: Int = try compute() catch case e: Throwable => 0 finally cleanup()\n")
      . assert(r => !r.contains("833.1"))

      test(m"`try/catch/finally`: broken `catch`, inline `finally` rejected"):
        rules("def f: Int =\n  try compute()\n  catch case e: Throwable => 0 finally cleanup()\n")
      . assert(_.contains("833.1"))

      test(m"`try`/`finally` without `catch` is accepted (compact)"):
        rules("def f(): Unit =\n  try block\n  finally cleanup()\n")
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"`inline if` aligns broken `then`/`else` with `inline`, not `if`"):
        // The `if` after `inline` starts mid-line, but visually the
        // construct begins at `inline`. A broken `then`/`else` aligned
        // with `inline` should be accepted.
        rules
         ( "inline def f(x: Int): Int =\n"
            +"  inline if x > 0\n"
            +"  then x\n"
            +"  else -x\n" )
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"`else inline if … then` is recognised as a chain bridge"):
        // The `else` is followed by `inline if` (modifier + `if`) on
        // the same line; this should fold into a single bridge rather
        // than be treated as a final `else` whose body happens to be
        // an `if`.
        rules
         ( "inline def f(x: Int): Int =\n"
            +"  inline if x > 0 then 1\n"
            +"  else inline if x < 0 then -1\n"
            +"  else 0\n" )
      . assert(r => !r.contains("833.1") && !r.contains("833.2"))

      test(m"Inline inner `else` in bridge doesn't fire 833.1"):
        // The bridge's inner `if/then` brings its own inline `else`;
        // that inner `else` belongs to the bridge, not to the outer
        // chain, so it must not be appended as the chain's K_(i+1).
        rules
         ( "inline def f(x: Int): Int =\n"
            +"  inline if x > 0 then inline if x == 1 then 1 else 2\n"
            +"  else inline if x == -1 then -1 else -2\n" )
      . assert(r => !r.contains("833.1"))

      test(m"`try`/`catch` without `finally` won't steal sibling's `finally`"):
        // The first def's `try`/`catch` has no `finally`; the second
        // def's `try foo finally bar` has its own. Without a dedent
        // bail in `findKeyword`, the first `try` would walk past the
        // function boundary, pair with the second def's `finally`,
        // and produce a 833.2 violation against the inline body of
        // that stolen `finally`.
        rules
         ( "def safely(): Unit =\n"
            +"  try\n"
            +"    val v = compute()\n"
            +"    process(v)\n"
            +"  catch\n"
            +"    case e: Exception => log(e)\n"
            +"    case e: Throwable => panic(e)\n"
            +"\n"
            +"def focus(): Unit =\n"
            +"  try block\n"
            +"  finally cleanup()\n" )
      . assert(r => !r.contains("833.2"))

      test(m"Compact `for/yield` is accepted"):
        rules("def f: List[Int] = for x <- List(1, 2) yield x\n")
      . assert(r => !r.contains("833.1"))

      test(m"Compact `for/do` is accepted"):
        rules("def f(): Unit = for x <- List(1, 2) do println(x)\n")
      . assert(r => !r.contains("833.1"))

    suite(m"Phase 5: For-comprehension generator alignment (R34)"):

      test(m"For-comprehension aligned-LHS style is accepted"):
        rules("val r =\n  for x <- List(1)\n      y <- List(2)\n  yield x + y\n")
      . assert(r => !r.contains("924.1") && !r.contains("924.2") && !r.contains("924.3"))

      test(m"For-comprehension indented-block style is accepted"):
        rules("val r =\n  for\n    x <- List(1)\n    y <- List(2)\n  yield x + y\n")
      . assert(r => !r.contains("924.1") && !r.contains("924.2") && !r.contains("924.3"))

      test(m"For-comprehension with misaligned generator LHS is rejected"):
        rules("val r =\n  for x <- List(1)\n     y <- List(2)\n  yield x + y\n")
      . assert(_.contains("924.2"))

      test(m"For-comprehension with misaligned `<-` operators is rejected"):
        rules("val r =\n  for x  <- List(1)\n      y <- List(2)\n  yield x + y\n")
      . assert(_.contains("924.1"))

      test(m"For-comprehension with `if` filter aligned with `<-` is accepted"):
        rules
         ( "val r =\n  for x  <- List(1)\n         if x > 0\n      y  <- List(2)"
            +"\n  yield x + y\n" )
      . assert(r => !r.contains("924.3"))

      test(m"For-comprehension with `if` filter at LHS column is rejected"):
        rules
         ( "val r =\n  for x  <- List(1)\n      if x > 0\n      y  <- List(2)"
            +"\n  yield x + y\n" )
      . assert(_.contains("924.3"))

      test(m"For-comprehension with `if` on same line as `<-` is accepted"):
        rules("val r =\n  for x <- List(1) if x > 0\n  yield x\n")
      . assert(r => !r.contains("924.3") && !r.contains("924.4"))

      test(m"For-comprehension with blank line before `if` is rejected"):
        rules
         ( "val r =\n  for x  <- List(1)\n\n         if x > 0\n      y  <- List(2)"
            +"\n  yield x + y\n" )
      . assert(_.contains("924.4"))

      test(m"For-comprehension aligned `if` filter is exempt from 473.1"):
        rules
         ( "val r =\n  for x  <- List(1)\n         if x > 0\n      y  <- List(2)"
            +"\n  yield x + y\n" )
      . assert(r => !r.contains("473.1"))

      test(m"Inline term quote with inside space is rejected"):
        rules("val q = '{ x }\n")
      . assert(_.contains("473.7"))

      test(m"Inline term splice with inside space is rejected"):
        rules("val s = ${ x }\n")
      . assert(_.contains("473.7"))

      test(m"Inline type quote with inside space is rejected"):
        rules("val t = '[ Int ]\n")
      . assert(_.contains("473.7"))

      test(m"Tight inline quote is accepted"):
        rules("val q = '{x}\n")
      . assert(r => !r.contains("473.7"))

      test(m"Tight inline splice is accepted"):
        rules("val s = ${x}\n")
      . assert(r => !r.contains("473.7"))

      test(m"Tight inline type quote is accepted"):
        rules("val t = '[Int]\n")
      . assert(r => !r.contains("473.7"))

      test(m"Multi-line quote opener is not flagged as inline"):
        rules("val q =\n  ' {\n    body\n  }\n")
      . assert(r => !r.contains("473.7"))

    suite(m"Phase 6: Indented-scope body indent (Rule B)"):

      test(m"Colon-block body at anchor+2 is accepted"):
        rules("val foo: Int = bar:\n  baz\n")
      . assert(r => !r.contains("473.8"))

      test(m"Colon-lambda body at anchor+2 is accepted"):
        rules("val xs = items.map: x =>\n  f(x)\n")
      . assert(r => !r.contains("473.8"))

      test(m"Nested match on a case line is accepted"):
        rules("foo match\n  case Bar(b) => b match\n    case Q => q\n")
      . assert(r => !r.contains("473.8"))

      test(m"Tuple as = RHS is accepted"):
        rules("val pair: (Int, Int) =\n  (a, b)\n")
      . assert(r => !r.contains("473.8"))

      test(m"= RHS receiver on its own line with arg block is accepted"):
        rules("val foo: Bar =\n  Bar\n    ( baz, quux )\n")
      . assert(r => !r.contains("473.8"))

      test(m"Colon-block body too deep is rejected"):
        rules("val foo: Int = bar:\n      baz\n")
      . assert(_.contains("473.8"))

      test(m"Multi-line signature with = not last is rejected"):
        rules("def fn(a: A)\n: Quux = quux:\n  body\n")
      . assert(_.contains("473.9"))

      test(m"Multi-line signature with = last is accepted"):
        rules("def fn(a: A)\n: Quux =\n  quux\n")
      . assert(r => !r.contains("473.9"))

      test(m"Param block under a val is owned by 833.4, not Rule B"):
        rules("val foo: Bar = Bar\n  ( baz, quux )\n")
      . assert(r => !r.contains("473.8"))

      test(m"Lambda parameter list is not a heavy continuation (833.4)"):
        rules("foo +\n  (data: Origin) =>\n    body\n")
      . assert(r => !r.contains("833.4"))

      test(m"Genuine heavy bracket continuation is still rejected (833.4)"):
        rules("foo +\n  ( baz, quux )\n")
      . assert(r => r.contains("833.4"))

      test(m"Tuple as the first line inside a quote/block is not 833.4"):
        rules("' {\n  ( baz, quux )\n")
      . assert(r => !r.contains("833.4"))

      test(m"Constructor params after an `into` class are not 833.4"):
        rules("into case class Response private\n  ( a: A, b: B )\n")
      . assert(r => !r.contains("833.4"))

      test(m"if-sequence as RHS keeps its own anchor (833.1), not Rule B"):
        rules("val x = if pred\nthen y\nelse z\n")
      . assert(r => r.contains("833.1") && !r.contains("473.8"))

      test(m"Chain-method colon-lambda body (+4) is not flagged"):
        rules("foo\n  .map: x =>\n    body\n")
      . assert(r => !r.contains("473.8"))

      test(m"String-interpolator first body line is handled"):
        rules("val x = bar:\n  t\"a\"+b\n  more\n")
      . assert(r => !r.contains("473.8"))
