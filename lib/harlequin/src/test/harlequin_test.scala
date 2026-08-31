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
package harlequin

import scala.language.dynamics

import soundness.*

// The `soundness` wildcard publishes `honeycomb.Fragment`, which outranks this package's own
// `Fragment` from another file; the by-name import restores it.
import harlequin.Fragment

import ambience.systems.javaSystem
import denominative.dysasymptotics.linearSize

// A `Dynamic` type with a `Completable` companion, exercising the dynamic-completions route: its
// valid member names are not symbols, so only the companion can offer them. The companion is
// found from the receiver's type at the caret and loaded reflectively from this test module's
// own classes.
object Creature extends Completable:
  def completions(using quotes: scala.quoted.Quotes)
    ( receiver: quotes.reflect.TypeRepr, prefix: Text )
  :   List[prophesy.Completion] =

    import quotes.reflect.*

    proscenium.List(t"habitat", t"diet").map: name =>
      prophesy.Completion(name, prophesy.Completion.Kind.Term, Syntax(TypeRepr.of[Text]))

trait Creature extends Dynamic:
  def selectDynamic(name: String): Text = t""

object Tests extends Suite(m"Harlequin Tests"):
  def run(): Unit =
    val snippet = t"val xs = List(1, 2, 3)"

    def typeOf(tokens: List[Token], text: Text): Optional[Text] =
      tokens.seek(_.text == text).let: token =>
        token.meta.let(_.tpe.qualified)

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings themselves.
    suite(m"Native-rendering coverage"):
      test(m"harlequin's types inspect natively"):
        Inspectable.fallbacks
         ( Token(t"val", Accent.Keyword).inspect,
           Token(t"xs", Accent.Term, role = Role.Binding).inspect )
      . assert(_ == Nil)

      test(m"a token inspects with all of its state"):
        Token(t"val", Accent.Keyword).inspect
      . assert(_ == Text("Token(text:t\"val\" ╱ accent:Keyword ╱ meta:○ ╱ span:⟪∅⟫ ╱ role:○)"))

    test(m"tokenized highlighting attaches no type metadata"):
      Scala.highlight(snippet).lines.to[List].stdlib.flatMap(_.stdlib).flatMap(_.meta.option)
    .assert(_ == Nil)

    test(m"each token carries its line and column position"):
      val tokens = Scala.highlight(t"val n =\n  List").lines.to[List].stdlib.flatMap(_.stdlib)

      tokens.map: token =>
        (token.text, token.span.startLine.lay(-1)(_.n0), token.span.startColumn.lay(-1)(_.n0))
    .assert(_.has((t"List", 1, 2)))

    // The binding/usage tagging runs on the parser output alone, so it is
    // exercised by the tokenized (no-typer) path. `tagOf` reads the accent and role (as
    // their rendered names) of the first token whose text matches.
    def tagOf(source: Text, word: Text): (Text, Text) =
      Scala.highlight(source).lines.to[List].stdlib.flatMap(_.stdlib).find(_.text == word) match
        case Some(token) => (token.accent.show, token.role.lay(t"")(_.show))
        case None        => (t"", t"")

    test(m"a val name is a term binding"):
      tagOf(t"val alpha = 1", t"alpha")
    .assert(_ == (t"term", t"binding"))

    test(m"a term usage is a term reference"):
      tagOf(t"val beta = gamma", t"gamma")
    .assert(_ == (t"term", t"usage"))

    test(m"a type usage is a typal usage"):
      tagOf(t"val n: List = xs", t"List")
    .assert(_ == (t"typal", t"usage"))

    test(m"a tuple-pattern val binds each name as a term"):
      val source = t"val (aa, bb) = pair"
      (tagOf(source, t"aa"), tagOf(source, t"bb"))
    .assert(_ == ((t"term", t"binding"), (t"term", t"binding")))

    test(m"a case-clause pattern binder is a term binding"):
      tagOf(t"val qq = nums.map { case xx => xx }", t"xx")
    .assert(_ == (t"term", t"binding"))

    test(m"a for-comprehension binder is a term binding"):
      val source = t"val qq = for cc <- items yield cc"
      (tagOf(source, t"cc"), tagOf(source, t"items"))
    .assert(_ == ((t"term", t"binding"), (t"term", t"usage")))

    test(m"a defined type parameter is a typal binding"):
      tagOf(t"def gg[TT](vv: TT) = vv", t"TT")
    .assert(_ == (t"typal", t"binding"))

    test(m"a value parameter is a term binding"):
      tagOf(t"def gg[TT](vv: TT) = vv", t"vv")
    .assert(_ == (t"term", t"binding"))

    test(m"typechecked highlighting resolves the type of a val"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      typeOf(Scala.highlight(snippet).lines.to[List].stdlib.flatMap(_.stdlib).to(List), t"xs").or(t"")
    .assert { rendered => rendered.subsumes(t"List") && rendered.subsumes(t"Int") }

    test(m"typechecked highlighting reports diagnostics for ill-typed code"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      Scala.highlight(t"val n: Int = \"oops\"").diagnostics.size
    .assert(_ > 0)

    test(m"no completions are computed without a caret"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      Scala.highlight(snippet).completions
    .assert(_ == Unset)

    test(m"a bare type position completes in-scope types"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source = t"val x: Li"
      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name))
    .assert(_.has(t"List"))

    test(m"a type application completes in-scope types"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source = t"val x = collection.mutable.Map[Li"
      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name))
    .assert(_.has(t"List"))

    test(m"a bare term position completes in-scope names"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source = t"val x = Li"
      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name))
    .assert(_.has(t"List"))

    test(m"completions at a member selection include the type's methods"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source = t"val xs = List(1, 2, 3)\nval y = xs.m"
      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name)).stdlib
    .assert(_.has(t"map"))

    test(m"a Dynamic receiver completes through its Completable companion"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source =
        t"val creature: harlequin.Creature = new harlequin.Creature {}\nval x = creature.ha"

      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name))
    .assert(_.has(t"habitat"))

    test(m"dynamic completions are filtered by the partial member name"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source =
        t"val creature: harlequin.Creature = new harlequin.Creature {}\nval x = creature.ha"

      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name))
    .assert(!_.has(t"diet"))

    // Fragment analysis is pure text+lexer work — no compiler givens — so a completion host
    // can split its input before deciding whether to invoke the typechecker at all.
    suite(m"Fragment analysis"):
      test(m"a bare identifier has no member base"):
        Fragment.memberBase(t"leng", 4)
      . assert(_ == (Unset, t"leng"))

      test(m"a member selection splits at the dot"):
        Fragment.memberBase(t"text.le", 7)
      . assert(_ == (t"text.", t"le"))

      test(m"a chained call keeps the whole receiver"):
        Fragment.memberBase(t"foo.bar(baz).qu", 15)
      . assert(_ == (t"foo.bar(baz).", t"qu"))

      test(m"the partial ends at the cursor, not the end of input"):
        Fragment.memberBase(t"text.length", 7)
      . assert(_ == (t"text.", t"le"))

      test(m"a value followed by space is an infix receiver"):
        Fragment.infixBase(t"xs ma", 5)
      . assert(_ == (t"xs.", t"ma"))

      test(m"a bracketed receiver is scanned as a balanced group"):
        Fragment.infixBase(t"foo(bar, baz) ma", 16)
      . assert(_ == (t"foo(bar, baz).", t"ma"))

      test(m"a keyword before the space is not a receiver"):
        Fragment.infixBase(t"if co", 5)
      . assert(_ == (Unset, t"co"))

      test(m"a soft keyword before the space is not a receiver"):
        Fragment.infixBase(t"inline de", 9)
      . assert(_ == (Unset, t"de"))

      test(m"a binding name after val is not a receiver"):
        Fragment.infixBase(t"val xs ma", 9)
      . assert(_ == (Unset, t"ma"))

      test(m"an operator before the space is not a receiver"):
        Fragment.infixBase(t"x + le", 6)
      . assert(_ == (Unset, t"le"))

      test(m"an expression start scans back over selections and brackets"):
        Fragment.expressionStart(t"foo(bar).baz")
      . assert(_ == 0)

      test(m"an expression start stops at an operator"):
        Fragment.expressionStart(t"a + foo.bar")
      . assert(_ == 4)

    // Keyword completions come from prophesy's curated pattern tree over the reversed lexeme
    // context at the caret; tokenized depth suffices, so no compiler givens are needed.
    suite(m"Keyword completions"):
      def keywordsAt(source: Text): List[Text] =
        Scala.highlight(source, caret = source.length.z).completions
        . lay(Nil)(_.items.map(_.name))

      test(m"a partial identifier at the start of input completes to keywords"):
        keywordsAt(t"va")
      . assert(_ == List(t"val", t"var"))

      test(m"imp completes to import, and implicit is never offered"):
        keywordsAt(t"imp")
      . assert(_ == List(t"import"))

      test(m"transparent offers inline and trait"):
        keywordsAt(t"transparent ")
      . assert { words => words.has(t"inline") && words.has(t"trait") }

      test(m"transparent inline unambiguously offers definitions"):
        keywordsAt(t"transparent inline ")
      . assert(_ == List(t"def", t"given"))

      test(m"a definition's parameter list offers using"):
        keywordsAt(t"def f(")
      . assert(_.has(t"using"))

      test(m"a call's argument list offers expressions, not definitions"):
        val words = keywordsAt(t"foo(")
        (words.has(t"new"), words.has(t"val"))
      . assert(_ == (true, false))

      test(m"a member selection offers no statement keywords"):
        keywordsAt(t"foo.")
      . assert(!_.has(t"val"))

      test(m"a fresh binding position suppresses all completions"):
        Scala.highlight(t"val ", caret = t"val ".size.z).completions.let(_.items.size)
      . assert(_ == 0)

      test(m"a new statement line offers statement keywords"):
        keywordsAt(t"val x = 1\nva")
      . assert(_ == List(t"val", t"var"))

      test(m"an indented continuation after = is an expression position"):
        keywordsAt(t"val x =\n  ")
      . assert(_.has(t"new"))

      test(m"a value on the same line offers match"):
        keywordsAt(t"xs ")
      . assert(_.has(t"match"))

      test(m"an if condition is followed by then"):
        keywordsAt(t"if x ")
      . assert(_.has(t"then"))

      test(m"match is followed by case"):
        keywordsAt(t"xs match ")
      . assert(_ == List(t"case"))

      test(m"a context bound offers no keywords"):
        keywordsAt(t"def fn[T: ")
      . assert(_ == Nil)

      test(m"a parameter type ascription offers no keywords"):
        keywordsAt(t"def f(x: ")
      . assert(_ == Nil)

      test(m"a val type ascription offers no keywords"):
        keywordsAt(t"val x: ")
      . assert(_ == Nil)

      test(m"an indented template body after a colon offers definitions"):
        keywordsAt(t"class Foo:\n  va")
      . assert(_ == List(t"val", t"var"))

      test(m"an operator continues an expression"):
        keywordsAt(t"val x = 1 + ")
      . assert { words => words.has(t"new") && !words.has(t"val") }
