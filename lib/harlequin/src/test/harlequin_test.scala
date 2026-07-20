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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import soundness.*

import proscenium.compat.*

import ambience.systems.javaSystem

object Tests extends Suite(m"Harlequin Tests"):
  def run(): Unit =
    val snippet = t"val xs = List(1, 2, 3)"

    def typeOf(tokens: List[Token], text: Text): Optional[Text] =
      tokens.find(_.text == text) match
        case Some(Token(_, _, meta, _, _)) => meta.let(_.tpe.qualified)
        case _                          => Unset

    test(m"tokenized highlighting attaches no type metadata"):
      Scala.highlight(snippet).lines.to(List).stdlib.flatMap(_.stdlib).flatMap(_.meta.option)
    .assert(_ == Nil)

    test(m"each token carries its line and column position"):
      val tokens = Scala.highlight(t"val n =\n  List").lines.to(List).stdlib.flatMap(_.stdlib)

      tokens.map: token =>
        (token.text, token.span.startLine.lay(-1)(_.n0), token.span.startColumn.lay(-1)(_.n0))
    .assert(_.contains((t"List", 1, 2)))

    // The binding/usage tagging runs on the parser output alone, so it is
    // exercised by the tokenized (no-typer) path. `tagOf` reads the accent and role (as
    // their rendered names) of the first token whose text matches.
    def tagOf(source: Text, word: Text): (Text, Text) =
      Scala.highlight(source).lines.to(List).stdlib.flatMap(_.stdlib).find(_.text == word) match
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

      typeOf(List.of(Scala.highlight(snippet).lines.to(List).stdlib.flatMap(_.stdlib)), t"xs").or(t"")
    .assert { rendered => rendered.contains(t"List") && rendered.contains(t"Int") }

    test(m"typechecked highlighting reports diagnostics for ill-typed code"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      Scala.highlight(t"val n: Int = \"oops\"").diagnostics.length
    .assert(_ > 0)

    test(m"no completions are computed without a caret"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      Scala.highlight(snippet).completions
    .assert(_ == Unset)

    test(m"completions at a member selection include the type's methods"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      val source = t"val xs = List(1, 2, 3)\nval y = xs.m"
      Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items.map(_.name)).stdlib
    .assert(_.contains(t"map"))
