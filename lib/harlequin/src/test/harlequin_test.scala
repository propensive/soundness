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
package harlequin

import soundness.*

import ambience.systems.java

object Tests extends Suite(m"Harlequin Tests"):
  def run(): Unit =
    val snippet = t"val xs = List(1, 2, 3)"

    def typeOf(tokens: List[Token], text: Text): Optional[Text] =
      tokens.find(_.text == text) match
        case Some(Token(_, _, meta, _)) => meta.let(_.tpe.qualified)
        case _                          => Unset

    test(m"tokenized highlighting attaches no type metadata"):
      Scala.highlight(snippet).lines.to(List).flatten.flatMap(_.meta.option)
    .assert(_ == Nil)

    test(m"each token carries its line and column position"):
      val tokens = Scala.highlight(t"val n =\n  List").lines.to(List).flatten

      tokens.map: token =>
        (token.text, token.span.startLine.lay(-1)(_.n0), token.span.startColumn.lay(-1)(_.n0))
    .assert(_.contains((t"List", 1, 2)))

    test(m"typechecked highlighting resolves the type of a val"):
      given Scalac[3.8] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().decode[LocalClasspath])
      import highlighting.typecheckedScala

      typeOf(Scala.highlight(snippet).lines.to(List).flatten, t"xs").or(t"")
    .assert { rendered => rendered.contains(t"List") && rendered.contains(t"Int") }

    test(m"typechecked highlighting reports diagnostics for ill-typed code"):
      given Scalac[3.8] = Scalac[3.8](Nil)
      given LocalClasspath = unsafely(System.properties.java.`class`.path().decode[LocalClasspath])
      import highlighting.typecheckedScala

      Scala.highlight(t"val n: Int = \"oops\"").diagnostics.length
    .assert(_ > 0)
