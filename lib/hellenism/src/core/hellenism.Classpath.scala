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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package hellenism

import java.net as jn

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import serpentine.*
import turbulence.*
import vacuous.*

object Classpath:
  type Rules = MustNotContain["/"]

  given substantiable: (classloader: Classloader) => (Path on Classpath) is Substantiable =
    path => classloader.java.getResourceAsStream(path.text.s) != null

  @targetName("child")
  infix def / (child: Text)(using classloader: Classloader): Path on Classpath raises NameError =
    Path(classloader, List(child))

  def apply(classloader: jn.URLClassLoader): Classpath =
    val entries = classloader.getURLs.nn.to(List).map(_.nn).flatMap(ClasspathEntry(_).option)

    if entries.exists:
      case _: ClasspathEntry.Url => true
      case _                     => false
    then OnlineClasspath(entries)
    else LocalClasspath:
      entries.collect:
        case directory: ClasspathEntry.Directory      => directory
        case jar: ClasspathEntry.Jar                  => jar
        case runtime: ClasspathEntry.JavaRuntime.type => runtime

  given readableBytes: (Tactic[ClasspathError], Classpath is Radical from Classloader)
        => (Path on Classpath) is Readable by Bytes =
    unsafely:
      Readable.inputStream.contramap: resource =>
        resource.root.inputStream(resource.text)

  given navigable: Classpath is Navigable by Text under Rules =
    new Navigable:
      type Self = Classpath
      type Operand = Text
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t".."

      def elementText(element: Text): Text = element
      def element(text: Text): Text = text
      def caseSensitivity: Case = Case.Sensitive

  given radical: Classloader => Classpath is Radical from Classloader = new Radical:
    type Self = Classpath
    type Source = Classloader

    def root(path: Text): Classloader = summon[Classloader]
    def rootLength(path: Text): Int = 0
    def rootText(root: Classloader): Text = t""

trait Classpath:
  def entries: List[ClasspathEntry]
