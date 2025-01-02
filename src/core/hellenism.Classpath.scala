/*
    Hellenism, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hellenism

import java.net as jn

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

object Classpath:
  type Rules = MustNotContain["/"]

  given (using classloader: Classloader) => (Path on Classpath) is Substantiable =
    path => classloader.java.getResourceAsStream(path.text.s) != null

  @targetName("child")
  infix def / (child: Name[Classpath])(using classloader: Classloader)
          : Path on Classpath raises NameError =
    Path(classloader, List(child))

  def apply(classloader: jn.URLClassLoader): Classpath =
    val entries = classloader.let(_.getURLs.nn.to(List)).or(Nil).map(_.nn).flatMap(ClasspathEntry(_).option)

    if entries.exists:
      case _: ClasspathEntry.Url => true
      case _                     => false
    then OnlineClasspath(entries)
    else LocalClasspath:
      entries.collect:
        case directory: ClasspathEntry.Directory      => directory
        case jar: ClasspathEntry.Jar                  => jar
        case runtime: ClasspathEntry.JavaRuntime.type => runtime

  given (using Tactic[ClasspathError], Classpath is Radical from Classloader)
      => (Path on Classpath) is Readable by Bytes as readableBytes =
    unsafely:
      Readable.inputStream.contramap: resource =>
        resource.root.inputStream(resource.text)

  given (using Tactic[NameError]) => Classpath is Navigable by Name[Classpath] under Rules =
    new Navigable:
      type Self = Classpath
      type Operand = Name[Classpath]
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t".."

      def elementText(element: Name[Classpath]): Text = element.text
      def element(text: Text): Name[Classpath] = Name(text)
      def caseSensitivity: Case = Case.Sensitive

  given (using Classloader) => Classpath is Radical from Classloader = new Radical:
    type Self = Classpath
    type Source = Classloader

    def root(path: Text): Classloader = summon[Classloader]
    def rootLength(path: Text): Int = 0
    def rootText(root: Classloader): Text = t""

trait Classpath:
  def entries: List[ClasspathEntry]
