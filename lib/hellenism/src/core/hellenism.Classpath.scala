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
package hellenism

import scala.caps

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import zephyrine.*

object Classpath extends Root(t""):
  type Plane = Classpath
  type Rules = MustNotContain["/"] & MustNotMatch["[0-9].*"] & MustMatch["[a-zA-Z0-9_$.]+"]

  inline given nominative: Classpath is Nominative under Rules = !!

  given radical: Classpath.type is Radical:
    type Plane = Classpath
    def encode(root: Classpath.type): Text = t""

  given radicle: (tactic: Tactic[Path.Error]) => ((Classpath.type is Radicle on Classpath)^{tactic}) =
    new Radicle:
      type Self = Classpath.type
      type Plane = Classpath

      def length(text: Text): Int = 1

      def decode(text: Text): Classpath.type =
        if text.starts(t"/") then Classpath else abort(Path.Error(_.InvalidRoot))

  object Directory:
    def apply[path: Abstractable across Paths to Text](path: path): Classpath.Entry.Directory =
      Classpath.Entry.Directory(path.generic)

  object Jar:
    def apply[path: Abstractable across Paths to Text](path: path): Classpath.Entry.Jar =
      Classpath.Entry.Jar(path.generic)

  given filesystem: Classpath is Filesystem:
    type UniqueRoot = true

    val name: Text = "Java classpath"
    val separator: Text = "/"
    val self: Text = "."
    val parent: Text = ".."

  // An instance retains the tactics it reads under — a missing resource, a truncated read — and
  // the pure classloader, and says so.
  given streamable: [path <: Path on Classpath]
  =>  ( classpath: Tactic[Classpath.Error], truncation: Tactic[Truncation.Error],
        classloader: Classloader )
  =>  ( (path is Streamable by Data)^{classpath, truncation} ) =

    Streamable.inputStream.contramap: path => classloader.inputStream(path.encode)

  given source: [path <: Path on Classpath]
  =>  ( classpath: Tactic[Classpath.Error], truncation: Tactic[Truncation.Error],
        classloader: Classloader, buffering: Buffering )
  =>  ( (path is Streamable by Data over Credit)^{classpath, truncation} ) =

    Streamable.inputStream.contramap: path => classloader.inputStream(path.encode)

  // ClasspathEntry → Classpath.Entry
  object Entry:
    case class Directory(path: Text) extends Classpath.Entry:
      def apply[directory: Instantiable across Paths from Text](): directory = directory(path)

    case class Jar(path: Text) extends Classpath.Entry:
      def apply[file: Instantiable across Paths from Text](): file = file(path)

    case class Url(url: Text) extends Classpath.Entry:
      def apply[instantiable: Instantiable across Urls from Text](): instantiable =
        instantiable(url)

    case object JavaRuntime extends Classpath.Entry

  // The `java.net.URL` conversions (`Loaders.entry(url)`, `Entry#javaUrl`) live in `core-jvm`:
  // Scala Native's javalib has no `URL`.
  sealed trait Entry

  // ClasspathError → Classpath.Error
  case class Error(resource: Text)(using Diagnostics)
  extends fulminate.Error(540, 0)(m"the resource $resource was not on the classpath")

  // ClasspathEvent → Classpath.Event
  object Event:
    given communicable: Classpath.Event is Communicable =
      case ResourceLoaded(path)  => m"loaded the classpath resource $path"
      case ResourceMissing(path) => m"the classpath resource $path was not found"

  enum Event:
    case ResourceLoaded(path: Text) extends Classpath.Event, Log.Runtime, Log.Resource
    case ResourceMissing(path: Text) extends Classpath.Event, Log.Runtime, Log.Resource

// Building a classloader over the classpath (`classloader`) and enumerating its services
// (`services`) are extensions in `core-jvm`: they need `URLClassLoader` and `ServiceLoader`,
// which Scala Native has no use for — its classpath is linked into the binary.
trait Classpath:
  def entries: List[Classpath.Entry]
