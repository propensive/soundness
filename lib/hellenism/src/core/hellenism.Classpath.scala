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
package hellenism

import java.net as jn
import java.util as ju

import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

object Classpath extends Root(t""):
  type Plane = Classpath
  type Rules = MustNotContain["/"] & MustNotMatch["[0-9].*"] & MustMatch["[a-zA-Z0-9_$.]+"]

  inline given nominative: Classpath is Nominative under Rules = !!

  given radical: Tactic[PathError] => Classpath.type is Radical:
    type Plane = Classpath

    def length(text: Text): Int = 1

    def decode(text: Text): Classpath.type =
      if text.starts(t"/") then Classpath else abort(PathError(_.InvalidRoot))

    def encode(root: Classpath.type): Text = t""

  object Directory:
    def apply[path: Abstractable across Paths to Text](path: path): ClasspathEntry.Directory =
      ClasspathEntry.Directory(path.generic)

  object Jar:
    def apply[path: Abstractable across Paths to Text](path: path): ClasspathEntry.Jar =
      ClasspathEntry.Jar(path.generic)

  given filesystem: Classpath is Filesystem:
    type UniqueRoot = true

    val name: Text = "Java classpath"
    val separator: Text = "/"
    val self: Text = "."
    val parent: Text = ".."

  given streamable: [path <: Path on Classpath] => Tactic[ClasspathError]
  =>  ( classloader: Classloader )
  =>  path is Streamable by Data =

    given Tactic[StreamError] = strategies.throwUnsafely

    Streamable.inputStream.contramap: path =>
      classloader.inputStream(path.encode)


  def servicesFor[service](classpath: Classpath, cls: Class[service]): Set[service] =
    val parent = Optional(cls.getClassLoader).or(ClassLoader.getSystemClassLoader.nn)

    val urls: Array[jn.URL | Null] =
      Array.from(classpath.entries.flatMap:
        case ClasspathEntry.JavaRuntime => Nil
        case other                      => List(other.javaUrl))

    val loader = jn.URLClassLoader(urls, parent)
    val seen = scala.collection.mutable.Set.empty[Class[?]]
    val result = scala.collection.mutable.Set.empty[service]

    ju.ServiceLoader.load(cls, loader).nn.stream.nn.forEach: provider =>
      val provider0 = provider.nn
      if seen.add(provider0.`type`.nn) then result += provider0.get.nn

    result.toSet


trait Classpath:
  def entries: List[ClasspathEntry]
  private def array: Array[jn.URL | Null] = Array.from(entries.map(_.javaUrl))

  def classloader(parent: Classloader = classloaders.platformClassloader): Classloader =
    val javaClassloader = new jn.URLClassLoader(array, parent.java):
      override def loadClass(name: String | Null, resolve: Boolean): Class[?] | Null =
        try findClass(name) catch case error: ClassNotFoundException =>
          super.loadClass(name, resolve)

    new Classloader(javaClassloader)

  def classloader: Classloader =
    val urls = entries.flatMap:
      case ClasspathEntry.JavaRuntime => Nil
      case other                      => List(other.javaUrl)

    new Classloader
      ( new jn.URLClassLoader(Array.from(urls), ClassLoader.getPlatformClassLoader().nn) )

  inline def services[service]: Set[service] =
    Classpath.servicesFor[service](this, reflectClass[service])
