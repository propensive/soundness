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
┃    Soundness, version 0.45.0.                                                                    ┃
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
import java.io as ji

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

object Classpath extends Root(t""):
  type Plane = Classpath
  type Rules = MustNotContain["/"] & MustNotMatch["[0-9].*"] & MustMatch["[a-zA-Z0-9_$.]+"]

  erased given nominative: Classpath is Nominative under Rules = !!

  given radical: Tactic[PathError] => Classpath.type is Radical:
    type Plane = Classpath
    def length(text: Text): Int = 1

    def decode(text: Text): Classpath.type =
      if text.starts(t"/") then Classpath else raise(PathError(_.InvalidRoot)) yet Classpath

    def encode(root: Classpath.type): Text = t""


  object Directory:
    def apply[path: Abstractable across Paths to Text](path: path): ClasspathEntry.Directory =
      ClasspathEntry.Directory(path.generic)

  object Jar:
    def apply[path: Abstractable across Paths to Text](path: path): ClasspathEntry.Jar =
      ClasspathEntry.Jar(path.generic)

  given system: Classpath is System:
    type UniqueRoot = true
    val separator: Text = t"/"
    val self: Text = t"."
    val parent: Text = t".."

  given substantiable: (classloader: Classloader) => (Path on Classpath) is Substantiable =
    path => classloader.java.getResourceAsStream(path.encode.s) != null

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

  given streamable: [path <: Path on Classpath]
        => Tactic[ClasspathError]
        => (classloader: Classloader)
        => path is Streamable by Bytes =
    given Tactic[StreamError] = strategies.throwUnsafely

    Streamable.inputStream.contramap: path =>
      classloader.inputStream(path.encode)

trait Classpath:
  def entries: List[ClasspathEntry]
  private def array: Array[jn.URL | Null] = Array.from(entries.map(_.javaUrl))

  def classloader(parent: Classloader = classloaders.platform): Classloader =
    val javaClassloader = new jn.URLClassLoader(array, parent.java):
      override def loadClass(name: String | Null, resolve: Boolean): Class[?] | Null =
        try findClass(name) catch case error: ClassNotFoundException =>
          super.loadClass(name, resolve)

    new Classloader(javaClassloader)

  def classloader: Classloader =
    val urls = entries.flatMap:
      case ClasspathEntry.Jar(jarfile)   => List(ji.File(jarfile.s).toURI.nn.toURL.nn)
      case ClasspathEntry.Directory(dir) => List(ji.File(dir.s).toURI.nn.toURL.nn)
      case ClasspathEntry.Url(url)       => List(jn.URI.create(url.s).nn.toURL.nn)
      case ClasspathEntry.JavaRuntime    => Nil

    new Classloader
         (new jn.URLClassLoader(Array.from(urls), ClassLoader.getPlatformClassLoader().nn))
