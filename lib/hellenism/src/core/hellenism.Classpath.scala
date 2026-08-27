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
import zephyrine.*
import vacuous.*
import java.io as ji
import fulminate.*

object Classpath extends Root(t""):
  type Plane = Classpath
  type Rules = MustNotContain["/"] & MustNotMatch["[0-9].*"] & MustMatch["[a-zA-Z0-9_$.]+"]

  inline given nominative: Classpath is Nominative under Rules = !!

  given radical: Classpath.type is Radical:
    type Plane = Classpath

    def length(text: Text): Int raises Path.Error = 1

    def decode(text: Text): Classpath.type raises Path.Error =
      if text.starts(t"/") then Classpath else abort(Path.Error(_.InvalidRoot))

    def encode(root: Classpath.type): Text = t""

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

  given streamable: [path <: Path on Classpath] => Tactic[Classpath.Error]
  =>  ( classloader: Classloader )
  =>  path is Streamable by Data =

    // Built over `throwUnsafely` (unscoped, merely throws) and the pure classloader, so the
    // instance retains no scoped capability; laundered pure (the codec-thunk seal pattern).
    caps.unsafe.unsafeAssumePure:
      given Tactic[Truncation.Error] = strategies.throwUnsafely

      Streamable.inputStream.contramap: path => classloader.inputStream(path.encode)

  given source: [path <: Path on Classpath] => Tactic[Classpath.Error]
  =>  ( classloader: Classloader, buffering: Buffering )
  =>  path is Streamable by Data over Credit =

    // See `streamable` above; laundered pure for the same reason.
    caps.unsafe.unsafeAssumePure:
      given Tactic[Truncation.Error] = strategies.throwUnsafely

      Streamable.inputStream.contramap: path => classloader.inputStream(path.encode)

  def servicesFor[service](classpath: Classpath, cls: Class[service]): Set[service] =
    val parent = Optional(cls.getClassLoader).or(ClassLoader.getSystemClassLoader.nn)

    val urls: scala.Array[jn.URL | Null] =
      scala.Array.from(classpath.entries.stdlib.flatMap:
        case Classpath.Entry.JavaRuntime => Nil.stdlib
        case other                      => List(other.javaUrl).stdlib)

    val loader = jn.URLClassLoader(urls, parent)
    val seen = scala.collection.mutable.Set.empty[Class[?]]
    val result = scala.collection.mutable.Set.empty[service]

    ju.ServiceLoader.load(cls, loader).nn.stream.nn.forEach: provider =>
      val provider0 = provider.nn
      if seen.add(provider0.`type`.nn) then result += provider0.get.nn

    result.to(Set)

  // Defined here, rather than inline in `Classpath#classloader`, so the anonymous
  // `URLClassLoader` subclass carries no outer reference to a `Classpath` instance and so
  // does not spuriously capture it under capture checking.
  private[hellenism] def delegatingClassloader(urls: scala.Array[jn.URL | Null], parent: ClassLoader)
  :   jn.URLClassLoader =

    // The anonymous classloader's only capture is the read view of the freshly-built URL
    // array, laundered here.
    //
    // Child-first delegation must still honour the `ClassLoader` contract: consult
    // `findLoadedClass` under the per-name loading lock before defining. Without the check, a
    // second request for an already-loaded class — or two threads racing on the same name, as
    // concurrent test workers routinely do — reaches `defineClass` twice and dies with a
    // `LinkageError: attempted duplicate class definition`.
    scala.caps.unsafe.unsafeAssumePure:
     new jn.URLClassLoader(urls, parent):
      override def loadClass(name: String | Null, resolve: Boolean): Class[?] | Null =
        getClassLoadingLock(name).nn.synchronized:
          val loaded = findLoadedClass(name)

          if loaded != null then
            if resolve then resolveClass(loaded)
            loaded
          else
            try findClass(name) catch case error: ClassNotFoundException =>
              super.loadClass(name, resolve)

  // ClasspathEntry → Classpath.Entry
  object Entry:
    case class Directory(path: Text) extends Classpath.Entry:
      def apply[directory: Instantiable across Paths from Text](): directory = directory(path)

    case class Jar(path: Text) extends Classpath.Entry:
      def apply[file: Instantiable across Paths from Text](): file = file(path)

    case class Url(url: Text) extends Classpath.Entry:
      def apply[instantiable: Instantiable across Urls from Text](): instantiable = instantiable(url)

    case object JavaRuntime extends Classpath.Entry

    def apply(url: jn.URL): Optional[Classpath.Entry] = url.getProtocol.nn.tt match
      case t"jrt"             => Classpath.Entry.JavaRuntime
      case t"http" | t"https" => Classpath.Entry.Url(url.toString.tt)

      case t"file" =>
        val path: Text = url.getPath.nn.tt
        if path.ends(t"/") then Classpath.Entry.Directory(path) else Classpath.Entry.Jar(path)

      case _ =>
        Unset

  sealed trait Entry:
    def javaUrl: jn.URL = this match
      case Classpath.Entry.Directory(path) => ji.File(path.s).toURI.nn.toURL.nn
      case Classpath.Entry.Jar(path)       => ji.File(path.s).toURI.nn.toURL.nn
      case Classpath.Entry.Url(url)        => jn.URI(url.s).toURL().nn
      case Classpath.Entry.JavaRuntime     => jn.URI("jrt:/").toURL().nn

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

trait Classpath:
  def entries: List[Classpath.Entry]
  private def array: scala.Array[jn.URL | Null] = scala.Array.from(entries.stdlib.map(_.javaUrl))

  def classloader(parent: Classloader = classloaders.platformClassloader): Classloader =
    new Classloader(Classpath.delegatingClassloader(array, parent.java))

  def classloader: Classloader =
    val urls = entries.stdlib.flatMap:
      case Classpath.Entry.JavaRuntime => Nil.stdlib
      case other                      => List(other.javaUrl).stdlib

    new Classloader
      ( new jn.URLClassLoader(scala.Array.from(urls), ClassLoader.getPlatformClassLoader().nn) )

  inline def services[service]: Set[service] =
    Classpath.servicesFor[service](this, reflectClass[service])
