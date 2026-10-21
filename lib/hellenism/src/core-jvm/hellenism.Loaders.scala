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

import java.io as ji
import java.net as jn
import java.util as ju

import scala.caps

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// The JVM half of the classpath model: everything that needs `java.net.URL`, `URLClassLoader` or
// `ServiceLoader`, which Scala Native's javalib does not have (and `nscplugin` rejects
// `ServiceLoader.load` with a runtime `Class`). On Scala Native a program's classpath is linked
// into its binary, so there is nothing to load; `Classloader#resource` and the `cp"…"` resources
// work there through the javalib's embedded resources.

extension (classloader: Classloader)
  // Building a `Classpath` from a `URLClassLoader` produces a galilei-backed `LocalClasspath`, so
  // `classpath` lives in `hellenism.jvm` (as an extension); `urlClassloader` is exposed to it.
  private[hellenism] def urlClassloader: Optional[jn.URLClassLoader] = classloader.java match
    case java: jn.URLClassLoader => java
    case _                       => classloader.parent.let(_.urlClassloader)

extension (entry: Classpath.Entry)
  def javaUrl: jn.URL = entry match
    case Classpath.Entry.Directory(path) => ji.File(path.s).toURI.nn.toURL.nn
    case Classpath.Entry.Jar(path)       => ji.File(path.s).toURI.nn.toURL.nn
    case Classpath.Entry.Url(url)        => jn.URI(url.s).toURL().nn
    case Classpath.Entry.JavaRuntime     => jn.URI("jrt:/").toURL().nn

extension (classRef: hellenism.internal.ClassRef)
  def classpathEntry: Optional[Classpath.Entry] =
    Loaders.entry(classRef.getProtectionDomain.nn.getCodeSource.nn.getLocation.nn)

extension (classpath: Classpath)
  def classloader
    ( delegation: Classloader.Delegation,
      parent: Classloader = classloaders.platformClassloader )
  :   Classloader =

    new Classloader(Loaders.delegatingClassloader(Loaders.array(classpath), parent.java, delegation))

  inline def services[service]: Set[service] =
    Loaders.servicesFor[service](classpath, reflectClass[service])

private[hellenism] object Loaders:
  // The entry a `URLClassLoader` URL denotes, or `Unset` for a protocol the model has no case for.
  def entry(url: jn.URL): Optional[Classpath.Entry] = url.getProtocol.nn.tt match
    case t"jrt"             => Classpath.Entry.JavaRuntime
    case t"http" | t"https" => Classpath.Entry.Url(url.toString.tt)

    case t"file" =>
      val path: Text = url.getPath.nn.tt
      if path.ends(t"/") then Classpath.Entry.Directory(path) else Classpath.Entry.Jar(path)

    case _ =>
      Unset

  // The Java runtime is omitted: a `jrt:/` URL means nothing to a `URLClassLoader`, and the
  // platform loader, at the root of every parent chain, supplies the JDK's classes anyway.
  def array(classpath: Classpath): scala.Array[jn.URL | Null] =
    val urls: List[jn.URL] = classpath.entries.flatMap:
      case Classpath.Entry.JavaRuntime => Nil
      case other                       => List(other.javaUrl)

    // `scala.Array.from` demands an `IterableOnce`, which the opaque `List` is not.
    scala.Array.from(urls.stdlib)

  def servicesFor[service](classpath: Classpath, cls: Class[service]): Set[service] =
    val parent = Optional(cls.getClassLoader).or(ClassLoader.getSystemClassLoader.nn)

    val entryUrls: List[jn.URL] = classpath.entries.flatMap:
      case Classpath.Entry.JavaRuntime => Nil
      case other                       => List(other.javaUrl)

    // `scala.Array.from` demands an `IterableOnce`, which the opaque `List` is not.
    val urls: scala.Array[jn.URL | Null] = scala.Array.from(entryUrls.stdlib)

    val loader = jn.URLClassLoader(urls, parent)
    val seen = scala.collection.mutable.Set.empty[Class[?]]
    val result = scala.collection.mutable.Set.empty[service]

    ju.ServiceLoader.load(cls, loader).nn.stream.nn.forEach: provider =>
      val provider0 = provider.nn
      if seen.add(provider0.`type`.nn) then result += provider0.get.nn

    result.to(Set)

  // Defined here, rather than inline in `classloader`, so the anonymous
  // `URLClassLoader` subclass carries no outer reference to a `Classpath` instance and so
  // does not spuriously capture it under capture checking.
  private[hellenism] def delegatingClassloader
    ( urls: scala.Array[jn.URL | Null], parent: ClassLoader, delegation: Classloader.Delegation )
  :   jn.URLClassLoader =

    // The loader's only capture is the read view of the freshly-built URL array, laundered
    // here.
    // [java-boundary] uRL array passed to Java URLClassLoader
    scala.caps.unsafe.unsafeAssumePure:
      delegation match
        case Classloader.Delegation.Deferential => new jn.URLClassLoader(urls, parent)

        // Child-first delegation must still honour the `ClassLoader` contract: consult
        // `findLoadedClass` under the per-name loading lock before defining. Without the
        // check, a second request for an already-loaded class — or two threads racing on the
        // same name, as concurrent test workers routinely do — reaches `defineClass` twice and
        // dies with a `LinkageError: attempted duplicate class definition`.
        case Classloader.Delegation.Preferential =>
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

