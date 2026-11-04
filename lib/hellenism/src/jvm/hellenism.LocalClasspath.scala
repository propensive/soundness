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

import java.net as jn
import java.nio.file as jnf

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import vacuous.*

import filesystemBackends.javaBaseFilesystem

object LocalClasspath:
  given encodable: System => LocalClasspath is Encodable in Text = _()

  // `encodable` needs a `System`, since the separator between entries is a system property, and
  // a debug rendering must always be available; the entries are therefore joined here with `:`
  // regardless of platform, and the whole rendering is tagged so that it cannot be mistaken for
  // the platform-specific text which `encodable` produces.
  given inspectable: [localClasspath <: LocalClasspath] => localClasspath is Inspectable =
    _.entries.map:
      case Classpath.Entry.Directory(directory) => directory
      case Classpath.Entry.Jar(jar)             => jar
      case Classpath.Entry.JavaRuntime          => t"‹jrt›"

    . join(t"classpath⟨", t":", t"⟩")

  given decodable: (System, Tactic[Property.Error])
  =>  LocalClasspath is Decodable in Text =

    classpath =>
      val entries: List[Classpath.Entry.Directory | Classpath.Entry.Jar] =
        classpath.cut(System.properties.path.separator())
        . map[Classpath.Entry.Directory | Classpath.Entry.Jar]: path =>
          if path.ends(t"/") then Classpath.Entry.Directory(path)
          else if path.ends(t".jar") then Classpath.Entry.Jar(path)
          else Classpath.Entry.Directory(path)

      // `Set` is invariant, so the elements are widened before the conversion.
      val widened: List[Classpath.Entry] = entries

      new LocalClasspath(entries, widened.to[Set])

  // The classpath a classloader loads from, when it is a `URLClassLoader` over local entries —
  // as it is under a host such as fume, which isolates each test suite in one — and otherwise
  // the `java.class.path` property. This, not the property alone, is what a process launched
  // to run the same code must be given: under a host, the property names the host's own jars.
  // The classpath the running process was loaded from, as the loaders name it: `classloader`'s
  // URLs, then its parents' (a launcher may load a tool's libraries through a child of the
  // loader it was given), then `java.class.path`, in that order, each entry once. Each is
  // classified by what is on disk, so a launcher's executable — an archive with no `.jar`
  // suffix — is a `Jar`, and an entry that does not exist is a `Directory` which may yet be made.
  def running(classloader: Classloader = Classloader.threadContext)
    ( using System, Tactic[Path.Error], Tactic[Io.Error], DereferenceSymlinks )
  :   LocalClasspath =

    def urls(loader: Optional[Classloader]): List[Text] = loader.lay(Nil): loader =>
      val own: List[Text] = loader.java match
        case urlLoader: jn.URLClassLoader =>
          List.from(urlLoader.getURLs.nn.iterator.map(_.nn)).bind: url =>
            if url.getProtocol.nn == "file"
            then List(jnf.Path.of(url.toURI.nn).nn.toString.nn.tt)
            else Nil

        case _ =>
          Nil

      own + urls(loader.parent)

    val system: List[Text] =
      safely(System.properties.java.`class`.path[Text]()).lay(Nil): path =>
        path.cut(System.properties.path.separator().show).filter(_ != t"")

    // Each once, in the order first named.
    def once(paths: List[Text], seen: Set[Text]): List[Text] = paths match
      case Nil          => Nil
      case head :: tail => if seen.has(head) then once(tail, seen) else head :: once(tail, seen + Set(head))

    val entries: List[Classpath.Entry.Directory | Classpath.Entry.Jar] =
      once(urls(classloader) + system, Set()).map: (path: Text) =>
        Classpath.Entry.classify(path.as[Path on Linux])

    val widened: List[Classpath.Entry] = entries
    new LocalClasspath(entries, widened.to[Set])

  def of(classloader: Classloader)(using System): LocalClasspath = classloader.classpath match
    case classpath: LocalClasspath => classpath

    // A JVM always defines `java.class.path`; were it absent, the classpath would be empty.
    case _ =>
      safely(System.properties.java.`class`.path().as[LocalClasspath]).or(LocalClasspath())

  def apply
    ( entries:
        (Classpath.Entry.Directory | Classpath.Entry.Jar | Classpath.Entry.JavaRuntime.type)* )
  :   LocalClasspath =

    new LocalClasspath(entries.toList.to(List), entries.to(Set))


  // The classpath of one entry, by its path: `of`, not `apply`, since a single `Classpath.Entry`
  // is itself a path, and `LocalClasspath(entry)` would be ambiguous with the entries' `apply`.
  def of[path: Abstractable across Paths to Text]
    ( path: path )
    ( using Tactic[Path.Error],
            Tactic[Io.Error],
            DereferenceSymlinks )
  :   LocalClasspath =

    new LocalClasspath(Nil, Set.empty) + path


  given paths: [path: Abstractable across Paths to Text]
  =>  ( pathTactic: Tactic[Path.Error], ioTactic: Tactic[Io.Error], deref: DereferenceSymlinks )
  =>  ( (LocalClasspath is Addable by path to LocalClasspath)^{pathTactic, ioTactic} ) =

    (classpath, path) =>
      path.generic.as[Path on Linux].pipe: path =>
        val entry: Classpath.Entry.Directory | Classpath.Entry.Jar = Classpath.Entry.classify(path)

        if classpath.entrySet.has(entry) then classpath
        else
          // `Set` is invariant, so the singleton is built at the wider element type.
          new LocalClasspath
            ( entry :: classpath.entries, classpath.entrySet + Set[Classpath.Entry](entry) )

class LocalClasspath private
  ( val entries
  : List[Classpath.Entry.Directory | Classpath.Entry.Jar | Classpath.Entry.JavaRuntime.type],
    val entrySet: Set[Classpath.Entry] )
extends Classpath:
  def apply()(using System): Text =
    entries.bind:
      case Classpath.Entry.Directory(directory) => List(directory)
      case Classpath.Entry.Jar(jar)             => List(jar)
      case _                                    => Nil

    . join(System.properties.path.separator().show)
