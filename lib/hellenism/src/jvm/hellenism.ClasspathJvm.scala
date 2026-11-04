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

import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

// The galilei-backed parts of `Classpath`, split out of `hellenism.core` so that the `Classpath`
// plane, `cp""` and `Resource` (which need only serpentine paths) can cross-compile to Scala.js.
// `Substantiable` comes from galilei, and the `URLClassLoader` factory builds a `LocalClasspath`,
// both of which are JVM-only.

given classpathSubstantiable: (classloader: Classloader) => (Path on Classpath) is Substantiable =
  path => classloader.java.getResource(path.encode.s) != null

extension (classloader: Classloader)
  def classpath: Optional[Classpath] = classloader.urlClassloader.let(Classpath.of(_))

extension (classpath: Classpath.type)
  def of(classloader: jn.URLClassLoader): Classpath =
    val entries =
      classloader.getURLs.nn.iterator.map(_.nn).flatMap(Loaders.entry(_).option).toList

    if entries.exists:
      case _: Classpath.Entry.Url => true
      case _                      => false
    then OnlineClasspath(entries.to(List))
    else
      type Entry =
        Classpath.Entry.Directory | Classpath.Entry.Jar | Classpath.Entry.JavaRuntime.type

      val items: List[Entry] =
        List.from:
          entries.collect:
            case directory: Classpath.Entry.Directory      => directory
            case jar: Classpath.Entry.Jar                  => jar
            case runtime: Classpath.Entry.JavaRuntime.type => runtime

      LocalClasspath(items*)

// `Classpath.Entry.classify(path)`: the entry at `path`, classified by what is there rather
// than by its name. A directory is a `Directory`; a file is a `Jar` if it ends with a ZIP
// end-of-central-directory record (so a launcher's shebang-prefixed executable, which has no
// `.jar` suffix, is one), and otherwise an `Io.Error`; a path that does not exist is a
// `Directory` which may yet be made. The classification needs the filesystem, which the
// platform-neutral `Entry` companion cannot reach, hence an extension here; it is not `apply`
// because an `apply` extension exported to `soundness` would clash with rudiments' generic one.
extension (entry: Classpath.Entry.type)
  def classify[path: Abstractable across Paths to Text](path: path)
    ( using pathTactic: Tactic[Path.Error], ioTactic: Tactic[Io.Error], symlinks: DereferenceSymlinks )
  :   Classpath.Entry.Directory | Classpath.Entry.Jar =

    import filesystemBackends.javaBaseFilesystem
    val resolved: Path on Linux = path.generic.as[Path on Linux]

    if !resolved.existent() then Classpath.Entry.Directory(resolved.encode)
    else if resolved.entry() == Directory then Classpath.Entry.Directory(resolved.encode)
    else if Classpath.archive(resolved.encode) then Classpath.Entry.Jar(resolved.encode)
    else abort(Io.Error(resolved, Io.Error.Operation.Read, Io.Error.Reason.InvalidData))

extension (classpath: Classpath.type)
  // Whether the file at `path` is a ZIP archive: its last 64 KiB (the longest trailing comment
  // permits) are scanned backwards for the end-of-central-directory signature, `PK\u0005\u0006`.
  private[hellenism] def archive(path: Text): Boolean =
    try
      val channel = java.nio.channels.FileChannel.open(java.nio.file.Path.of(path.s)).nn

      try
        val size: Long = channel.size
        val window: Int = size.min(22L + 65535L).toInt
        val buffer = java.nio.ByteBuffer.allocate(window).nn
        channel.position(size - window)
        while buffer.hasRemaining && channel.read(buffer) > 0 do ()
        val bytes: scala.Array[Byte] = buffer.array.nn
        var index: Int = window - 22

        // [scan] a backwards signature search over a fixed window; the trailer's offset is the goal
        while index >= 0 &&
            !(bytes(index) == 0x50 && bytes(index + 1) == 0x4b && bytes(index + 2) == 0x05 &&
              bytes(index + 3) == 0x06)
        do index -= 1

        index >= 0
      finally channel.close()
    catch case _: java.io.IOException => false
