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
package zeppelin

import java.io as ji
import java.net as jn
import java.nio.file as jnf
import java.util.zip as juz
import scala.collection.concurrent as scc
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import feudalism.*
import fulminate.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import vacuous.*

import juz.ZipFile

object Zipfile:
  given openable: Zipfile is Openable:
    type Self = Zipfile
    type Operand = Unit
    type Result = Root
    protected type Carrier = jnf.FileSystem

    def init(value: Zipfile, options: List[Operand]): Carrier =
      try jnf.FileSystems.newFileSystem(value.uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException =>
        panic(m"There was unexpectedly no filesystem provider for ZIP files")

    def handle(carrier: jnf.FileSystem): Zip.ZipRoot = Zip.ZipRoot(carrier)

    def close(carrier: Carrier): Unit = carrier.close()

  private val cache: scc.TrieMap[Text, Semaphore] = scc.TrieMap()

  def write[path: Abstractable across Paths into Text](path: path)(stream: Stream[ZipEntry]): Unit =
    val filename = path.generic
    val out: juz.ZipOutputStream = juz.ZipOutputStream(ji.FileOutputStream(ji.File(filename.s)))
    val directories: scm.HashSet[Path on Zip] = scm.HashSet()

    def addEntry(path: Path on Zip): Boolean =
      directories(path)
      || { directories += path
           out.putNextEntry(juz.ZipEntry(path.encode.s))
           out.closeEntry()
           false }

    for entry <- stream do
      entry.ref.ancestors.reverse.exists: path =>
        directories(path) || addEntry(path)

      out.putNextEntry(juz.ZipEntry(entry.ref.encode.s))

      entry.content().each: bytes =>
        out.write(bytes.mutable(using Unsafe))

      out.closeEntry()

    out.close()


case class Zipfile(path: Text):
  protected lazy val zipFile: juz.ZipFile = juz.ZipFile(ji.File(path.s)).nn
  protected lazy val uri: jn.URI = jn.URI.create(t"jar:file:$path".s).nn
  private def semaphore = Zipfile.cache.getOrElseUpdate(path, Semaphore())
