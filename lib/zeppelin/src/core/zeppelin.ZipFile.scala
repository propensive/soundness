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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
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
    type Result = Zip.ZipRoot
    protected type Transport = jnf.FileSystem

    def initialize(value: Zipfile, options: List[Operand]): Transport =
      try jnf.FileSystems.newFileSystem(value.uri, Map("zipinfo-time" -> "false").asJava).nn
      catch case exception: jnf.ProviderNotFoundException =>
        panic(m"There was unexpectedly no filesystem provider for ZIP files")

    def handle(transport: jnf.FileSystem): Zip.ZipRoot = Zip.ZipRoot()
    def close(transport: Transport): Unit = transport.close()


  def write[path: Abstractable across Paths to Text](path: path)(stream: Iterable[Zip.Entry])
  :   Unit raises ZipError =

    val filename = path.generic
    val out: juz.ZipOutputStream = juz.ZipOutputStream(ji.FileOutputStream(ji.File(filename.s)))

    for entry <- stream do
      val ref = entry.ref.encode
      val ref2 = if ref.starts("/") then ref.skip(1).s else ref.s

      try out.putNextEntry(juz.ZipEntry(ref2)) catch case exception: juz.ZipException =>
        raise(ZipError(ZipError.Reason.DuplicateEntry(entry.ref)))

      entry.content().each: bytes => out.write(bytes.mutable(using Unsafe))
      out.closeEntry()

    out.close()


case class Zipfile(path: Text):
  protected lazy val zipFile: juz.ZipFile = juz.ZipFile(ji.File(path.s)).nn
  protected lazy val uri: jn.URI = jn.URI.create(t"jar:file:$path".s).nn
