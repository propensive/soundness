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

import ambience.*, environments.virtualMachine, systemProperties.virtualMachine
import anticipation.*, filesystemApi.javaIoFile
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, textSanitizers.skip
import imperial.*
import probably.*
import proscenium.*
import rudiments.*
import serpentine.*
import turbulence.*

import java.io.File

object Tests extends Suite(m"Zeppelin tests"):
  def run(): Unit =

    val root: File = Base.Var.Tmp()
    root.mkdirs()

    test(m"Create an empty ZIP file"):
      val file = File(root, "empty.zip")
      if file.exists() then file.delete()
      Zipfile.create(file)
      file
    .assert(_.length > 0)

    val simpleFile: File = test(m"Create a simple ZIP file"):
      val path = File.createTempFile("tmp", ".zip").nn
      val entry = ZipEntry(ZipRef / p"hello.txt", t"Hello world")
      val zip = Zipfile.create(path)
      zip.append(Stream(entry))
      path
    .check(_.length > 0)

    test(m"Check zip file contains one entry"):
      Zipfile(simpleFile).entries()
    .assert(_.length == 1)

    test(m"Check ZIP file's entry has correct content"):
      Zipfile(simpleFile).entries().head.read[Text]
    .assert(_ == t"Hello world")

    val twoEntryFile: File = test(m"Append a file to a ZIP archive"):
      val entry = ZipEntry(ZipRef / p"fox.txt", t"The quick brown fox jumps over the lazy dog.")
      val newFile: File  = File.createTempFile("tmp", ".zip").nn
      newFile.delete()
      java.nio.file.Files.copy(simpleFile.toPath, newFile.toPath)
      val zip = Zipfile(newFile)
      zip.append(Stream(entry))
      newFile
    .check(_.length > 0)

    test(m"Check zip file based on another has two entries"):
      Zipfile(twoEntryFile).entries()
    .assert(_.length == 2)

    test(m"Check ZIP file's first entry has correct content after update"):
      Zipfile(twoEntryFile).entries().head.read[Text]
    .assert(_ == t"Hello world")

    test(m"Check ZIP file's second entry has correct content"):
      Zipfile(twoEntryFile).entries().tail.head.read[Text]
    .assert(_ == t"The quick brown fox jumps over the lazy dog.")

    test(m"Access ZIP file content by path"):
      (Zipfile(twoEntryFile) / p"fox.txt").read[Text]
    .assert(_ == t"The quick brown fox jumps over the lazy dog.")

    simpleFile.delete()
    twoEntryFile.delete()
