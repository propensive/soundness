/*
    Zeppelin, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package zeppelin

import serpentine.*
import gossamer.*
import probably.*
import contingency.*, errorHandlers.throwUnsafely
import imperial.*
import anticipation.*, fileApi.javaIo
import ambience.*, environments.virtualMachine, systemProperties.virtualMachine
import turbulence.*
import rudiments.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.skip

import java.io.File

object Tests extends Suite(t"Zeppelin tests"):
  def run(): Unit =

    val root: File = Base.Var.Tmp()
    root.mkdirs()

    test(t"Create an empty ZIP file"):
      val file = File(root, "empty.zip")
      if file.exists() then file.delete()
      ZipFile.create(file)
      file
    .assert(_.length > 0)
    
    val simpleFile: File = test(t"Create a simple ZIP file"):
      val path = File.createTempFile("tmp", ".zip").nn
      val entry = ZipEntry(ZipRef / p"hello.txt", t"Hello world")
      val zip = ZipFile.create(path)
      zip.append(LazyList(entry))
      path
    .check(_.length > 0)

    test(t"Check zip file contains one entry"):
      ZipFile(simpleFile).entries()
    .assert(_.length == 1)

    test(t"Check ZIP file's entry has correct content"):
      ZipFile(simpleFile).entries().head.readAs[Text]
    .assert(_ == t"Hello world")
    
    val twoEntryFile: File = test(t"Append a file to a ZIP archive"):
      val entry = ZipEntry(ZipRef / p"fox.txt", t"The quick brown fox jumps over the lazy dog.")
      val newFile: File  = File.createTempFile("tmp", ".zip").nn
      newFile.delete()
      java.nio.file.Files.copy(simpleFile.toPath, newFile.toPath)
      val zip = ZipFile(newFile)
      zip.append(LazyList(entry))
      newFile
    .check(_.length > 0)

    test(t"Check zip file based on another has two entries"):
      ZipFile(twoEntryFile).entries()
    .assert(_.length == 2)
    
    test(t"Check ZIP file's first entry has correct content after update"):
      ZipFile(twoEntryFile).entries().head.readAs[Text]
    .assert(_ == t"Hello world")
    
    test(t"Check ZIP file's second entry has correct content"):
      ZipFile(twoEntryFile).entries().tail.head.readAs[Text]
    .assert(_ == t"The quick brown fox jumps over the lazy dog.")
    
    test(t"Access ZIP file content by path"):
      (ZipFile(twoEntryFile) / p"fox.txt").readAs[Text]
    .assert(_ == t"The quick brown fox jumps over the lazy dog.")
    
    simpleFile.delete()
    twoEntryFile.delete()
