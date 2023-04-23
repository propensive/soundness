/*
    Zeppelin, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import imperial.*
import galilei.*, filesystems.unix
import anticipation.*, fileApi.galileiApi
import ambience.*, environments.system
import turbulence.*, characterEncodings.utf8
import rudiments.*

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Zeppelin tests"):
  def run(): Unit =

    val root: Directory = Xdg.Var.Tmp().directory(Expect)

    test(t"Create an empty ZIP file"):
      val zip = ZipFile.create(root / p"empty.zip")
      zip.file
    .assert(_.size() > 0.b)
    
    val simpleFile = test(t"Create a simple ZIP file"):
      val path = root.tmpPath(t".zip")
      val entry = ZipEntry(? / p"hello.txt", t"Hello world")
      val zip = ZipFile(path.file(Create))
      zip.append(LazyList(entry))
      zip.file
    .check(_.size() > 0.b)

    test(t"Check zip file contains one entry"):
      ZipFile(simpleFile).entries()
    .assert(_.length == 1)

    test(t"Check ZIP file's entry has correct content"):
      ZipFile(simpleFile).entries().head.read[Text]
    .assert(_ == t"Hello world")
    
    val twoEntryFile = test(t"Append a file to a ZIP archive"):
      val entry = ZipEntry(? / p"fox.txt", t"The quick brown fox jumps over the lazy dog.")
      val path = root.tmpPath(t".zip")
      val zip = ZipFile(path.file(Create))
      zip.append(LazyList(entry), base = simpleFile)
      zip.file
    .check(_.size() > 0.b)

    test(t"Check zip file based on another has two entries"):
      ZipFile(twoEntryFile).entries()
    .assert(_.length == 2)
    
    test(t"Check ZIP file's first entry has correct content after update"):
      ZipFile(twoEntryFile).entries().head.read[Text]
    .assert(_ == t"Hello world")
    
    test(t"Check ZIP file's second entry has correct content"):
      ZipFile(twoEntryFile).entries().tail.head.read[Text]
    .assert(_ == t"The quick brown fox jumps over the lazy dog.")
    
    simpleFile.delete()
    twoEntryFile.delete()
