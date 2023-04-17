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
