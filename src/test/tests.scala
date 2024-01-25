/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import probably.*
import gossamer.*
import imperial.*
import turbulence.*, stdioSources.virtualMachine
import eucalyptus.*
import parasite.*
import rudiments.*, workingDirectories.virtualMachine
import serpentine.*
import spectacular.*
import perforate.*
import vacuous.*
import anticipation.*, fileApi.galileiApi
import ambience.*, environments.virtualMachine, systemProperties.virtualMachine

import errorHandlers.throwUnsafely

object Tests extends Suite(t"Galilei tests"):
  def run(): Unit =
    suite(t"Link tests"):
      suite(t"UNIX tests"):
        import hierarchies.unix

        test(t"Parse a relative Unix link"):
          t"../docs/data.txt".decodeAs[Unix.Link]
        .assert(_ == ?^ / p"docs" / p"data.txt")
      
      suite(t"Windows tests"):
        import hierarchies.windows

        test(t"Parse a relative Unix link"):
          t"..\\Docs\\Data.txt".decodeAs[Windows.Link]
        .assert(_ == ?^ / p"Docs" / p"Data.txt")
      
    suite(t"Path tests"):
      suite(t"UNIX tests"):
        import hierarchies.unix
      
        test(t"Get volume in UNIX"):
          import filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
          (Base.Var.Tmp() / p"galilei").as[Directory].volume
        .matches:
          case Volume(_, _) =>

        test(t"Parse /home/work/file.txt"):
          t"/home/work/file.txt".decodeAs[Unix.Path]
        .assert(_ == % / p"home" / p"work" / p"file.txt")

        test(t"Parsing C:\\Windows\\System32 should fail"):
          capture[PathError](t"C:\\Windows\\System32".decodeAs[Unix.Path])
        .assert(_ == PathError(t"C:\\Windows\\System32", PathError.Reason.NotRooted))
        
      suite(t"Windows tests"):
        test(t"Parsing /home/work/file.txt should fail"):
          capture[PathError](t"/home/work/file.txt".decodeAs[Windows.Path])
        .assert(_ == PathError(t"/home/work/file.txt", PathError.Reason.NotRooted))

        test(t"Parse C:\\Windows\\System32"):
          t"C:\\Windows\\System32".decodeAs[Windows.Path]
        .assert(_ == Windows.Drive('C') / p"Windows" / p"System32")
        
      suite(t"Adaptive tests"):
        test(t"Parsing /home/work/file.txt should fail"):
          t"/home/work/file.txt".decodeAs[Path]
        .assert(_ == Unix / p"home" / p"work" / p"file.txt")

        test(t"Parse C:\\Windows\\System32"):
          t"C:\\Windows\\System32".decodeAs[Windows.Path]
        .assert(_ == Windows.Drive('C') / p"Windows" / p"System32")
    
    suite(t"Adaptive hierarchy tests"):
      import hierarchies.unixOrWindows
      import filesystemOptions.dereferenceSymlinks
      import filesystemOptions.doNotCreateNonexistent
      
      val tmpPath = test(t"Get /var/tmp"):
        Base.Var.Tmp()
      .check(_.fullname == t"/var/tmp")

      val galileiTmpPath = test(t"Get /var/tmp/galilei"):
        tmpPath / p"galilei"
      .check(_.fullname == t"/var/tmp/galilei")

      test(t"Check descent of path"):
        galileiTmpPath.descent.map(_.show)
      .assert(_ == List(t"galilei", t"tmp", t"var"))

      val tmpDir =
        test(t"Create a directory which may or may not exist"):
          import filesystemOptions.{overwritePreexisting, deleteRecursively, createNonexistentParents}
          galileiTmpPath.make[Directory]()
        .check(_.stillExists())
      
      val tmpDir2 =
        test(t"Try creating a directory which already exists"):
          import filesystemOptions.{doNotOverwritePreexisting, createNonexistentParents}
          capture[OverwriteError](galileiTmpPath.make[Directory]())
        .check(_ ==  OverwriteError(galileiTmpPath))

      suite(t"File and directory creation"):
        test(t"Create a new file"):
          import filesystemOptions.{doNotOverwritePreexisting, createNonexistentParents}
          val path = (tmpDir.path / p"file.txt")
          path.make[File]()
        .assert(_.stillExists())

        test(t"Create a symlink to a file"):
          import filesystemOptions.{doNotOverwritePreexisting, doNotCreateNonexistent, doNotCreateNonexistentParents, doNotDereferenceSymlinks}
          val link = (tmpDir.path / p"linked.txt")
          (tmpDir.path / p"file.txt").as[File].symlinkTo(link)
          link.entryType()
        .assert(_ == PathStatus.Symlink)

        test(t"Delete a file"):
          import filesystemOptions.{doNotDeleteRecursively, createNonexistent, createNonexistentParents}
          (tmpDir.path / p"file.txt").as[File].delete()
          
          tmpDir.path / p"file.txt"
        .assert(!_.exists())
      
    suite(t"Unix hierarchy tests"):
      import hierarchies.unix
      import filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
      
      val tmpPath = test(t"Get /var/tmp"):
        Base.Var.Tmp()
      .check(_.fullname == t"/var/tmp")

      val galileiTmpPath = test(t"Get /var/tmp/galilei"):
        tmpPath / p"galilei"
      .check(_.fullname == t"/var/tmp/galilei")
      
      test(t"Check descent of path"):
        galileiTmpPath.descent.map(_.show)
      .assert(_ == List(t"galilei", t"tmp", t"var"))
    
      test(t"Get volume"):
        (tmpPath / p"galilei").as[Directory].volume
      .matches:
        case Volume(_, _) =>
      
      
      test(t"Make a FIFO"):
        import filesystemOptions.{doNotCreateNonexistentParents, overwritePreexisting, doNotDeleteRecursively, dereferenceSymlinks}
        
        supervise:
          import logging.stdout
          val fifoPath = tmpPath / p"galilei" / p"fifo1"
          fifoPath.make[Fifo]().path.entryType()
      .assert(_ == PathStatus.Fifo)

    suite(t"Windows hierarchy tests"):
      
      val windowsSystem = test(t"Get C:\\Windows\\System"):
        Windows.Drive('C') / p"Windows" / p"System"
      .check(_.fullname == t"C:\\Windows\\System")
      
      test(t"Check descent of path"):
        windowsSystem.descent.map(_.show)
      .assert(_ == List(t"System", t"Windows"))
      
      test(t"Get root"):
        windowsSystem.root
      .assert(_ == Windows.Drive('C'))
      
