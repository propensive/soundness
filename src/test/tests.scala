/*
    Galilei, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import serpentine.*
import spectacular.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8
import anticipation.*, fileApi.galileiApi
import ambience.*, environments.system

import unsafeExceptions.canThrowAny
import filesystemOptions.createNonexistentParents.yes

object Tests extends Suite(t"Galilei tests"):
  def run(): Unit =
    
    suite(t"Flexible hierarchy tests"):
      import hierarchies.flexible
      import filesystemOptions.dereferenceSymlinks.yes
      
      val tmpPath = test(t"Get /var/tmp"):
        Xdg.Var.Tmp()
      .check(_.fullname == t"/var/tmp")

      val galileiTmpPath = test(t"Get /var/tmp/galilei"):
        tmpPath / p"galilei"
      .check(_.fullname == t"/var/tmp/galilei")

      test(t"Check descent of path"):
        galileiTmpPath.descent.map(_.show)
      .assert(_ == List(t"galilei", t"tmp", t"var"))

      val tmpDir = galileiTmpPath.make[Directory]()

      suite(t"File and directory creation"):
        test(t"Create a new file"):
          val path = (tmpDir.path / p"file.txt")
          path.make[File]()
          path
        .assert(_.exists())

        test(t"Delete a file"):
          (tmpDir.path / p"file.txt").file().delete()
        .assert(!_.exists())
    
    suite(t"Unix hierarchy tests"):
      import hierarchies.unix
      
      val tmpPath = test(t"Get /var/tmp"):
        Xdg.Var.Tmp()
      .check(_.fullname == t"/var/tmp")

      val galileiTmpPath = test(t"Get /var/tmp/galilei"):
        tmpPath / p"galilei"
      .check(_.fullname == t"/var/tmp/galilei")
      
      test(t"Check descent of path"):
        galileiTmpPath.descent.map(_.show)
      .assert(_ == List(t"galilei", t"tmp", t"var"))
    
    suite(t"Windows hierarchy tests"):
      import hierarchies.windows
      
      val windowsSystem = test(t"Get C:\\Windows\\System"):
        Windows.Drive('C') / p"Windows" / p"System"
      .check(_.fullname == t"C:\\Windows\\System")
      
      test(t"Check descent of path"):
        windowsSystem.descent.map(_.show)
      .assert(_ == List(t"System", t"Windows"))
      
      test(t"Get root"):
        windowsSystem.root
      .assert(_ == Windows.Drive('C'))
      