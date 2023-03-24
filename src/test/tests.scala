/*
    Serpentine, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import probably.*
import rudiments.*
import turbulence.*
import gossamer.*
import larceny.*

object Tests extends Suite(t"Serpentine Tests"):
  def run(): Unit =
    test(t"parse simple relative path"):
      Relative.parse(t"peer")
    .assert(_ == Relative(0, List(t"peer")))

    test(t"parse three-part relative subpath"):
      Relative.parse(t"path/to/child")
    .assert(_ == Relative(0, List(t"path", t"to", t"child")))

    test(t"parse parent relative path"):
      Relative.parse(t"..")
    .assert(_ == Relative(1, List()))

    test(t"parse ancestor relative path"):
      Relative.parse(t"../../..")
    .assert(_ == Relative(3, List()))
  
    test(t"parse relative link to current path"):
      Relative.parse(t".")
    .assert(_ == Relative(0, List()))
    
    test(t"parse relative link to uncle path"):
      Relative.parse(t"../path")
    .assert(_ == Relative(1, List(t"path")))
    
    test(t"parse relative link to cousin path"):
      Relative.parse(t"../path/child")
    .assert(_ == Relative(1, List(t"path", t"child")))

    suite(t"Show paths"):
      test(t"show simple relative path"):
        (? / p"hello" / p"world").show
      .assert(_ == t"hello/world")
    
    suite(t"invalid paths"):

      test(t"con is not a valid path name"):
        captureCompileErrors(p"con").map(_.message)
      .assert(_ == List("serpentine: the pathname con (with or without an extension) is not valid on Windows"))
      
      test(t"con.xyz is not a valid path name"):
        captureCompileErrors(p"con.xyz").map(_.message)
      .assert(_ == List("serpentine: the pathname con.xyz (with or without an extension) is not valid on Windows"))
      
      test(t"lpt4 is not a valid path name"):
        captureCompileErrors(p"lpt4").map(_.message)
      .assert(_ == List("serpentine: the pathname lpt4 (with or without an extension) is not valid on Windows"))
      
      test(t"LPT4 is not a valid path name"):
        captureCompileErrors(p"lpt4").map(_.message)
      .assert(_ == List("serpentine: the pathname lpt4 (with or without an extension) is not valid on Windows"))
      
      test(t"Empty path is invalid"):
        captureCompileErrors(p"").map(_.message)
      .assert(_ == List("serpentine: a pathname cannot be empty"))
      
      test(t"Current directory (.) is not a valid path"):
        captureCompileErrors(p".").map(_.message)
      .assert(_ == List("serpentine: a pathname cannot be the string \".\""))
      
      test(t"Parent directory (..) is not a valid path"):
        captureCompileErrors(p"..").map(_.message)
      .assert(_ == List("serpentine: a pathname cannot be the string \"..\""))
          
