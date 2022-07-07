/*
    Serpentine, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> SystemOut)

object Tests extends Suite(t"Serpentine Tests"):
  def run(using Runner): Unit =
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

    suite(t"show paths"):
      test(t"show simple relative path"):
        (? / p"hello" / p"world").show
      .assert(_ == t"hello/world")
    