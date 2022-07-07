/*
    Imperial, version 0.4.0. Copyright 2022-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package imperial

import probably.*
import gossamer.*
import turbulence.*
import rudiments.*
import stdouts.stdout
import anticipation.integration.javaIo
import eucalyptus.*

given Log(Everything |-> SystemOut)

given Environment({
  case t"HOME" => Some(t"/home/work")
  case _       => None
})

object Tests extends Suite(t"Imperial tests"):
  def run(using Runner): Unit =
    println("Hello")
    test(t"Home directory"):
      Home().getAbsolutePath
    .assert(_ == t"/home/work")

    test(t"Cache directory"):
      Home.Cache().getAbsolutePath
    .assert(_ == t"/home/work/.cache")
    
    test(t"~/.local/bin path"):
      Home.Local.Bin().getAbsolutePath
    .assert(_ == t"/home/work/.local/bin")
