/*
    Imperial, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import ambience.*
import anticipation.*
import gossamer.*
import perforate.*
import probably.*
import rudiments.*
import vacuous.*

import errorHandlers.throwUnsafely

given Environment =
  case t"HOME" => t"/home/work"
  case _       => Unset

given SystemProperties =
  case t"user.home" => t"/home/work"
  case _            => t""

given SpecificPath[Text] = identity(_)

object Tests extends Suite(t"Imperial tests"):
  def run(): Unit =
    
    test(t"Home directory"):
      Home()
    .assert(_ == t"/home/work")

    test(t"Cache directory"):
      Home.Cache()
    .assert(_ == t"/home/work/.cache")
    
    test(t"~/.local/bin path"):
      Home.Local.Bin()
    .assert(_ == t"/home/work/.local/bin")
    
    test(t"/ path"):
      Base()
    .assert(_ == t"/")

    test(t"/boot path"):
      Base.Boot()
    .assert(_ == t"/boot")
    
    test(t"/efi path"):
      Base.Efi()
    .assert(_ == t"/efi")
    
    test(t"/etc path"):
      Base.Etc()
    .assert(_ == t"/etc")
    
    test(t"/home path"):
      Base.Home()
    .assert(_ == t"/home")
    
    test(t"/root path"):
      Base.Root()
    .assert(_ == t"/root")
    
    test(t"/srv path"):
      Base.Srv()
    .assert(_ == t"/srv")
    
    test(t"/tmp path"):
      Base.Tmp()
    .assert(_ == t"/tmp")
    
    test(t"/usr path"):
      Base.Usr()
    .assert(_ == t"/usr")
    
    test(t"/usr/share path"):
      Base.Usr.Share()
    .assert(_ == t"/usr/share")
    
    test(t"/usr/bin path"):
      Base.Usr.Bin()
    .assert(_ == t"/usr/bin")
    
    test(t"/usr/share/doc path"):
      Base.Usr.Share.Doc()
    .assert(_ == t"/usr/share/doc")
    
    test(t"/usr/share/factory/etc path"):
      Base.Usr.Share.Factory.Etc()
    .assert(_ == t"/usr/share/factory/etc")
    
    test(t"/proc PID path"):
      Base.Proc(Pid(2000))()
    .assert(_ == t"/proc/2000")
