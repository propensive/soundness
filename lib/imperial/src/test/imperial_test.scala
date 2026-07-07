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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package imperial

import soundness.*

import strategies.throwUnsafely

given Environment =
  case t"HOME" => t"/home/work"
  case _       => Unset

given System =
  case t"user.home" => t"/home/work"
  case _            => t""

given Text is Instantiable across Paths from Text = identity(_)

object Tests extends Suite(m"Imperial tests"):
  def run(): Unit =

    test(m"Home directory"):
      Home()
    . assert(_ == t"/home/work")

    test(m"Cache directory"):
      Home.Cache()
    . assert(_ == t"/home/work/.cache")

    test(m"~/.local/bin path"):
      Home.Local.Bin()
    . assert(_ == t"/home/work/.local/bin")

    test(m"/ path"):
      Base()
    . assert(_ == t"/")

    test(m"/boot path"):
      Base.Boot()
    . assert(_ == t"/boot")

    test(m"/efi path"):
      Base.Efi()
    . assert(_ == t"/efi")

    test(m"/etc path"):
      Base.Etc()
    . assert(_ == t"/etc")

    test(m"/home path"):
      Base.Home()
    . assert(_ == t"/home")

    test(m"/root path"):
      Base.Root()
    . assert(_ == t"/root")

    test(m"/srv path"):
      Base.Srv()
    . assert(_ == t"/srv")

    test(m"/tmp path"):
      Base.Tmp()
    . assert(_ == t"/tmp")

    test(m"/usr path"):
      Base.Usr()
    . assert(_ == t"/usr")

    test(m"/usr/share path"):
      Base.Usr.Share()
    . assert(_ == t"/usr/share")

    test(m"/usr/bin path"):
      Base.Usr.Bin()
    . assert(_ == t"/usr/bin")

    test(m"/usr/share/doc path"):
      Base.Usr.Share.Doc()
    . assert(_ == t"/usr/share/doc")

    test(m"/usr/share/factory/etc path"):
      Base.Usr.Share.Factory.Etc()
    . assert(_ == t"/usr/share/factory/etc")

    test(m"/proc PID path"):
      Base.Proc(Pid(2000))()
    . assert(_ == t"/proc/2000")
