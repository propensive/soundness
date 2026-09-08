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
┃    Soundness, version 0.64.0.                                                                    ┃
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
  case "HOME" => "/home/work"
  case _       => Unset

given System =
  case "user.home" => "/home/work"
  case _            => ""

given Text is Instantiable across Paths from Text = identity(_)

object Tests extends Suite(m"Imperial tests"):
  def run(): Unit =

    test(m"Home directory"):
      Home()
    . assert(_ == "/home/work")

    test(m"Cache directory"):
      Home.Cache()
    . assert(_ == "/home/work/.cache")

    test(m"~/.local/bin path"):
      Home.Local.Bin()
    . assert(_ == "/home/work/.local/bin")

    test(m"/ path"):
      Base()
    . assert(_ == "/")

    test(m"/boot path"):
      Base.Boot()
    . assert(_ == "/boot")

    test(m"/efi path"):
      Base.Efi()
    . assert(_ == "/efi")

    test(m"/etc path"):
      Base.Etc()
    . assert(_ == "/etc")

    test(m"/home path"):
      Base.Home()
    . assert(_ == "/home")

    test(m"/root path"):
      Base.Root()
    . assert(_ == "/root")

    test(m"/srv path"):
      Base.Srv()
    . assert(_ == "/srv")

    test(m"/tmp path"):
      Base.Tmp()
    . assert(_ == "/tmp")

    test(m"/usr path"):
      Base.Usr()
    . assert(_ == "/usr")

    test(m"/usr/share path"):
      Base.Usr.Share()
    . assert(_ == "/usr/share")

    test(m"/usr/bin path"):
      Base.Usr.Bin()
    . assert(_ == "/usr/bin")

    test(m"/usr/share/doc path"):
      Base.Usr.Share.Doc()
    . assert(_ == "/usr/share/doc")

    test(m"/usr/share/factory/etc path"):
      Base.Usr.Share.Factory.Etc()
    . assert(_ == "/usr/share/factory/etc")

    test(m"/proc PID path"):
      val proc = Base.Proc(Pid(2000))
      proc()
    . assert(_ == "/proc/2000")
