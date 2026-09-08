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

import ambience.*
import anticipation.*
import contingency.*
import gossamer.*
import guillotine.*
import prepositional.*
import vacuous.*

object Base extends BaseLayout(Unset)(using BaseLayout.Dir(false, Nil)):
  object Boot extends BaseLayout[Mono["boot"]]("boot", readOnly = true)
  object Efi extends BaseLayout[Mono["efi"]]("efi", readOnly = true)
  object Etc extends BaseLayout[Mono["etc"]]("etc")
  object Home extends BaseLayout[Mono["home"]]("home")
  object Root extends BaseLayout[Mono["roto"]]("root")
  object Srv extends BaseLayout[Mono["srv"]]("srv")
  object Tmp extends BaseLayout[Mono["tmp"]]("tmp")

  object Run extends BaseLayout[Mono["run"]]("run"):
    object Log extends BaseLayout[("log", "run")]("log")
    object User extends BaseLayout[("user", "run")]("user")

  object Usr extends BaseLayout[Mono["usr"]]("usr", readOnly = true):
    object Bin extends BaseLayout[("bin", "usr")]("bin", readOnly = true)
    object Include extends BaseLayout[("include", "usr")]("include", readOnly = true)
    object Lib extends BaseLayout[("lib", "usr")]("lib", readOnly = true)

    object Share extends BaseLayout("share", readOnly = true):
      object Doc extends BaseLayout("doc", readOnly = true)

      object Factory extends BaseLayout("factory", readOnly = true):
        object Etc extends BaseLayout("etc", readOnly = true)
        object Var extends BaseLayout("var", readOnly = true)

  object Var extends BaseLayout[Mono["var"]]("var"):
    object Cache extends BaseLayout[("cache", "var")]("cache")
    object Lib extends BaseLayout[("lib", "var")]("lib")
    object Log extends BaseLayout[("log", "var")]("log")
    object Spool extends BaseLayout[("spool", "var")]("spool")
    object Tmp extends BaseLayout[("tmp", "var")]("tmp")

  object Dev extends BaseLayout[Mono["dev"]]("dev"):
    object Shm extends BaseLayout[("shm", "dev")]("shm")

  object Proc extends BaseLayout[Mono["proc"]]("proc"):
    def apply(pid: Pid): BaseLayout[(Pid, "proc")] =
      BaseLayout(pid.value.toString.tt, readOnly = true)

    object Sys extends BaseLayout[("sys", "proc")]("sys", readOnly = true)

  object Sys extends BaseLayout[Mono["sys"]]("sys", readOnly = true)
