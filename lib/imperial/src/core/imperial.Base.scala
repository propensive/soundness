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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import proscenium.*
import vacuous.*

object Base extends BaseLayout(Unset)(using BaseLayout.Dir(false, Nil)):

  override def apply[path: Instantiable across Paths from Text]()
    ( using System, Environment )
  :   path raises PropertyError raises EnvironmentError =

      path(t"/")


  object Boot extends BaseLayout[Mono["boot"]](t"boot", readOnly = true)
  object Efi extends BaseLayout[Mono["efi"]](t"efi", readOnly = true)
  object Etc extends BaseLayout[Mono["etc"]](t"etc")
  object Home extends BaseLayout[Mono["home"]](t"home")
  object Root extends BaseLayout[Mono["roto"]](t"root")
  object Srv extends BaseLayout[Mono["srv"]](t"srv")
  object Tmp extends BaseLayout[Mono["tmp"]](t"tmp")

  object Run extends BaseLayout[Mono["run"]](t"run"):
    object Log extends BaseLayout[("log", "run")](t"log")
    object User extends BaseLayout[("user", "run")](t"user")

  object Usr extends BaseLayout[Mono["usr"]](t"usr", readOnly = true):
    object Bin extends BaseLayout[("bin", "usr")](t"bin", readOnly = true)
    object Include extends BaseLayout[("include", "usr")](t"include", readOnly = true)
    object Lib extends BaseLayout[("lib", "usr")](t"lib", readOnly = true)
    object Share extends BaseLayout(t"share", readOnly = true):
      object Doc extends BaseLayout(t"doc", readOnly = true)

      object Factory extends BaseLayout(t"factory", readOnly = true):
        object Etc extends BaseLayout(t"etc", readOnly = true)
        object Var extends BaseLayout(t"var", readOnly = true)

  object Var extends BaseLayout[Mono["var"]](t"var"):
    object Cache extends BaseLayout[("cache", "var")](t"cache")
    object Lib extends BaseLayout[("lib", "var")](t"lib")
    object Log extends BaseLayout[("log", "var")](t"log")
    object Spool extends BaseLayout[("spool", "var")](t"spool")
    object Tmp extends BaseLayout[("tmp", "var")](t"tmp")

  object Dev extends BaseLayout[Mono["dev"]](t"dev"):
    object Shm extends BaseLayout[("shm", "dev")](t"shm")

  object Proc extends BaseLayout[Mono["proc"]](t"proc"):
    def apply(pid: Pid): BaseLayout[(Pid, "proc")] =
      BaseLayout(pid.value.toString.tt, readOnly = true)

    object Sys extends BaseLayout[("sys", "proc")](t"sys", readOnly = true)

  object Sys extends BaseLayout[Mono["sys"]](t"sys", readOnly = true)
