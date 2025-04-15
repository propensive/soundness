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
┃    Soundness, version 0.27.0.                                                                    ┃
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
  override def apply[path: Instantiable across Paths from Text]()
                (using SystemProperties, Environment)
  :     path raises SystemPropertyError raises EnvironmentError =

    path(t"/")

  object Boot extends BaseLayout(t"boot", readOnly = true)
  object Efi extends BaseLayout(t"efi", readOnly = true)
  object Etc extends BaseLayout(t"etc")
  object Home extends BaseLayout(t"home")
  object Root extends BaseLayout(t"root")
  object Srv extends BaseLayout(t"srv")
  object Tmp extends BaseLayout(t"tmp")

  object Run extends BaseLayout(t"run"):
    object Log extends BaseLayout(t"log")

    object User extends BaseLayout(t"user"):
      def apply(uid: Long): BaseLayout = BaseLayout(uid.toString.tt)
      def current: BaseLayout = apply(com.sun.security.auth.module.UnixSystem().getUid())

  object Usr extends BaseLayout(t"usr", readOnly = true):
    object Bin extends BaseLayout(t"bin", readOnly = true)
    object Include extends BaseLayout(t"include", readOnly = true)
    object Lib extends BaseLayout(t"lib", readOnly = true)
    object Share extends BaseLayout(t"share", readOnly = true):
      object Doc extends BaseLayout(t"doc", readOnly = true)

      object Factory extends BaseLayout(t"factory", readOnly = true):
        object Etc extends BaseLayout(t"etc", readOnly = true)
        object Var extends BaseLayout(t"var", readOnly = true)

  object Var extends BaseLayout(t"var"):
    object Cache extends BaseLayout(t"cache")
    object Lib extends BaseLayout(t"lib")
    object Log extends BaseLayout(t"log")
    object Spool extends BaseLayout(t"spool")
    object Tmp extends BaseLayout(t"tmp")

  object Dev extends BaseLayout(t"dev"):
    object Shm extends BaseLayout(t"shm")

  object Proc extends BaseLayout(t"proc"):
    def apply(pid: Pid): BaseLayout = BaseLayout(pid.value.toString.tt, readOnly = true)
    object Sys extends BaseLayout(t"sys", readOnly = true)

  object Sys extends BaseLayout(t"sys", readOnly = true)
