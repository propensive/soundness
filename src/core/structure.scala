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

import rudiments.*
import clairvoyant.*

object BaseLayout:
  case class Dir(path: String)

case class BaseLayout(private val path: String, private val env: Maybe[String] = Unset,
                            readOnly: Boolean = false)
                       (using baseDir: BaseLayout.Dir):
  def absolutePath: String = s"${baseDir.path}/$path"
  given newBaseDir: BaseLayout.Dir = BaseLayout.Dir(absolutePath)
  
  def apply[T]()(using DirectoryProvider[T]): T =
    val path: String = env.option match
      case None      => absolutePath
      case Some(env) => Option(System.getenv(env)).map(_.nn) match
        case None       => absolutePath
        case Some(path) => path

    summon[DirectoryProvider[T]].make(path)

object Root extends BaseLayout("")(using BaseLayout.Dir("")):
  override def absolutePath: String = ""
  override def apply[T]()(using DirectoryProvider[T]): T = summon[DirectoryProvider[T]].make("/")
  object Boot extends BaseLayout("boot")
  object Efi extends BaseLayout("efi")
  object Etc extends BaseLayout("etc")
  object Home extends BaseLayout("home")
  object Srv extends BaseLayout("srv")
  object Tmp extends BaseLayout("tmp")
  object Run extends BaseLayout("run"):
    object Log extends BaseLayout("log")
    object User extends BaseLayout("user")
  object Usr extends BaseLayout("usr"):
    object Bin extends BaseLayout("bin")
    object Include extends BaseLayout("include")
    object Lib extends BaseLayout("lib")
    object Share extends BaseLayout("share"):
      object Doc extends BaseLayout("doc")
      object Factory extends BaseLayout("factory"):
        object Etc extends BaseLayout("etc")
        object Var extends BaseLayout("var")
  object Var extends BaseLayout("var"):
    object Cache extends BaseLayout("cache")
    object Lib extends BaseLayout("lib")
    object Log extends BaseLayout("log")
    object Spool extends BaseLayout("spool")
    object Tmp extends BaseLayout("tmp", "TMPDIR")
  object Dev extends BaseLayout("dev"):
    object Shm extends BaseLayout("shm")
  object Proc extends BaseLayout("proc"):
    object Sys extends BaseLayout("sys")
  object Sys extends BaseLayout("sys")

object Home extends BaseLayout("~")(using BaseLayout.Dir(System.getenv("HOME").nn)):
  override def absolutePath: String = System.getenv("HOME").nn

  object Cache extends BaseLayout(".cache", "XDG_CACHE_HOME")
  object Config extends BaseLayout(".config", "XDG_CONFIG_HOME")
  object Local extends BaseLayout(".local"):
    object Bin extends BaseLayout("bin")
    object Lib extends BaseLayout("lib")
    object Share extends BaseLayout("share", "XDG_DATA_HOME")
    object State extends BaseLayout("state", "XDG_STATE_HOME")