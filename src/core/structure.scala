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
import anticipation.*

object BaseLayout:
  case class Dir(home: Boolean, path: String)

case class BaseLayout(private val part: String, private val envVar: Maybe[String] = Unset,
                          readOnly: Boolean = false)
                     (using baseDir: BaseLayout.Dir):
  def absolutePath(using env: Environment): String =
    if baseDir.home then
      val home: Text = unsafely(env(Text("HOME")).otherwise(env.property(Text("user.home"))))
      val slash = if home.s.endsWith("/") then "" else "/"
      s"$home$slash${baseDir.path}/$part" else s"${baseDir.path}/$part"

  given newBaseDir: BaseLayout.Dir = BaseLayout.Dir(baseDir.home, s"${baseDir.path}/$part")

  def apply[T]()(using PathProvider[T], Environment): T =
    val path: String = envVar.option match
      case None         => absolutePath
      case Some(envVar) => summon[Environment](Text(envVar)).envelop(absolutePath)(_.s)

    summon[PathProvider[T]].makePath(path, readOnly = readOnly) match
      case None      => throw RuntimeException("failed to parse: '"+path+"'")
      case Some(dir) => dir

object Xdg extends BaseLayout("")(using BaseLayout.Dir(false, "")):
  override def apply[T]()(using PathProvider[T], Environment): T =
    summon[PathProvider[T]].makePath("/", readOnly = true).get

  object Boot extends BaseLayout("boot", readOnly = true)
  object Efi extends BaseLayout("efi", readOnly = true)
  object Etc extends BaseLayout("etc")
  object Home extends BaseLayout("home")
  object Root extends BaseLayout("root")
  object Srv extends BaseLayout("srv")
  object Tmp extends BaseLayout("tmp")
  object Run extends BaseLayout("run"):
    object Log extends BaseLayout("log")
    
    object User extends BaseLayout("user"):
      def apply(uid: Long): BaseLayout = BaseLayout(uid.toString)
      def current: BaseLayout = apply(com.sun.security.auth.module.UnixSystem().getUid())
  
  object Usr extends BaseLayout("usr", readOnly = true):
    object Bin extends BaseLayout("bin", readOnly = true)
    object Include extends BaseLayout("include", readOnly = true)
    object Lib extends BaseLayout("lib", readOnly = true)
    object Share extends BaseLayout("share", readOnly = true):
      object Doc extends BaseLayout("doc", readOnly = true)
      object Factory extends BaseLayout("factory", readOnly = true):
        object Etc extends BaseLayout("etc", readOnly = true)
        object Var extends BaseLayout("var", readOnly = true)
  object Var extends BaseLayout("var"):
    object Cache extends BaseLayout("cache")
    object Lib extends BaseLayout("lib")
    object Log extends BaseLayout("log")
    object Spool extends BaseLayout("spool")
    object Tmp extends BaseLayout("tmp", "TMPDIR")
  object Dev extends BaseLayout("dev"):
    object Shm extends BaseLayout("shm")
  object Proc extends BaseLayout("proc"):
    def apply(pid: Pid): BaseLayout = BaseLayout(pid.value.toString, readOnly = true)
    object Sys extends BaseLayout("sys", readOnly = true)
  object Sys extends BaseLayout("sys", readOnly = true)

object Home extends BaseLayout("")(using BaseLayout.Dir(true, "")):
  object Cache extends BaseLayout(".cache", "XDG_CACHE_HOME")
  object Config extends BaseLayout(".config", "XDG_CONFIG_HOME")
  object Local extends BaseLayout(".local"):
    object Bin extends BaseLayout("bin")
    object Lib extends BaseLayout("lib")
    object Share extends BaseLayout("share", "XDG_DATA_HOME")
    object State extends BaseLayout("state", "XDG_STATE_HOME")