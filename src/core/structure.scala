/*
    Imperial, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import ambience.*
import perforate.*
import anticipation.*
import gossamer.*

object BaseLayout:
  case class Dir(home: Boolean, path: List[Text]):
    @targetName("child")
    def /(name: Text): Dir = Dir(home, name :: path)
    
    def render(homeDir: Text): Text =
      val slash = if path.isEmpty then t"" else t"/"
      t"${if home then homeDir else t""}$slash${path.reverse.join(t"/")}"

case class BaseLayout
    (private val part: Maybe[Text], readOnly: Boolean = false)
    (using baseDir: BaseLayout.Dir):
  
  def absolutePath
      (using Environment, Raises[EnvironmentError], SystemProperties,
          Raises[SystemPropertyError])
      : Text =
    val home: Text = Environment.home[Text].or(Properties.user.home[Text]())
    val home2: Text = if home.ends(t"/") then home.drop(1, Rtl) else home
    part.let(baseDir / _).or(baseDir).render(home2)

  given newBaseDir: BaseLayout.Dir = BaseLayout.Dir(baseDir.home, part.let(_ :: baseDir.path).or(baseDir.path))

  def apply[PathType]()(using SpecificPath[PathType], SystemProperties, Raises[SystemPropertyError], Environment, Raises[EnvironmentError]): PathType =
    val path: Text = absolutePath

    SpecificPath(path)

object Base extends BaseLayout(Unset)(using BaseLayout.Dir(false, Nil)):
  override def apply[PathType: SpecificPath]()(using SystemProperties, Raises[SystemPropertyError], Environment, Raises[EnvironmentError]): PathType =
    SpecificPath(t"/")

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

object Home extends BaseLayout(Unset)(using BaseLayout.Dir(true, Nil)):
  object Cache extends BaseLayout(t".cache")
  object Config extends BaseLayout(t".config")
  
  object Local extends BaseLayout(t".local"):
    object Bin extends BaseLayout(t"bin")
    object Lib extends BaseLayout(t"lib")
    object Share extends BaseLayout(t"share")
    object State extends BaseLayout(t"state")
