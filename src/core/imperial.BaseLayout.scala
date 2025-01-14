/*
    Imperial, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

object BaseLayout:
  case class Dir(home: Boolean, path: List[Text]):
    @targetName("child")
    infix def / (name: Text): Dir = Dir(home, name :: path)

    def render(homeDir: Text): Text =
      val slash = if path.isEmpty then t"" else t"/"
      t"${if home then homeDir else t""}$slash${path.reverse.join(t"/")}"

case class BaseLayout(private val part: Optional[Text], readOnly: Boolean = false)
   (using baseDir: BaseLayout.Dir):

  def absolutePath(using Environment, SystemProperties)
          : Text raises EnvironmentError raises SystemPropertyError =

    val home: Text = Environment.home[Text].or(Properties.user.home[Text]())
    val home2: Text = if home.ends(t"/") then home.skip(1, Rtl) else home
    part.let(baseDir / _).or(baseDir).render(home2)

  given BaseLayout.Dir = BaseLayout.Dir(baseDir.home, part.let(_ :: baseDir.path).or(baseDir.path))

  def apply[PathType: SpecificPath]()(using SystemProperties, Environment)
          : PathType raises SystemPropertyError raises EnvironmentError =

    val path: Text = absolutePath
    SpecificPath(path)
