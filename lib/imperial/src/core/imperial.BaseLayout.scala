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

import scala.caps

import ambience.*
import anticipation.*
import beneficence.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import vacuous.*

object BaseLayout:
  case class Dir(home: Boolean, path: List[Text]) extends Findable:
    @targetName("child")
    infix def / (name: Text): Dir = Dir(home, name :: path)

    def render(homeDir: Text): Text =
      // The filesystem root (`Base`): not home-relative and no path segments → `/`.
      if !home && path.nil then t"/"
      else
        val slash = if path.nil then t"" else t"/"
        t"${if home then homeDir else t""}$slash${path.stdlib.reverse.join(t"/")}"

// `caps.Pure`: a layout is pure path data (`part`, `readOnly`, `Dir`); the marker also keeps the
// `Home.type`/`Base.type` members of nested layouts' `Topic` tuples pure, without which the
// nested objects' parent types acquire capture variables their pure self types reject.
case class BaseLayout[topic <: Tuple](private val part: Optional[Text], readOnly: Boolean = false)
  ( using baseDir: BaseLayout.Dir )
extends caps.Pure:

  type Topic = topic

  inline def absolutePath(using Environment, System)
  :   Text raises EnvironmentError raises PropertyError =

    val dir = part.let(baseDir/_).or(baseDir)
    // Only home-relative layouts need `$HOME`; the `/`-rooted `Base.*` layouts render without it.
    val home2: Text =
      if !dir.home then t"" else
        val home: Text = Environment.home[Text]
        if home.ends(t"/") then home.skip(1, Rtl) else home

    dir.render(home2)


  given dir: BaseLayout.Dir =
    BaseLayout.Dir(baseDir.home, part.let { p => (p :: baseDir.path): List[Text] }.or(baseDir.path))


  // `inline` so the `raises` context-functions resolve at the call site: a non-inline `raises`
  // method called inside a deferred test block boxes its summoned tactic with the block's capability,
  // which cannot flow into the `raises` existential under capture checking.
  inline def apply[instantiable: Instantiable across Paths from Text]()
    ( using System, Environment )
  :   instantiable raises PropertyError raises EnvironmentError =

    val path: Text = absolutePath
    instantiable(path)
