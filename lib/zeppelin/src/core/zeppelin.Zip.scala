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
package zeppelin

import java.nio.file as jnf

import anticipation.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import turbulence.*

object Zip:
  type Rules =
    MustNotContain["\\"] & MustNotContain["\""] & MustNotContain["/"] & MustNotContain[":"]
    & MustNotContain["*"] & MustNotContain["?"] & MustNotContain["<"] & MustNotContain[">"]
    & MustNotContain["|"]

  inline given compliant: Linux is Compliant on Zip = !!
  inline given compliant2: MacOs is Compliant on Zip = !!
  inline given nominative: Zip is Nominative under Rules = !!
  given submissible: %.type is Submissible on Zip = _ => ()

  class ZipRoot() extends Root(t""):
    type Plane = Zip

  given filesystem: Zip is Filesystem:
    type UniqueRoot = false

    val name: Text = "ZIP"
    val separator: Text = "/"
    val self: Text = "."
    val parent: Text = ".."

  given radical: %.type is Radical:
    type Plane = Zip

    def length(text: Text): Int = 0
    def decode(text: Text): %.type = %
    def encode(root: %.type): Text = t""


  object Entry:
    def apply[resource: Streamable by Data](path: Path on Zip, resource: resource): Entry =
      new Entry(path, () => resource.stream[Data])

    given streamable: Entry is Streamable by Data = Streamable.stream[Data].contramap(_.content())

    // 00:00:00, 1 January 2000
    val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L).nn

  case class Entry(ref: Path on Zip, content: () => Stream[Data])

sealed trait Zip
