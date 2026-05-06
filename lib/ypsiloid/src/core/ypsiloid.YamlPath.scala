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
package ypsiloid

import scala.collection.mutable as scm

import anticipation.*
import beneficence.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import urticose.*
import vacuous.*

// A YAML Path identifies a node within a YAML document. Modelled on
// `jacinta.JsonPointer` and using the same RFC 6901 escaping (`~0`
// for `~`, `~1` for `/`) so paths interoperate cleanly with JSON
// Pointers when YAML is treated as JSON. Non-string mapping keys are
// not addressable; attempting to navigate through one is undefined.
object YamlPath extends Root(""):
  type Plane = YamlPath

  trait Registry extends Findable:
    private val documents: scm.HashMap[HttpUrl, Yaml] = scm.HashMap()

    def update(url: HttpUrl, document: Yaml): Unit = documents(url) = document
    def apply(url: HttpUrl): Optional[Yaml] = documents.at(url).or(lookup(url))
    protected def lookup(url: HttpUrl): Optional[Yaml]

  given navigable: [ordinal <: Ordinal] => ordinal is Navigable on YamlPath = _.n0.toString.tt
  given admissible: [ordinal <: Ordinal] => ordinal is Admissible on YamlPath = _ => ()
  given admissible2: [text <: Text] => text is Admissible on YamlPath = _ => ()

  given filesystem: YamlPath is Filesystem:
    override def escape(text: Text): Text = text.sub("~", "~0").sub("/", "~1")
    override def unescape(text: Text): Text = text.sub("~1", "/").sub("~0", "~")

    val name: Text = "YAML"
    val parent: Text = ".."
    val self: Text = "#"
    val separator: Text = "/"

  given YamlPath is Encodable in Text = path =>
    t"${path.url.let(_.encode).or(t"")}#${path.path}"

  given divisible: YamlPath is Divisible by Text to YamlPath =
    (path, segment) => YamlPath(path.url, path.path / segment)

  given divisible2: YamlPath is Divisible by Ordinal to YamlPath =
    (path, segment) => YamlPath(path.url, path.path / segment)

case class YamlPath(url: Optional[HttpUrl] = Unset, path: Path on YamlPath = YamlPath):
  def apply(using registry: YamlPath.Registry)(document: Yaml): Yaml raises YamlPathError =
    url.let(registry(_).lest(YamlPathError(YamlPathError.Reason.UnknownDocument)))
    . or(document)

  def apply(ordinal: Ordinal): YamlPath = YamlPath(url, path / ordinal)
  def apply(text: Text): YamlPath = YamlPath(url, path / text)
