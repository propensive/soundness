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
package jacinta

import scala.annotation.*
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import urticose.*
import vacuous.*

object JsonPointer extends Root(""):
  type Plane = JsonPointer

  trait Registry:
    private val documents: scm.HashMap[HttpUrl, Json] = scm.HashMap()

    def update(url: HttpUrl, document: Json): Unit = documents(url) = document
    def apply(url: HttpUrl): Optional[Json] = documents.at(url).or(lookup(url))
    protected def lookup(url: HttpUrl): Optional[Json]

  given navigable: [ordinal <: Ordinal] => ordinal is Navigable on JsonPointer = _.n0.toString.tt
  given admissible: [ordinal <: Ordinal] => ordinal is Admissible on JsonPointer = _ => ()
  given admissible2: [text <: Text] => text is Admissible on JsonPointer = _ => ()

  given filesystem: JsonPointer is Filesystem:
    override def escape(text: Text): Text = text.sub("~", "~0").sub("/", "~1")
    override def unescape(text: Text): Text = text.sub("~1", "/").sub("~0", "~")

    val parent: Text = ".."
    val self: Text = "#"
    val separator: Text = "/"

  given JsonPointer is Encodable in Text = pointer =>
    t"${pointer.url.let(_.encode).or(t"")}#${pointer.path}"

  given divisible: JsonPointer is Divisible by Text to JsonPointer =
    (pointer, segment) => JsonPointer(pointer.url, pointer.path / segment)

  given divisible2: JsonPointer is Divisible by Ordinal to JsonPointer =
    (pointer, segment) => JsonPointer(pointer.url, pointer.path / segment)

case class JsonPointer(url: Optional[HttpUrl] = Unset, path: Path on JsonPointer = JsonPointer):
  def apply(using registry: JsonPointer.Registry)(document: Json): Json raises JsonPointerError =
    url.let(registry(_).lest(JsonPointerError())).or(document)

  def apply(ordinal: Ordinal): JsonPointer = JsonPointer(url, path / ordinal)
  def apply(text: Text): JsonPointer = JsonPointer(url, path / text)
