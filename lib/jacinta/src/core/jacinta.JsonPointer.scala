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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import beneficence.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import urticose.*
import vacuous.*

object JsonPointer extends Root(""):
  type Plane = JsonPointer

  trait Registry extends Findable:
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

    val name: Text = "JSON"
    val parent: Text = ".."
    val self: Text = "#"
    val separator: Text = "/"

  given JsonPointer is Encodable in Text = pointer =>
    val url = pointer.url.let(_.encode).or(t"")

    if pointer.path.descent.length == 0 then t"$url#"
    else t"$url#/${pointer.path}"

  inline given interpolable: JsonPointer is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   JsonPointer =

      ${jacinta.internal.jsonPointer[parts, origins]('insertions)}

  // Parses a same-document JSON reference (`#`, `#/`, `#/a/b`), reporting the
  // offset of any error. URL-bearing references begin with a non-`#` character
  // and so are rejected as `ExpectedHash`; same-document refs are all OpenAPI's
  // `$ref`s use, and are JSON Pointer fragments per RFC 6901.
  given decodable: Tactic[JsonPointerError] => JsonPointer is Decodable in Text = text =>
    val string = text.s

    if string.isEmpty || string.charAt(0) != '#'
    then abort(JsonPointerError(JsonPointerError.Reason.ExpectedHash, 0))
    else if string.length > 1 && string.charAt(1) != '/'
    then abort(JsonPointerError(JsonPointerError.Reason.ExpectedSlash, 1))
    else
      var index = 1

      while index < string.length do
        if string.charAt(index) == '~' then
          val next = if index + 1 < string.length then string.charAt(index + 1) else ' '

          if next != '0' && next != '1'
          then abort(JsonPointerError(JsonPointerError.Reason.BadEscape, index))

        index += 1

      text.skip(1).cut(t"/").filter(_ != t"").foldLeft(JsonPointer(): JsonPointer):
        (pointer, segment) => pointer(filesystem.unescape(segment))

  given divisible: JsonPointer is Divisible by Text to JsonPointer =
    Divisible: (pointer, segment) => JsonPointer(pointer.url, pointer.path / segment)

  given divisible2: JsonPointer is Divisible by Ordinal to JsonPointer =
    Divisible: (pointer, segment) => JsonPointer(pointer.url, pointer.path / segment)

case class JsonPointer(url: Optional[HttpUrl] = Unset, path: Path on JsonPointer = JsonPointer):
  def apply(using registry: JsonPointer.Registry)(document: Json): Json raises JsonPointerError =
    url.let(registry(_).lest(JsonPointerError(JsonPointerError.Reason.UnknownDocument, 0)))
    . or(document)

  def apply(ordinal: Ordinal): JsonPointer = JsonPointer(url, path / ordinal)
  def apply(text: Text): JsonPointer = JsonPointer(url, path / text)
