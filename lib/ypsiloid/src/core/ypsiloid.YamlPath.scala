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
package ypsiloid

import scala.collection.mutable as scm

import anticipation.*
import beneficence.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
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

  given navigable: [ordinal <: Ordinal] => ordinal is Navigable on YamlPath =
    // `(ordinal: Ordinal)` widens the singleton-bounded parameter (case-2 pure-value box).
    ordinal => (ordinal: Ordinal).n0.toString.tt
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

  inline given interpolable: YamlPath is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   YamlPath =

      ${ypsiloid.internal.yamlPath[parts, origins]('insertions)}

  // Parses a same-document YAML path (`#`, `#/`, `#/a/b`), reporting the offset
  // of any error. Modelled on `jacinta.JsonPointer`'s decoder, with the same
  // RFC 6901 escaping.
  given decodable: (tactic: Tactic[YamlPath.Error])
  =>  ((YamlPath is Decodable in Text)^{tactic}) = text =>
    val string = text.s

    if string.isEmpty || string.charAt(0) != '#'
    then abort(YamlPath.Error(YamlPath.Error.Reason.ExpectedHash, 0))
    else if string.length > 1 && string.charAt(1) != '/'
    then abort(YamlPath.Error(YamlPath.Error.Reason.ExpectedSlash, 1))
    else
      var index = 1

      while index < string.length do
        if string.charAt(index) == '~' then
          val next = if index + 1 < string.length then string.charAt(index + 1) else ' '

          if next != '0' && next != '1'
          then abort(YamlPath.Error(YamlPath.Error.Reason.BadEscape, index))

        index += 1

      val segments = text.skip(1).cut(t"/").filter(_ != t"")

      // Build with root `/`, matching the derivation's `prepend`, so the
      // slashless encoder (`#${path}`) renders `#/a/b`. The whole-document path
      // (`#`) keeps the default empty root so it encodes back to `#`.
      if segments.nil then YamlPath()
      else
        val descent = segments.reverse.map(filesystem.unescape)
        YamlPath(Unset, Path[YamlPath, YamlPath.type, Tuple]("/", descent))

  given divisible: YamlPath is Divisible by Text to YamlPath =
    Divisible: (path, segment) => YamlPath(path.url, path.path / segment)

  given divisible2: YamlPath is Divisible by Ordinal to YamlPath =
    // An explicit anonymous class rather than the `Divisible:` factory: under capture checking
    // the factory's lambda parameter mints a fresh capture variable on the pure `Ordinal`
    // (a case-2 pure-value box, cf. scala/scala3#16978), and a plain SAM conversion crashes
    // genSJSIR for Scala.js. The explicit instance avoids both.
    new Divisible:
      type Self = YamlPath
      type Result = YamlPath
      type Operand = Ordinal

      def divide(path: YamlPath, segment: Ordinal): YamlPath =
        YamlPath(path.url, path.path / segment)

  // YamlPathError → YamlPath.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case UnknownDocument extends Reason(1)
      case ExpectedHash    extends Reason(2)
      case ExpectedSlash   extends Reason(3)
      case BadEscape       extends Reason(4)

    given communicable: Reason is Communicable =
      case Reason.UnknownDocument => m"the registry contains no document at the path's URL"
      case Reason.ExpectedHash    => m"a YAML path must begin with '#'"
      case Reason.ExpectedSlash   => m"a YAML path fragment must begin with '/'"
      case Reason.BadEscape       => m"a '~' in a YAML path must be followed by '0' or '1'"

  // `offset` is the character index, within the path text, where the error was
  // detected; consumers (e.g. the `yp"…"` interpolator) use it to position a
  // compile-time error precisely.
  case class Error(reason: YamlPath.Error.Reason, offset: Int)(using Diagnostics)
  extends fulminate.Error(546, reason.number)(m"the YAML path was not valid because $reason")

case class YamlPath(url: Optional[HttpUrl] = Unset, path: Path on YamlPath = YamlPath):
  def apply(using registry: YamlPath.Registry)(document: Yaml): Yaml raises YamlPath.Error =
    url.let(registry(_).lest(YamlPath.Error(YamlPath.Error.Reason.UnknownDocument, 0)))
    . or(document)

  def apply(ordinal: Ordinal): YamlPath = YamlPath(url, path / ordinal)
  def apply(text: Text): YamlPath = YamlPath(url, path / text)

  // Append `segment` at the root end of the path, leaving the rest of
  // the descent intact. Used by `Yaml`'s Wisteria derivation: each
  // outer `focus` block runs *after* the inner one (contingency's
  // try/finally order), and needs to push its label to the root side
  // of the accumulated path so `/parent/child` lands root-first. The
  // `apply(text)` / `apply(ordinal)` methods above go through
  // Serpentine's `/`, which adds at the leaf side — the wrong
  // direction for focus supplementing.
  private[ypsiloid] def prepend(segment: Text): YamlPath =
    YamlPath(url, Path[YamlPath, YamlPath.type, Tuple]("/", (path.descent :+ segment).to(List)))

  private[ypsiloid] def prepend(ordinal: Ordinal): YamlPath =
    YamlPath(url, Path[YamlPath, YamlPath.type, Tuple]("/", (path.descent :+ ordinal.n0.toString.tt).to(List)))
