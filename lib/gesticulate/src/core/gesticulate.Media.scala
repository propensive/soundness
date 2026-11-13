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
package gesticulate

import scala.language.dynamics

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import zephyrine.*

object Media:
  // Detects the media type of content: `cursor match case Media["image/gif"](gif) => …` matches
  // if the contextual `"image/gif" is Detectable` recognises the content, and binds a
  // `Detection` which records what was established. A cursor is left where it was found whether
  // or not the case matches, so a later case, and the code which follows, see all of it.
  //
  // A second type argument demands a certainty: `Media["image/gif", Likely](gif)` compiles only
  // if a detector which establishes at least `Likely` is in scope.
  //
  // An `Expanse`, or `Data` in memory, has random access, which lets a format be detected by
  // something a stream cannot reach, such as the index at the end of a ZIP file.
  transparent inline def unapply[label <: Label](scrutinee: Cursor[Data, {}]^): Option[Any] =
    Detectable.cursor[label, Plausible](scrutinee)

  transparent inline def unapply[label <: Label, floor <: Plausible]
    ( scrutinee: Cursor[Data, {}]^ )
  :   Option[Any] =

    Detectable.cursor[label, floor](scrutinee)

  transparent inline def unapply[label <: Label](scrutinee: Expanse^): Option[Any] =
    Detectable.expanse[label, Plausible](scrutinee)

  transparent inline def unapply[label <: Label, floor <: Plausible](scrutinee: Expanse^)
  :   Option[Any] =

    Detectable.expanse[label, floor](scrutinee)

  transparent inline def unapply[label <: Label](scrutinee: Data): Option[Any] =
    Detectable.expanse[label, Plausible](Detectable.memory(scrutinee))

  transparent inline def unapply[label <: Label, floor <: Plausible](scrutinee: Data)
  :   Option[Any] =

    Detectable.expanse[label, floor](Detectable.memory(scrutinee))

  private val plain: MediaType = MediaType(Group.Text, Subtype.Standard(t"plain"))

  given text: Text is Media:
    extension (value: Text)
      def mediaType: MediaType = plain

  given nominable: [nominable: Nominable] => nominable is Media:
    extension (value: nominable)
      def mediaType: MediaType =
        val parts = nominable.name(value).cut(t".").stdlib
        Extensions.guess(parts.last)

  object Group:
    given inspectable: Group is Inspectable = _.name
    given showable: Group is Showable = _.name.lower

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model, Haptics

    def name: Text = this.toString.tt.lower

  object Subtype:
    given showable: Subtype is Showable = _.name

  enum Subtype:
    case Standard(value: Text)
    case Vendor(value: Text)
    case Personal(value: Text)
    case X(value: Text)

    def name: Text = this match
      case Standard(value) => value
      case Vendor(value)   => t"vnd.$value"
      case Personal(value) => t"prs.$value"
      case X(value)        => t"x-$value"

  object Suffix:
    // `name`, not the enum's own name: `JsonSeq` is written `json-seq`, and a rendering which
    // dropped the hyphen could not be parsed back; `FastInfoset` is registered (RFC 6839) as
    // `fastinfoset`, with none.
    given showable: Suffix is Showable = _.name

  enum Suffix:
    case
      Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
      CborSeq, Zstd

    def name: Text = this match
      case JsonSeq     => t"json-seq"
      case CborSeq     => t"cbor-seq"
      case FastInfoset => t"fastinfoset"
      case other       => other.toString.tt.uncamel.kebab

  // For the `media"…"` interpolator, which has parsed the literal at expansion: the gate asserts
  // that, so the runtime re-parse cannot fail.
  def unsafeParse(text: Text)(using erased unsafe: Unsafe): MediaType = unsafely(parse(text))

  def parse(string: Text)(using Tactic[MediaType.Error]^): MediaType =
    def parseParams(ps: List[Text]): List[(Text, Text)] =
      ps match
        case List(t"") =>
          raise(MediaType.Error(string, MediaType.Error.Reason.MissingParam))

        case _ =>
          ()

      ps.map{ (param: Text) => param.cut(t"=", 2) }.map: (p: List[Text]) =>
        p.absolve match
          case List(key, value) => key.show -> value.show

          case _ =>
            abort(MediaType.Error(string, MediaType.Error.Reason.MissingParam))

    // Suffixes are written in kebab-case (`json-seq`, `cbor-seq`) but named in camel-case,
    // so each hyphenated word is capitalized before the enum is looked up; capitalizing the
    // whole suffix instead left `Json-seq`, which no enum case is called. `fastinfoset` is the
    // one registered suffix whose two words are not hyphenated, so it is named outright.
    def parseSuffixes(suffixes: List[Text]): List[Suffix] =
      suffixes.bind: suffix =>
        val name =
          if suffix.lower == t"fastinfoset" then t"FastInfoset"
          else suffix.lower.cut(t"-").map(_.capitalize).join

        try List(Suffix.valueOf(name.s)) catch IllegalArgumentException =>
          abort(MediaType.Error(string, MediaType.Error.Reason.InvalidSuffix(suffix)))

    def parseInit(string: Text): (Subtype, List[Suffix]) =
      val xs: List[Text] = string.cut(t"+")

      xs.absolve match
        case (h: Text) :: rest => (parseSubtype(h), parseSuffixes(rest))

    def parseBasic(string: Text): (Group, Subtype, List[Suffix]) = string.cut(t"/") match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)

      case _ =>
        raise(MediaType.Error(string, MediaType.Error.Reason.NotOneSlash))
        Group.Text *: parseInit(string)

    def parseGroup(string: Text): Group =
      try Group.valueOf(string.lower.capitalize.s) catch IllegalArgumentException =>
        abort(MediaType.Error(string, MediaType.Error.Reason.InvalidGroup))

    def parseSubtype(string: Text): Subtype =
      def notAllowed(char: Char): Boolean =
        char.isWhitespace || char.isControl || specials.has(char)

      val chars = string.chars.stdlib

      chars.find(notAllowed(_)).map: char =>
        raise(MediaType.Error(string, MediaType.Error.Reason.InvalidChar(char)))
        Subtype.X(Array.from(chars.filter(!notAllowed(_))).text)

      . getOrElse:
          if string.starts(t"vnd.") then Subtype.Vendor(string.skip(4))
          else if string.starts(t"prs.") then Subtype.Personal(string.skip(4))
          else if string.starts(t"x.") || string.starts(t"x-") then Subtype.X(string.skip(2))
          else Subtype.Standard(string)

    val xs: List[Text] = string.cut(t";").map(_.trim)

    xs.absolve match
      case (h: Text) :: rest =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(rest))

  final private val specials: Set[Char] =
    Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '+')

trait Media extends Typeclass.Pure:
  extension (value: Self) def mediaType: MediaType
