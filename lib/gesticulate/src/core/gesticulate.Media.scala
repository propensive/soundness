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

object Media:
  given text: Text is Media:
    extension (value: Text)
      def mediaType: MediaType = MediaType(Group.Text, Subtype.Standard(t"plain"))

  given nominable: [nominable: Nominable] => nominable is Media:
    extension (value: nominable)
      def mediaType: MediaType =
        val parts = nominable.name(value).cut(t".").stdlib
        Extensions.guess(parts.last)

  object Group:
    given inspectable: Group is Inspectable = _.name
    given showable: Group is Showable = _.name.lower

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

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
    given showable: Suffix is Showable = _.toString.tt.lower

  enum Suffix:
    case
      Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
      CborSeq, Zstd

    def name: Text = this match
      case JsonSeq => t"json-seq"
      case CborSeq => t"cbor-seq"
      case other   => other.toString.tt.uncamel.kebab

  def parse(string: Text)(using Tactic[MediaTypeError]^): MediaType =
    def parseParams(ps: List[Text]): List[(Text, Text)] =
      ps.stdlib match
        case scala.collection.immutable.List(t"") =>
          raise(MediaTypeError(string, MediaTypeError.Reason.MissingParam))

        case _ =>
          ()

      ps.map((param: Text) => param.cut(t"=", 2).stdlib).map: (p: scala.collection.immutable.List[Text]) =>
        p(0).show -> p(1).show

    def parseSuffixes(suffixes: List[Text]): List[Suffix] =
      suffixes.map(_.lower.capitalize).bind: suffix =>
        try List(Suffix.valueOf(suffix.s)) catch IllegalArgumentException =>
          abort(MediaTypeError(string, MediaTypeError.Reason.InvalidSuffix(suffix)))

    def parseInit(string: Text): (Subtype, List[Suffix]) =
      val xs: List[Text] = string.cut(t"+")

      xs.absolve match
        case (h: Text) :: _ => (parseSubtype(h), parseSuffixes(List.of(xs.stdlib.tail)))

    def parseBasic(string: Text): (Group, Subtype, List[Suffix]) = string.cut(t"/") match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)

      case _ =>
        raise(MediaTypeError(string, MediaTypeError.Reason.NotOneSlash))
        Group.Text *: parseInit(string)

    def parseGroup(string: Text): Group =
      try Group.valueOf(string.lower.capitalize.s) catch IllegalArgumentException =>
        abort(MediaTypeError(string, MediaTypeError.Reason.InvalidGroup))

    def parseSubtype(string: Text): Subtype =
      def notAllowed(char: Char): Boolean =
        char.isWhitespace || char.isControl || specials.has(char)

      val chars = scala.collection.immutable.ArraySeq.unsafeWrapArray:
        string.chars.mutable(using Unsafe)

      chars.find(notAllowed(_)).map: char =>
        raise(MediaTypeError(string, MediaTypeError.Reason.InvalidChar(char)))
        Subtype.X(IArray.from(chars.filter(!notAllowed(_))).text)

      . getOrElse:
          if string.starts(t"vnd.") then Subtype.Vendor(string.skip(4))
          else if string.starts(t"prs.") then Subtype.Personal(string.skip(4))
          else if string.starts(t"x.") || string.starts(t"x-") then Subtype.X(string.skip(2))
          else Subtype.Standard(string)

    val xs: List[Text] = string.cut(t";").map(_.trim)

    xs.absolve match
      case (h: Text) :: _ =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(List.of(xs.stdlib.tail)))

  final private val specials: Set[Char] =
    Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '+')

trait Media extends Typeclass.Pure:
  extension (value: Self) def mediaType: MediaType
