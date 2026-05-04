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
package gesticulate

import language.dynamics

import scala.io.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import caseSensitivity.insensitive
import errorDiagnostics.empty
import proximities.levenshteinDistance

object Media:
  given text: Text is Media:
    extension (value: Text)
      def mediaType: MediaType = MediaType(Group.Text, Subtype.Standard(t"plain"))

  given nominable: [nominable: Nominable] => nominable is Media:
    extension (value: nominable)
      def mediaType: MediaType = Extensions.guess(nominable.name(value).cut(t".").last)

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

  lazy val systemMediaTypes: Set[Text] =
    try
      val stream = Optional(getClass.getResourceAsStream("/gesticulate/media.types")).or:
        throw InterpolationError(m"could not find 'gesticulate/media.types' on the classpath")

      val lines: Iterator[Text] =
        scala.io.Source.fromInputStream(stream).getLines.map(Text(_)).map(_.cut(t"\t").head.lower)

      lines.to(Set)

    catch case error: InterpolationError => Set()

  def validateLiteral(text: Text): Optional[Message] =
    val parsed = try throwErrors(Media.parse(text)) catch
      case error: MediaTypeError =>
        return m"${error.value} is not a valid media type; ${error.reason.message}"

    parsed.subtype match
      case Subtype.Standard(_) =>
        if !systemMediaTypes.nil then
          if !systemMediaTypes.contains(parsed.basic) then
            val suggestion = systemMediaTypes.minBy(_.proximity(parsed.basic))

            return
              m"""
                ${parsed.basic} is not a registered media type; did you mean $suggestion or
                ${parsed.basic.sub(t"/", t"/x-")}?
              """

      case _ =>
        ()

    Unset

  def parse(string: Text)(using Tactic[MediaTypeError]): MediaType =
    def parseParams(ps: List[Text]): List[(Text, Text)] =
      if ps == List("")
      then raise(MediaTypeError(string, MediaTypeError.Reason.MissingParam))
      ps.map(_.cut(t"=", 2).to(List)).map { p => p(0).show -> p(1).show }

    def parseSuffixes(suffixes: List[Text]): List[Suffix] =
      suffixes.map(_.lower.capitalize).flatMap: suffix =>
        try List(Suffix.valueOf(suffix.s)) catch IllegalArgumentException =>
          raise(MediaTypeError(string, MediaTypeError.Reason.InvalidSuffix(suffix))) yet Nil

    def parseInit(string: Text): (Subtype, List[Suffix]) =
      val xs: List[Text] = string.cut(t"+").to(List)

      xs.absolve match
        case (h: Text) :: _ => (parseSubtype(h), parseSuffixes(xs.tail))

    def parseBasic(string: Text): (Group, Subtype, List[Suffix]) = string.cut(t"/").to(List) match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)

      case _ =>
        raise(MediaTypeError(string, MediaTypeError.Reason.NotOneSlash))
        Group.Text *: parseInit(string)

    def parseGroup(string: Text): Group =
      try Group.valueOf(string.lower.capitalize.s) catch IllegalArgumentException =>
        raise(MediaTypeError(string, MediaTypeError.Reason.InvalidGroup)) yet Group.Text

    def parseSubtype(string: Text): Subtype =
      def notAllowed(char: Char): Boolean =
        char.isWhitespace || char.isControl || specials.contains(char)

      string.chars.find(notAllowed(_)).map: char =>
        raise(MediaTypeError(string, MediaTypeError.Reason.InvalidChar(char)))
        Subtype.X(string.chars.filter(!notAllowed(_)).text)

      . getOrElse:
          if string.starts(t"vnd.") then Subtype.Vendor(string.skip(4))
          else if string.starts(t"prs.") then Subtype.Personal(string.skip(4))
          else if string.starts(t"x.") || string.starts(t"x-") then Subtype.X(string.skip(2))
          else Subtype.Standard(string)

    val xs: List[Text] = string.cut(t";").to(List).map(_.trim)

    xs.absolve match
      case (h: Text) :: _ =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(xs.tail))

  final private val specials: Set[Char] =
    Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '+')

trait Media extends Typeclass:
  extension (value: Self) def mediaType: MediaType
