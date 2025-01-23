/*
    Gesticulate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gesticulate

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import scala.io.*

import language.dynamics

import errorDiagnostics.empty
import proximityMeasures.levenshteinDistance

object Media:
  given Text is Media:
    extension (value: Text) def mediaType = MediaType(Group.Text, Subtype.Standard(t"plain"))

  given [ValueType: Nominable] => ValueType is Media:
    extension (value: ValueType)
      def mediaType = Extensions.guess(ValueType.name(value).cut(t".").last)

  object Group:
    given Group is Inspectable = _.name
    given Group is Showable = _.name.lower

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

    def name: Text = this.toString.tt.lower

  object Subtype:
    given Subtype is Showable = _.name

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
    given Suffix is Showable = _.toString.tt.lower

  enum Suffix:
    case Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
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

    catch case err: InterpolationError => Set()

  object Prefix extends Interpolator[Unit, Text, MediaType]:
    def parse(state: Text, next: Text): Text = next

    def insert(state: Text, value: Unit): Text =
      throw InterpolationError(m"a media type literal cannot have substitutions")

    def skip(value: Text): Text = value
    def initial: Text = t""

    def complete(value: Text): MediaType =
      val parsed = try throwErrors(Media.parse(value)) catch
        case err: MediaTypeError =>
          throw InterpolationError(m"${err.value} is not a valid media type; ${err.reason.message}")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMediaTypes.isEmpty then
            if !systemMediaTypes.contains(parsed.basic) then
              val suggestion = systemMediaTypes.minBy(_.proximity(parsed.basic))
              throw InterpolationError(m"""
                ${parsed.basic} is not a registered media type; did you mean $suggestion or
                ${parsed.basic.sub(t"/", t"/x-")}?
              """)

        case _ =>
          ()

      parsed

  def parse(string: Text)(using Tactic[MediaTypeError]): MediaType =
    def parseParams(ps: List[Text]): List[(Text, Text)] =
      if ps == List("")
      then raise(MediaTypeError(string, MediaTypeError.Reason.MissingParam))
      ps.map(_.cut(t"=", 2).to(List)).map { p => p(0).show -> p(1).show }

    def parseSuffixes(suffixes: List[Text]): List[Suffix] = suffixes.map(_.lower.capitalize).flatMap: suffix =>
      try List(Suffix.valueOf(suffix.s)) catch IllegalArgumentException =>
        raise(MediaTypeError(string, MediaTypeError.Reason.InvalidSuffix(suffix)), Nil)

    def parseInit(str: Text): (Subtype, List[Suffix]) =
      val xs: List[Text] = str.cut(t"+").to(List)

      (xs: @unchecked) match
      case (h: Text) :: _ => (parseSubtype(h), parseSuffixes(xs.tail))

    def parseBasic(str: Text): (Group, Subtype, List[Suffix]) = str.cut(t"/").to(List) match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)

      case _ =>
        raise
         (MediaTypeError(string, MediaTypeError.Reason.NotOneSlash),
          Group.Text *: parseInit(string))

    def parseGroup(str: Text): Group =
      try Group.valueOf(str.lower.capitalize.s) catch IllegalArgumentException =>
        raise(MediaTypeError(string, MediaTypeError.Reason.InvalidGroup), Group.Text)

    def parseSubtype(str: Text): Subtype =
      def notAllowed(char: Char): Boolean = char.isWhitespace || char.isControl || specials.contains(char)

      str.chars.find(notAllowed(_)).map: char =>
        raise
         (MediaTypeError(string, MediaTypeError.Reason.InvalidChar(char)),
          Subtype.X(str.chars.filter(!notAllowed(_)).text))

      . getOrElse:
          if str.starts(t"vnd.") then Subtype.Vendor(str.skip(4))
          else if str.starts(t"prs.") then Subtype.Personal(str.skip(4))
          else if str.starts(t"x.") || str.starts(t"x-") then Subtype.X(str.skip(2))
          else Subtype.Standard(str)

    val xs: List[Text] = string.cut(t";").to(List).map(_.trim)

    (xs: @unchecked) match
      case (h: Text) :: _ =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(xs.tail))

  final private val specials: Set[Char] =
    Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '+')

trait Media:
  type Self
  extension (value: Self) def mediaType: MediaType
