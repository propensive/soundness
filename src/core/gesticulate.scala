/*
    Gesticulate, version 0.1.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*
import contextual.*
import scala.io.*

object MediaType:
  given DebugString[MediaType] = mt => str"""media"${mt.toString}""""
  given Show[MediaType] = _.text
  
  given formenctype: clairvoyant.HtmlAttribute["formenctype", MediaType] with
    def name: String = "formenctype"
    def serialize(mediaType: MediaType): String = mediaType.toString
  
  given media: clairvoyant.HtmlAttribute["media", MediaType] with
    def name: String = "media"
    def serialize(mediaType: MediaType): String = mediaType.toString
  
  given enctype: clairvoyant.HtmlAttribute["enctype", MediaType] with
    def name: String = "enctype"
    def serialize(mediaType: MediaType): String = mediaType.toString
  
  given htype: clairvoyant.HtmlAttribute["htype", MediaType] with
    def name: String = "type"
    def serialize(mediaType: MediaType): String = mediaType.toString

  def unapply(value: Txt): Option[MediaType] =
    try Some(Media.parse(value)) catch case InvalidMediaTypeError(_, _) => None

case class MediaType(group: Media.Group, subtype: Media.Subtype, suffixes: List[Media.Suffix] = Nil,
                        parameters: List[(Txt, Txt)] = Nil):
  private def suffixString: Txt = suffixes.map { s => str"+${s.name}" }.join
  def text: Txt = str"$basic${parameters.map { p => str"; ${p(0)}=${p(1)}" }.join}"
  def basic: Txt = str"${group.name}/${subtype.name}$suffixString"

object Media:
  object Group:
    given DebugString[Group] = _.name
    given Show[Group] = _.name.lower

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

    def name: Txt = Txt(toString).lower

  object Subtype:
    given Show[Subtype] = _.name

  enum Subtype:
    case Standard(value: Txt)
    case Vendor(value: Txt)
    case Personal(value: Txt)
    case X(value: Txt)
  
    def name: Txt = this match
      case Standard(value) => value
      case Vendor(value)   => str"vnd.$value"
      case Personal(value) => str"prs.$value"
      case X(value)        => str"x.$value"

  object Suffix:
    given Show[Suffix] = _.toString.lower.text

  enum Suffix:
    case Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
        CborSeq, Zstd
  
    def name: String = this match
      case JsonSeq => "json-seq"
      case CborSeq => "cbor-seq"
      case other   => toString.lower

  lazy val systemMediaTypes: Set[Txt] =
    val stream = Option(getClass.getResourceAsStream("/gesticulate/media.types")).getOrElse {
      throw InterpolationError("could not find the file 'gesticulate/media.types' on the classpath")
    }.nn

    val lines: Iterator[Txt] = scala.io.Source.fromInputStream(stream).getLines.map(Txt(_)).map(_.cut(str"\t").head.lower)
    lines.to(Set)

  object Prefix extends Interpolator[Unit, String, MediaType]:
    def parse(state: String, next: String): String = next
    
    def insert(state: String, value: Unit): String =
      throw InterpolationError("a media type literal cannot have substitutions")

    def skip(value: String): String = value
    def initial: String = ""

    def complete(value: String): MediaType =
      val parsed = try Media.parse(Txt(value)) catch
        case InvalidMediaTypeError(value, nature) =>
          throw InterpolationError(s"'$value' is not a valid media type; ${nature.message}")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMediaTypes.contains(parsed.basic)
          then
            val suggestion = systemMediaTypes.minBy(_.lev(parsed.basic))
            throw InterpolationError(txt"""${parsed.basic} is not a registered media type; did you
                                           mean $suggestion or
                                           ${parsed.basic.sub("/", "/x-")}?""".s)
        case _ =>
          ()
      
      parsed

  def parse(string: Txt): MediaType throws InvalidMediaTypeError =
    def parseParams(ps: List[Txt]): List[(Txt, Txt)] =
      if ps == List("")
      then throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.MissingParam)
      ps.map(_.cut(str"=", 2).asInstanceOf[List[Txt]]).map { p => p(0) -> p(1) }
    
    def parseSuffixes(ss: List[Txt]): List[Suffix] = ss.map(_.lower.capitalize).map { s =>
      try Suffix.valueOf(s) catch IllegalArgumentException =>
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidSuffix(s))
    }

    def parseInit(str: Txt): (Subtype, List[Suffix]) =
      val xs: List[Txt] = str.cut(str"+")
      
      xs match
      case Nil           => throw Impossible("cannot return empty list from `cut`")
      case (h: Txt) :: _ => (parseSubtype(h), parseSuffixes(xs.tail))

    def parseBasic(str: Txt): (Group, Subtype, List[Suffix]) = str.cut(str"/").to(List) match
      case List(group, subtype) => parseGroup(group.asInstanceOf[Txt]) *: parseInit(subtype.asInstanceOf[Txt])
      case _                    => throw InvalidMediaTypeError(string,
                                       InvalidMediaTypeError.Nature.NotOneSlash)

    def parseGroup(str: Txt): Group =
      try Group.valueOf(str.lower.capitalize)
      catch IllegalArgumentException =>
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidGroup)

    def parseSubtype(str: Txt): Subtype =
      try
        val idx = (str.indexWhere { ch => ch.isWhitespace || ch.isControl || specials.contains(ch) })
        val ch = try str(idx) catch case error@OutOfRangeError(_, _, _) => throw Impossible(error)
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidChar(ch))
      catch case OutOfRangeError(_, _, _) =>
        if str.startsWith("vnd.") then Subtype.Vendor(str.drop(4))
        else if str.startsWith("prs.") then Subtype.Personal(str.drop(4))
        else if str.startsWith("x.") || str.startsWith("x-") then Subtype.X(str.drop(2))
        else Subtype.Standard(str)
        
    val xs: List[Txt] = string.cut(str";").map(_.trim.asInstanceOf[Txt])
    
    xs match
      case Nil    => throw Impossible("cannot return empty list from `cut`")
      case (h: Txt) :: _ =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(xs.tail))
    
  final private val specials: Set[Char] = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"',
      '/', '[', ']', '?', '=', '+')

object InvalidMediaTypeError:
  enum Nature:
    case NotOneSlash, MissingParam, InvalidGroup
    case InvalidChar(char: Char)
    case InvalidSuffix(suffix: String)

    def message: Txt = this match
      case NotOneSlash      => txt"a media type should always contain exactly one '/' character"
      case MissingParam     => txt"a terminal ';' suggests that a parameter is missing"
      case InvalidGroup     => val list: Txt = Media.Group.values.map(_.name).join(str", ", str" or ")
                               txt"the type must be one of: $list"
      case InvalidChar(c)   => txt"the character '$c' is not allowed"
      case InvalidSuffix(s) => txt"the suffix '' is not recognized"

case class InvalidMediaTypeError(value: Txt, nature: InvalidMediaTypeError.Nature)
extends Exception(txt"gesticulate: \"$value\" is not a valid media type; ${nature.message}".s)
