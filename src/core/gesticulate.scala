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
  given Show[MediaType] = _.toString.text
  
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

  def unapply(value: String): Option[MediaType] =
    try Some(Media.parse(value)) catch case InvalidMediaTypeError(_, _) => None

case class MediaType(group: Media.Group, subtype: Media.Subtype, suffixes: List[Media.Suffix] = Nil,
                        parameters: List[(String, String)] = Nil):
  private def suffixString: String = suffixes.map { s => str"+${s.name}" }.join
  override def toString: String = str"$basic${parameters.map { p => str"; ${p(0)}=${p(1)}" }.join}"
  def basic: String = str"${group.name}/${subtype.name}$suffixString"

object Media:
  object Group:
    given DebugString[Group] = _.name.text
    given Show[Group] = _.name.lower.text

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

    def name: String = toString.lower

  object Subtype:
    given Show[Subtype] = _.name.text

  enum Subtype:
    case Standard(value: String)
    case Vendor(value: String)
    case Personal(value: String)
    case X(value: String)
  
    def name: String = this match
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

  lazy val systemMediaTypes: Set[String] =
    val stream = Option(getClass.getResourceAsStream("/gesticulate/media.types")).getOrElse {
      throw InterpolationError("could not find the file 'gesticulate/media.types' on the classpath")
    }.nn

    scala.io.Source.fromInputStream(stream).getLines.map(_.cut("\t").head.lower).to(Set)

  object Prefix extends Interpolator[Unit, String, MediaType]:
    def parse(state: String, next: String): String = next
    
    def insert(state: String, value: Unit): String =
      throw InterpolationError("a media type literal cannot have substitutions")

    def skip(value: String): String = value
    def initial: String = ""

    def complete(value: String): MediaType =
      val parsed = try Media.parse(value) catch
        case InvalidMediaTypeError(value, nature) =>
          throw InterpolationError(str"'$value' is not a valid media type; ${nature.message}")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMediaTypes.contains(parsed.basic)
          then
            val suggestion = systemMediaTypes.minBy(_.lev(parsed.basic))
            throw InterpolationError(txt"""${parsed.basic} is not a registered media type; did you
                                           mean $suggestion or
                                           ${parsed.basic.replaceAll("/", "/x-").nn}?""".s)
        case _ =>
          ()
      
      parsed

  def parse(string: String): MediaType throws InvalidMediaTypeError =
    def parseParams(ps: List[String]): List[(String, String)] =
      if ps == List("")
      then throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.MissingParam)
      ps.map(_.cut("=", 2)).map { p => p(0) -> p(1) }
    
    def parseSuffixes(ss: List[String]): List[Suffix] = ss.map(_.lower.capitalize).map { s =>
      try Suffix.valueOf(s) catch IllegalArgumentException =>
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidSuffix(s))
    }

    def parseInit(str: String): (Subtype, List[Suffix]) = str.cut("+").to(List) match
      case Nil    => throw Impossible("cannot return empty list from `cut`")
      case h :: t => (parseSubtype(h), parseSuffixes(t))

    def parseBasic(str: String): (Group, Subtype, List[Suffix]) = str.cut("/").to(List) match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)
      case _                    => throw InvalidMediaTypeError(string,
                                       InvalidMediaTypeError.Nature.NotOneSlash)

    def parseGroup(str: String): Group =
      try Group.valueOf(str.lower.capitalize)
      catch IllegalArgumentException =>
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidGroup)

    def parseSubtype(str: String): Subtype =
      str.indexWhere { ch => ch.isWhitespace || ch.isControl || specials.contains(ch) }.match
        case -1 =>
          if str.startsWith("vnd.") then Subtype.Vendor(str.drop(4))
          else if str.startsWith("prs.") then Subtype.Personal(str.drop(4))
          else if str.startsWith("x.") || str.startsWith("x-") then Subtype.X(str.drop(2))
          else Subtype.Standard(str)
        
        case idx =>
          val ch = try str(idx) catch case error@OutOfRangeError(_, _, _) => throw Impossible(error)
          throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidChar(ch))
    
    string.cut(";").map(_.trim).to(List) match
      case Nil    => throw Impossible("cannot return empty list from `cut`")
      case h :: t =>
        val basic = parseBasic(h.nn)
        MediaType(basic(0), basic(1), basic(2), parseParams(t.map(_.nn)))
    
  final private val specials: Set[Char] = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"',
      '/', '[', ']', '?', '=', '+')

object InvalidMediaTypeError:
  enum Nature:
    case NotOneSlash, MissingParam, InvalidGroup
    case InvalidChar(char: Char)
    case InvalidSuffix(suffix: String)

    def message: String = this match
      case NotOneSlash      => txt"a media type should always contain exactly one '/' character".s
      case MissingParam     => txt"a terminal ';' suggests that a parameter is missing".s
      case InvalidGroup     => val list: String = Media.Group.values.map(_.name).join(", ", " or ")
                                txt"the type must be one of: $list".s
      case InvalidChar(c)   => txt"the character '$c' is not allowed".s
      case InvalidSuffix(s) => txt"the suffix '' is not recognized".s

case class InvalidMediaTypeError(value: String, nature: InvalidMediaTypeError.Nature)
extends Exception(txt"gesticulate: \"$value\" is not a valid media type; ${nature.message}".s)
