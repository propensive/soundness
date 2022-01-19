/*
    Gesticulate, version 0.1.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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

object Media:
  object Group:
    given DebugString[Group] = _.name
    given Show[Group] = _.name.lower

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

    def name: Text = Showable(this).show.lower

  object Subtype:
    given Show[Subtype] = _.name

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
    given Show[Suffix] = Showable(_).show.lower

  enum Suffix:
    case Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
        CborSeq, Zstd
  
    def name: Text = this match
      case JsonSeq => t"json-seq"
      case CborSeq => t"cbor-seq"
      case other   => Showable(other).show.dashed

  lazy val systemMediaTypes: Set[Text] =
    try
      val stream = Option(getClass.getResourceAsStream("/gesticulate/media.types")).getOrElse:
        throw InterpolationError(t"could not find 'gesticulate/media.types' on the classpath")
      .nn

      val lines: Iterator[Text] =
        scala.io.Source.fromInputStream(stream).getLines.map(Text(_)).map(_.cut(t"\t").head.lower)
      
      lines.to(Set)
    catch case err: InterpolationError => Set()

  object Prefix extends Interpolator[Unit, Text, MediaType]:
    def parse(state: Text, next: Text): Text = next
    
    def insert(state: Text, value: Unit): Text =
      throw InterpolationError(t"a media type literal cannot have substitutions")

    def skip(value: Text): Text = value
    def initial: Text = t""

    def complete(value: Text): MediaType =
      val parsed = try Media.parse(value) catch
        case err: InvalidMediaTypeError =>
          throw InterpolationError(t"'${err.value}' is not a valid media type; ${err.nature.message}")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMediaTypes.contains(parsed.basic)
          then
            val suggestion = systemMediaTypes.minBy(_.lev(parsed.basic))
            throw InterpolationError(txt"""${parsed.basic} is not a registered media type; did you
                                           mean $suggestion or
                                           ${parsed.basic.sub(t"/", t"/x-")}?""")
        case _ =>
          ()
      
      parsed

  def parse(string: Text): MediaType throws InvalidMediaTypeError =
    def parseParams(ps: List[Text]): List[(Text, Text)] =
      if ps == List("")
      then throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.MissingParam)
      ps.map(_.cut(t"=", 2).to(List)).map { p => p(0).show -> p(1).show }
    
    def parseSuffixes(ss: List[Text]): List[Suffix] = ss.map(_.lower.capitalize).map:
      s =>
        try Suffix.valueOf(s.s) catch IllegalArgumentException =>
          throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidSuffix(s))

    def parseInit(str: Text): (Subtype, List[Suffix]) =
      val xs: List[Text] = str.cut(t"+")
      
      xs match
      case Nil           => throw Impossible("cannot return empty list from `cut`")
      case (h: Text) :: _ => (parseSubtype(h), parseSuffixes(xs.tail))

    def parseBasic(str: Text): (Group, Subtype, List[Suffix]) = str.cut(t"/").to(List) match
      case List(group, subtype) =>
        parseGroup(group.asInstanceOf[Text]) *: parseInit(subtype.asInstanceOf[Text])
      
      case _ =>
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.NotOneSlash)

    def parseGroup(str: Text): Group =
      try Group.valueOf(str.lower.capitalize.s)
      catch IllegalArgumentException =>
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidGroup)

    def parseSubtype(str: Text): Subtype =
      try
        val idx = (str.where { ch => ch.isWhitespace || ch.isControl || specials.contains(ch) })
        val ch = try str(idx) catch case error: OutOfRangeError => throw Impossible(error)
        throw InvalidMediaTypeError(string, InvalidMediaTypeError.Nature.InvalidChar(ch))
      catch case e: OutOfRangeError =>
        if str.startsWith(t"vnd.") then Subtype.Vendor(str.drop(4))
        else if str.startsWith(t"prs.") then Subtype.Personal(str.drop(4))
        else if str.startsWith(t"x.") || str.startsWith(t"x-") then Subtype.X(str.drop(2))
        else Subtype.Standard(str)
        
    val xs: List[Text] = string.cut(t";").map(_.trim.asInstanceOf[Text])
    
    xs match
      case Nil    => throw Impossible("cannot return empty list from `cut`")
      case (h: Text) :: _ =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(xs.tail))
    
  final private val specials: Set[Char] = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"',
      '/', '[', ']', '?', '=', '+')

object InvalidMediaTypeError:
  enum Nature:
    case NotOneSlash, MissingParam, InvalidGroup
    case InvalidChar(char: Char)
    case InvalidSuffix(suffix: Text)

    def message: Text = this match
      case NotOneSlash      => txt"a media type should always contain exactly one '/' character"
      case MissingParam     => txt"a terminal ';' suggests that a parameter is missing"
      case InvalidGroup     => val list = Media.Group.values.unsafeImmutable.map(_.name)
                               txt"the type must be one of: ${list.join(t", ", t" or ")}"
      case InvalidChar(c)   => txt"the character '$c' is not allowed"
      case InvalidSuffix(s) => txt"the suffix '$s' is not recognized"

case class InvalidMediaTypeError(value: Text, nature: InvalidMediaTypeError.Nature) extends Error:
  def message: Text = t"the value \"$value\" is not a valid media type; ${nature.message}"

case class MediaType(group: Media.Group, subtype: Media.Subtype, suffixes: List[Media.Suffix] = Nil,
                        parameters: List[(Text, Text)] = Nil):
  private def suffixString: Text = suffixes.map { s => t"+${s.name}" }.join
  def basic: Text = t"${group.name}/${subtype.name}$suffixString"

object MediaType:
  given DebugString[MediaType] = mt => t"""media"${mt}""""
  
  given Show[MediaType] =
    mt => t"${mt.basic}${mt.parameters.map { p => t"; ${p(0)}=${p(1)}" }.join}"
  
  given formenctype: clairvoyant.HtmlAttribute["formenctype", MediaType] with
    def name: String = "formenctype"
    def serialize(mediaType: MediaType): String = mediaType.show.s
  
  given media: clairvoyant.HtmlAttribute["media", MediaType] with
    def name: String = "media"
    def serialize(mediaType: MediaType): String = mediaType.show.s
  
  given enctype: clairvoyant.HtmlAttribute["enctype", MediaType] with
    def name: String = "enctype"
    def serialize(mediaType: MediaType): String = mediaType.show.s
  
  given htype: clairvoyant.HtmlAttribute["htype", MediaType] with
    def name: String = "type"
    def serialize(mediaType: MediaType): String = mediaType.show.s

  def unapply(value: Text): Option[MediaType] =
    try Some(Media.parse(value)) catch case err: InvalidMediaTypeError => None
  