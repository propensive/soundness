/*
    Gesticulate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import vacuous.*
import fulminate.*
import gossamer.*
import contextual.*
import perforate.*
import anticipation.*
import spectacular.*

import scala.io.*

import language.dynamics
//import language.experimental.captureChecking

object Media:
  object Group:
    given Debug[Group] = _.name
    given Show[Group] = _.name.lower

  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

    def name: Text = this.toString.tt.lower

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
    given Show[Suffix] = _.toString.tt.lower

  enum Suffix:
    case Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
        CborSeq, Zstd
  
    def name: Text = this match
      case JsonSeq => t"json-seq"
      case CborSeq => t"cbor-seq"
      case other   => other.toString.tt.uncamel.kebab

  lazy val systemMediaTypes: Set[Text] =
    try
      val stream = Option(getClass.getResourceAsStream("/gesticulate/media.types")).getOrElse:
        throw InterpolationError(msg"could not find 'gesticulate/media.types' on the classpath")
      .nn

      val lines: Iterator[Text] =
        scala.io.Source.fromInputStream(stream).getLines.map(Text(_)).map(_.cut(t"\t").head.lower)
      
      lines.to(Set)
    catch case err: InterpolationError => Set()

  object Prefix extends Interpolator[Unit, Text, MediaType]:
    def parse(state: Text, next: Text): Text = next
    
    def insert(state: Text, value: Unit): Text =
      throw InterpolationError(msg"a media type literal cannot have substitutions")

    def skip(value: Text): Text = value
    def initial: Text = t""

    def complete(value: Text): MediaType =
      val parsed = try throwErrors(Media.parse(value)) catch
        case err: MediaTypeError =>
          throw InterpolationError(msg"${err.value} is not a valid media type; ${err.nature.message}")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMediaTypes.contains(parsed.basic)
          then
            val suggestion = systemMediaTypes.minBy(_.lev(parsed.basic))
            throw InterpolationError(msg"""
              ${parsed.basic} is not a registered media type; did you mean $suggestion or
              ${parsed.basic.sub(t"/", t"/x-")}?
            """)
        case _ =>
          ()
      
      parsed

  def parse(string: Text)(using Raises[MediaTypeError]): MediaType =
    def parseParams(ps: List[Text]): List[(Text, Text)] =
      if ps == List("")
      then raise(MediaTypeError(string, MediaTypeError.Nature.MissingParam))(())
      ps.map(_.cut(t"=", 2).to(List)).map { p => p(0).show -> p(1).show }
    
    def parseSuffixes(suffixes: List[Text]): List[Suffix] = suffixes.map(_.lower.capitalize).flatMap: suffix =>
      try List(Suffix.valueOf(suffix.s)) catch IllegalArgumentException =>
        raise(MediaTypeError(string, MediaTypeError.Nature.InvalidSuffix(suffix)))(Nil)

    def parseInit(str: Text): (Subtype, List[Suffix]) =
      val xs: List[Text] = str.cut(t"+")
      
      (xs: @unchecked) match
      case (h: Text) :: _ => (parseSubtype(h), parseSuffixes(xs.tail))

    def parseBasic(str: Text): (Group, Subtype, List[Suffix]) = str.cut(t"/").to(List) match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)
      
      case _ =>
        raise(MediaTypeError(string, MediaTypeError.Nature.NotOneSlash)):
          Group.Text *: parseInit(string)

    def parseGroup(str: Text): Group =
      try Group.valueOf(str.lower.capitalize.s)
      catch IllegalArgumentException =>
        raise(MediaTypeError(string, MediaTypeError.Nature.InvalidGroup))(Group.Text)

    def parseSubtype(str: Text): Subtype =
      def notAllowed(char: Char): Boolean = char.isWhitespace || char.isControl || specials.contains(char)
      str.chars.find(notAllowed(_)).map: char =>
        raise(MediaTypeError(string, MediaTypeError.Nature.InvalidChar(char))):
          Subtype.X(str.chars.filter(!notAllowed(_)).text)
      .getOrElse:
        if str.starts(t"vnd.") then Subtype.Vendor(str.drop(4))
        else if str.starts(t"prs.") then Subtype.Personal(str.drop(4))
        else if str.starts(t"x.") || str.starts(t"x-") then Subtype.X(str.drop(2))
        else Subtype.Standard(str)
        
    val xs: List[Text] = string.cut(t";").map(_.trim)
    
    (xs: @unchecked) match
      case (h: Text) :: _ =>
        val basic = parseBasic(h)
        MediaType(basic(0), basic(1), basic(2), parseParams(xs.tail))
    
  final private val specials: Set[Char] =
    Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '+')

object MediaTypeError:
  enum Nature:
    case NotOneSlash, MissingParam, InvalidGroup
    case InvalidChar(char: Char)
    case InvalidSuffix(suffix: Text)

    def message: Text = this match
      case NotOneSlash      => txt"a media type should always contain exactly one '/' character"
      case MissingParam     => txt"a terminal ';' suggests that a parameter is missing"
      case InvalidGroup     => val list = Media.Group.values.immutable(using Unsafe).map(_.name)
                               txt"the type must be one of: ${list.join(t", ", t" or ")}"
      case InvalidChar(c)   => txt"the character '$c' is not allowed"
      case InvalidSuffix(s) => txt"the suffix '$s' is not recognized"

case class MediaTypeError(value: Text, nature: MediaTypeError.Nature)
extends Error(msg"the value $value is not a valid media type; ${nature.message}")

case class MediaType(group: Media.Group, subtype: Media.Subtype, suffixes: List[Media.Suffix] = Nil,
                        parameters: List[(Text, Text)] = Nil) extends Dynamic:
  private def suffixString: Text = suffixes.map { s => t"+${s.name}" }.join
  def basic: Text = t"${group.name}/${subtype.name}$suffixString"
  
  def applyDynamicNamed(apply: "apply")(kvs: (String, Text)*): MediaType =
    copy(parameters = parameters ::: kvs.map(_.show -> _).to(List))

object MediaType:
  given Debug[MediaType] = mt => t"""media"${mt}""""
  given GenericHttpRequestParam["content-type", MediaType] = show(_)

  given show: Show[MediaType] =
    mt => t"${mt.basic}${mt.parameters.map { p => t"; ${p(0)}=${p(1)}" }.join}"
  
  given formenctype: GenericHtmlAttribute["formenctype", MediaType] with
    def name: Text = t"formenctype"
    def serialize(mediaType: MediaType): Text = mediaType.show
  
  given media: GenericHtmlAttribute["media", MediaType] with
    def name: Text = t"media"
    def serialize(mediaType: MediaType): Text = mediaType.show
  
  given enctype: GenericHtmlAttribute["enctype", MediaType] with
    def name: Text = t"enctype"
    def serialize(mediaType: MediaType): Text = mediaType.show
  
  given htype: GenericHtmlAttribute["htype", MediaType] with
    def name: Text = t"type"
    def serialize(mediaType: MediaType): Text = mediaType.show

  def unapply(value: Text): Option[MediaType] = safely(Some(Media.parse(value))).or(None)
  
