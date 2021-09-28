package gesticulate

import rudiments.*
import gossamer.*
import contextual.*
import scala.io.*

case class MediaType(group: Media.Group, subtype: Media.Subtype, suffixes: List[Media.Suffix] = Nil,
                        parameters: List[(String, String)] = Nil):
  private def suffixString: String = suffixes.map { s => str"+${s.name}" }.join
  override def toString: String = str"$basic${parameters.map { p => str"; ${p(0)}=${p(1)}" }.join}"
  def basic: String = str"${group.name}/${subtype.name}$suffixString"

object Media:

  object Group:
    given Show[Group] = _.name.text

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

    Source.fromInputStream(stream).getLines.map(_.cut("\t").head.lower).to(Set)

  object Prefix extends Interpolator[Unit, String, MediaType]:
    def parse(state: String, next: String): String = next
    
    def insert(state: String, value: Unit): String =
      throw InterpolationError("a media type literal cannot have substitutions")

    def skip(value: String): String = value
    def initial: String = ""

    def complete(value: String): MediaType =
      val parsed = try Media.parse(value) catch
        case InvalidMediaTypeError(value, message) =>
          throw InterpolationError(str"'$value' is not a valid media type; $message")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMediaTypes.contains(parsed.basic)
          then
            val suggestion = systemMediaTypes.minBy(_.editDistanceTo(parsed.basic))
            throw InterpolationError(txt"""${parsed.basic} is not a registered media type; did you
                                           mean $suggestion or
                                           ${parsed.basic.replaceAll("/", "/x-").nn}?""".s)
        case _ =>
          ()
      
      parsed

  def parse(string: String): MediaType throws InvalidMediaTypeError =
    def parseParams(ps: List[String]): List[(String, String)] =
      if ps == List("") then throw InvalidMediaTypeError(string, txt"""a terminal ';' suggests that
          a parameter is missing""".s)
      ps.map(_.cut("=", 2)).map { p => p(0) -> p(1) }
    
    def parseSuffixes(ss: List[String]): List[Suffix] = ss.map(_.lower.capitalize).map { s =>
      try Suffix.valueOf(s) catch IllegalArgumentException =>
        throw InvalidMediaTypeError(string, str"the suffix '${s.lower}' is not recognized")
    }

    def parseInit(str: String): (Subtype, List[Suffix]) = str.cut("+").to(List) match
      case Nil    => throw Impossible("cannot return empty list from `cut`")
      case h :: t => (parseSubtype(h), parseSuffixes(t))

    def parseBasic(str: String): (Group, Subtype, List[Suffix]) = str.cut("/").to(List) match
      case List(group, subtype) => parseGroup(group) *: parseInit(subtype)
      case _                    => throw InvalidMediaTypeError(string, txt"""a media type should
                                        always contain exactly one '/' character""".s)

    def parseGroup(str: String): Group =
      try Group.valueOf(str.lower.capitalize)
      catch IllegalArgumentException =>
        val list: String = Group.values.map(_.name).join(", ", " or ")
        throw InvalidMediaTypeError(string, str"the type must be one of: $list")

    def parseSubtype(str: String): Subtype =
      str.indexWhere { ch => ch.isWhitespace || ch.isControl || specials.contains(ch) }.match
        case -1 =>
          if str.startsWith("vnd.") then Subtype.Vendor(str.drop(4))
          else if str.startsWith("prs.") then Subtype.Personal(str.drop(4))
          else if str.startsWith("x.") || str.startsWith("x-") then Subtype.X(str.drop(2))
          else Subtype.Standard(str)
        
        case idx =>
          val ch = try str(idx) catch case error@OutOfRangeError(_, _, _) => throw Impossible(error)
          throw InvalidMediaTypeError(string, str"the character ${ch} is not allowed")
    
    string.cut(";").map(_.trim).to(List) match
      case Nil    => throw Impossible("cannot return empty list from `cut`")
      case h :: t =>
        val basic = parseBasic(h.nn)
        MediaType(basic(0), basic(1), basic(2), parseParams(t.map(_.nn)))
    
  final private val specials: Set[Char] = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"',
      '/', '[', ']', '?', '=', '+')

case class InvalidMediaTypeError(value: String, message: String)
extends Exception(txt"gesticulate: \"$value\" is not a valid media type; $message".s)