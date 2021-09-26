package gesticulate

import rudiments.*
import gossamer.*
import contextual.*
import scala.io.*

case class MimeType(group: Mime.Group, subtype: Mime.Subtype, suffixes: List[Mime.Suffix] = Nil,
                        parameters: List[(String, String)] = Nil):
  private def suffixString: String = suffixes.map { s => str"+${s.name}" }.join
  override def toString: String = str"$basic${parameters.map { p => str"; ${p(0)}=${p(1)}" }.join}"
  def basic: String = str"${group.name}/${subtype.name}$suffixString"

object Mime:
  enum Group:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model
    //case X(value: String)

    def name: String = this match
      //case X(value) => str"X-${value.lower}"
      case other    => other.toString.lower
  
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
  
  enum Suffix:
    case Xml, Json, Ber, Cbor, Der, FastInfoset, Wbxml, Zip, Tlv, JsonSeq, Sqlite3, Jwt, Gzip,
        CborSeq, Zstd
  
    def name: String = this match
      case JsonSeq => "json-seq"
      case CborSeq => "cbor-seq"
      case other   => toString.lower

  lazy val systemMimeTypes: Set[String] =
    Source.fromFile("/etc/mime.types").getLines.map(_.cut("\t").head).to(Set)

  object Prefix extends Interpolator[Nothing, String, MimeType]:
    def parse(state: String, next: String): String = next
    
    def insert(state: String, value: Nothing): String =
      throw InterpolationError("a MIME type literal cannot have substitutions")

    def skip(value: String): String = value
    def initial: String = ""

    def complete(value: String): MimeType =
      val parsed = try parse(value) catch
        case InvalidMimeTypeError(value, message) =>
          throw InterpolationError(str"'$value' is not a valid MIME type; $message")

      parsed.subtype match
        case Subtype.Standard(_) =>
          if !systemMimeTypes.contains(parsed.basic)
          then throw InterpolationError(s"${parsed.basic} is not a registered MIME type")
        case _ =>
          ()
      
      parsed

    def parse(string: String): MimeType =
      def parseParams(ps: List[String]): List[(String, String)] =
        if ps == List("") then throw InvalidMimeTypeError(string, txt"""a terminal ';' suggests that
            a parameter is missing""".s)
        ps.map(_.cut("=", 2)).map { p => p(0) -> p(1) }
      
      def parseSuffixes(ss: List[String]): List[Suffix] = ss.map(_.lower.capitalize).map { s =>
        try Suffix.valueOf(s) catch IllegalArgumentException =>
          throw InvalidMimeTypeError(string, str"the suffix '${s.lower}' is not recognized")
      }
  
      def parseInit(str: String): (Subtype, List[Suffix]) = str.cut("+").to(List) match
        case Nil    => throw Impossible("cannot return empty list from `cut`")
        case h :: t => (parseSubtype(h), parseSuffixes(t))
  
      def parseBasic(str: String): (Group, Subtype, List[Suffix]) = str.cut("/").to(List) match
        case List(group, subtype) => parseGroup(group) *: parseInit(subtype)
        case _                    => throw InvalidMimeTypeError(string, txt"""a MIME type should
                                         always contain exactly one '/' character""".s)
  
      def parseGroup(str: String): Group =
        try Group.valueOf(str.lower.capitalize)
        catch IllegalArgumentException =>
          val list: String = Group.values.map(_.name).join(", ", " or ")
          throw InvalidMimeTypeError(string, str"the type must be one of, $list")
  
      def parseSubtype(str: String): Subtype =
        str.indexWhere { ch => ch.isWhitespace || ch.isControl || specials.contains(ch) }.match
          case -1 =>
            if str.startsWith("vnd.") then Subtype.Vendor(str.drop(4))
            else if str.startsWith("prs.") then Subtype.Personal(str.drop(4))
            else if str.startsWith("x.") then Subtype.X(str.drop(2))
            else Subtype.Standard(str)
          
          case idx =>
            throw InvalidMimeTypeError(string, str"the character ${str(idx)} is not allowed")
      
      string.cut(";").map(_.trim).to(List) match
        case Nil    => throw Impossible("cannot return empty list from `cut`")
        case h :: t =>
          val basic = parseBasic(h)
          MimeType(basic(0), basic(1), basic(2), parseParams(t))
    
  final private val specials: Set[Char] = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"',
      '/', '[', ']', '?', '=', '+')

case class InvalidMimeTypeError(value: String, message: String)
extends Exception(txt"gesticulate: \"$value\" is not a valid MIME type; $message".s)