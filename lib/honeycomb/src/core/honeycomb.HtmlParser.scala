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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package honeycomb

import language.dynamics

import java.lang as jl

import scala.collection.mutable as scm

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import turbulence.*
import typonym.*
import vacuous.*
import zephyrine.*

import classloaders.threadContext
import charDecoders.utf8
import textSanitizers.skip

object Tag:

  def root(children: Set[Text]): Tag =
    new Tag("#root", false, Html.TextContent.Normal, Nil, children, false)

  def void
       [label      <: Label: ValueOf]
       (autoclose:  Boolean          = false,
        content:    Html.TextContent = Html.TextContent.Normal,
        presets:    List[Attribute]  = Nil)
  : Tag of label over "" =

      new Tag(valueOf[label].tt, autoclose, content, presets, Set(), false):
        type Topic = label
        type Transport = ""

  def container
       [label      <: Label: ValueOf,
        children   <: Label: Reifiable to List[String]]
       (autoclose:  Boolean          = false,
        content:    Html.TextContent = Html.TextContent.Normal,
        presets:    List[Attribute]  = Nil,
        insertable: Boolean          = false)
  : Container of label over children =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)

      new Container(valueOf[label].tt, autoclose, content, presets, admissible, insertable):
        type Topic = label
        type Transport = children

  class Container
         (name:      Text,
          autoclose:  Boolean          = false,
          content:    Html.TextContent = Html.TextContent.Normal,
          presets:    List[Attribute]  = Nil,
          admissible: Set[Text]        = Set(),
          insertable: Boolean          = false)
  extends Tag(name, autoclose, content, presets, admissible, insertable):
    override def void = false

    def applyDynamic(method: "apply")(children: Html of Transport*)
    : Html.Node of Topic =

        Html.Node(name, Nil, children).asInstanceOf[Html.Node of Topic]

class Tag
       (    tagname:    Text,
        val autoclose:  Boolean          = false,
        val content:    Html.TextContent = Html.TextContent.Normal,
        val presets:    List[Attribute]  = Nil,
        val admissible: Set[Text]        = Set(),
        val insertable: Boolean          = false)
extends Html.Node(tagname, Nil, Nil), Dynamic:

  def void: Boolean = true

  def applyDynamicNamed(method: "apply")(attributes: Optional[Attribute of Topic]*)
  : Html.Node & Html.Vacant of Topic over Transport =

      Html.Node(tagname, attributes.to(List).compact, Nil)
      . asInstanceOf[Html.Node & Html.Vacant of Topic over Transport]


object data extends Dynamic:
  def applyDynamic(name: String)(value: Text): (String, Text) = (name, value)
  def updateDynamic(name: String)(value: Text): (String, Text) = (name, value)

object Attribute:
  given conversion: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, Text), Optional[Attribute of tag]] =
    attribute => Attribute(attribute(0), attribute(1)).asInstanceOf[Attribute of tag]

  given conversion2: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, String), Optional[Attribute of tag]] =
    attribute => Attribute(attribute(0), attribute(1).tt).asInstanceOf[Attribute of tag]

  given conversion3: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, Boolean), Optional[Attribute of tag]] =
    attribute => Attribute(attribute(0), attribute(0).tt).asInstanceOf[Attribute of tag].unless(!attribute(1))

into case class Attribute(key: Text, value: Optional[Text]) extends Topical

extension (node: Html.Node & Html.Vacant)
  def apply(children: Html of node.Transport*): Html.Node =
    Html.Node(node.tagname, node.attributes, children)

trait Dom:
  val elements: Dictionary[Tag]
  val entities: Dictionary[Text]

  def infer(parent: Tag, child: Tag): Optional[Tag]

given html5Dom: Dom:
  import Html.Issue.*

  private val inferences: scm.HashMap[Text, scm.HashMap[Text, Optional[Tag]]] = scm.HashMap()

  private def recur(tagname: Text, target: Text): Boolean =
    elements(tagname).lay(false): tag =>
      tag.admissible(target) || tag.insertable

  private type InteractivePhrasing =
    "a" | "audio" | "button" | "embed" | "iframe" | "img" | "input" | "label" | "select"
    | "textarea" | "video"

  type Interactive = InteractivePhrasing | "details"

  type Flow =
    Heading | Phrasing | Sectioning | "address" | "blockquote" | "details" | "dialog" | "div" | "dl"
    | "fieldset" | "figure" | "footer" | "form" | "header" | "hr" | "main" | "menu" | "ol" | "p"
    | "pre" | "table" | "ul" | "search"

  type Phrasing =
    Embedded | InteractivePhrasing | "abbr" | "area" | "b" | "bdi" | "bdo" | "br" | "cite" | "code"
    | "data" | "datalist" | "del" | "dfn" | "em" | "i" | "ins" | "kbd" | "link" | "map" | "mark"
    | "meta" | "meter" | "noscript" | "output" | "progress" | "q" | "ruby" | "s" | "samp" | "script"
    | "slot" | "small" | "span" | "strong" | "sub" | "sup" | "template" | "time" | "u" | "var"
    | "wbr" | "selectedcontent" | "#text"

  type Embedded =
    "audio" | "canvas" | "embed" | "iframe" | "img" | "object" | "picture" | "video" | "math"
    | "svg"

  type Sectioning = "article" | "aside" | "nav" | "section"
  type ScriptSupporting = "script" | "template"
  type Metadata = "base" | "link" | "meta" | "noscript" | "script" | "style" | "template" | "title"
  type Heading = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hgroup"

  def insertable(tag: Tag): Set[Tag] =
    tag.admissible.map(elements(_)).compact.filter(_.insertable)

  def infer(parent: Tag, child: Tag): Optional[Tag] =
    def recur(parent: Tag): Boolean =
      parent.admissible.contains(child.tagname) || insertable(parent).exists(recur(_))

    insertable(parent).find(recur(_)).optional


  // - should be transparent
  val A = Tag.container["a", Flow | "#transparent"]()
  val Abbr = Tag.container["abbr", Phrasing]()

  val Address =
    Tag.container
     ["address",
      "a" | "abbr" | "area" | "audio" | "b" | "bdi" | "bdo" | "blockquote" | "br" | "button"
      | "canvas" | "cite" | "code" | "data" | "datalist" | "del" | "details" | "dfn" | "dialog"
      | "div" | "dl" | "em" | "embed" | "fieldset" | "figure" | "form" | "hr" | "i" | "iframe"
      | "img" | "input" | "ins" | "kbd" | "label" | "link" | "main" | "map" | "mark" | "menu"
      | "meta" | "meter" | "noscript" | "object" | "ol" | "output" | "p" | "picture" | "pre"
      | "progress" | "q" | "ruby" | "s" | "samp" | "script" | "select" | "slot" | "small" | "span"
      | "strong" | "sub" | "sup" | "table" | "template" | "textarea" | "time" | "u" | "ul" | "var"
      | "video" | "wbr"]
     ()

  val Area = Tag.void["area"]()
  val Article = Tag.container["article", Flow]()
  val Aside = Tag.container["aside", Flow]()

  // - transparent content
  // - audio and video are prohibited in transparent content
  // - conditions based on presence or absence of `src` attribute
  val Audio = Tag.container["audio", "source" | "track" | "#transparent"]

  val B = Tag.container["b", Phrasing]()

  // - `href` or `target` attributes are required
  val Base = Tag.void["base"]()

  val Bdi = Tag.container["bdi", Phrasing]()
  val Bdo = Tag.container["bdo", Phrasing]()
  val Blockquote = Tag.container["blockquote", Flow]()
  val Body = Tag.container["body", Flow](autoclose = true, insertable = true)
  val Br = Tag.void["br"]()

  // - constraints on content
  val Button = Tag.container["button", Phrasing]()

  // - transparent, but non-interactive
  val Canvas = Tag.container["canvas", "#transparent"]()

  val Caption = Tag.container["caption", Flow]()
  val Cite = Tag.container["cite", Phrasing]()
  val Code = Tag.container["code", Phrasing]()
  val Col = Tag.void["col"]()

  val Colgroup = Tag.container["colgroup", "col"]
                  (content = Html.TextContent.Whitespace, insertable = true)

  val Data = Tag.container["data", Phrasing]()
  val Datalist = Tag.container["datalist", Phrasing | "option"]()
  val Dd = Tag.container["dd", Flow](autoclose = true)
  val Del = Tag.container["del", "#transparent"]()
  val Details = Tag.container["details", "summary" | Flow]()
  val Dfn = Tag.container["dfn", Phrasing]()
  val Dialog = Tag.container["dialog", Flow]()
  val Div = Tag.container["div", Flow]()

  val Dl = Tag.container["dl", "div" | "dt" | ScriptSupporting]
            (autoclose = true, content = Html.TextContent.Whitespace)

  val Dt = Tag.container["dl", Flow](autoclose = true)
  val Em = Tag.container["em", Phrasing]()
  val Embed = Tag.void["embed"]()
  val Fieldset = Tag.container["fieldset", "legend" | Flow]()
  val Figcaption = Tag.container["figcaption", Flow]()
  val Figure = Tag.container["figure", "figcaption" | Flow]()
  val Footer = Tag.container["footer", Flow]()
  val Form = Tag.container["form", Flow]()
  val H1 = Tag.container["h1", Phrasing]()
  val H2 = Tag.container["h2", Phrasing]()
  val H3 = Tag.container["h3", Phrasing]()
  val H4 = Tag.container["h4", Phrasing]()
  val H5 = Tag.container["h5", Phrasing]()
  val H6 = Tag.container["h6", Phrasing]()

  val Head = Tag.container["head", Metadata]
              (autoclose = true, content = Html.TextContent.Whitespace, insertable = true)

  val Header = Tag.container["header", Flow](autoclose = true)
  val Hgroup = Tag.container["hgroup", "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"]()
  val Hr = Tag.void["hr"]()
  val Html = honeycomb.Html
  val I = Tag.container["i", Phrasing]()
  val Iframe = Tag.void["iframe"]()
  val Img = Tag.void["img"]()

  object Input extends Tag("input"):
    type Topic = "input"
    type Transport = ""

    val Button = Tag.void["input"]
                  (autoclose = autoclose, presets = List(Attribute(t"type", t"button")))
    // FIXME: More Input types

  val Ins = Tag.container["ins", "#transparent"]()
  val Kbd = Tag.container["kbd", Phrasing]()
  val Label = Tag.container["label", Phrasing]()
  val Legend = Tag.container["label", Phrasing | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"]()
  val Li = Tag.container["li", Flow](autoclose = true)
  val Link = Tag.void["link"]()
  val Main = Tag.container["main", Flow]()
  val Map = Tag.container["map", "#transparent"]
  val Mark = Tag.container["mark", Phrasing]
  val Menu = Tag.container["menu", "li" | ScriptSupporting](content = Html.TextContent.Whitespace)
  val Meta = Tag.void["meta"]()
  val Meter = Tag.container["meter", Phrasing]()
  val Nav = Tag.container["nav", Flow]()
  val Noscript = Tag.container["noscript", "link" | "style" | "meta"]()
  val Object = Tag.container["object", "#transparent"]()
  val Ol = Tag.container["ol", "li" | ScriptSupporting](content = Html.TextContent.Whitespace)

  val Optgroup = Tag.container["optgroup", "option" | "legend"]
                  (autoclose = true, content = Html.TextContent.Whitespace)

  val Option = Tag.container["option", "#text"](autoclose = true)
  val Output = Tag.container["output", Phrasing]()
  val P = Tag.container["p", Phrasing](autoclose = true)

  val Picture = Tag.container["picture", "source" | "img" | ScriptSupporting]
                 (content = Html.TextContent.Whitespace)

  val Pre = Tag.container["pre", Phrasing]()
  val Progress = Tag.container["progress", Phrasing]()
  val Q = Tag.container["q", Phrasing]()
  val Rp = Tag.container["rp", "#text"](autoclose = true)
  val Rt = Tag.container["rt", Phrasing](autoclose = true)
  val Ruby = Tag.container["ruby", Phrasing | "rt" | "rp"]()
  val S = Tag.container["s", Phrasing]()
  val Samp = Tag.container["samp", Phrasing]()
  val Script = Tag.container["script", "#text"](content = Html.TextContent.Raw)
  val Search = Tag.container["search", Flow]()
  val Section = Tag.container["section", Flow]()

  val Select =
    Tag.container
     ["select", "option" | "optgroup" | "hr" | "button" | "noscript" | ScriptSupporting]
     (content = Html.TextContent.Whitespace)

  val Slot = Tag.container["slot", "#transparent"]()
  val Small = Tag.container["small", Phrasing]()
  val Source = Tag.void["source"]()
  val Span = Tag.container["span", Phrasing]()
  val Strong = Tag.container["strong", Phrasing]()
  val Style = Tag.container["style", "#text"](content = Html.TextContent.Raw)
  val Sub = Tag.container["sub", Phrasing]()
  val Summary = Tag.container["summary", Phrasing | Heading]()
  val Sup = Tag.container["sup", Phrasing]()

  val Table =
    Tag.container["table", "caption" | "colgroup" | "thead" | "tbody" | "tfoot"]
     (content = Html.TextContent.Whitespace)

  val Tbody = Tag.container["tbody", "tr"]
               (autoclose = true, content = Html.TextContent.Whitespace, insertable = true)

  val Td = Tag.container["td", Flow](autoclose = true)
  val Template = Tag.void["template"]()
  val Textarea = Tag.container["textarea", "#text"](content = Html.TextContent.Rcdata)

  val Tfoot = Tag.container["tfoot", "tr"]
               (autoclose = true, content = Html.TextContent.Whitespace)

  val Th = Tag.container["th", Flow](autoclose = true)

  val Thead = Tag.container["thead", "tr" | ScriptSupporting]
               (autoclose = true, content = Html.TextContent.Whitespace)

  val Time = Tag.container["time", Phrasing]()
  val Title = Tag.container["title", "#text"](content = Html.TextContent.Rcdata)

  val Tr = Tag.container["tr", "td" | "th" | ScriptSupporting]
            (autoclose = true, content = Html.TextContent.Whitespace, insertable = true)

  val Track = Tag.void["track"]()
  val U = Tag.container["u", Phrasing]()
  val Ul = Tag.container["ul", "li" | ScriptSupporting](content = Html.TextContent.Whitespace)
  val Var = Tag.container["var", Phrasing]()
  val Video = Tag.container["video", "track" | "#transparent" | "source"]()
  val Wbr = Tag.void["wbr"]()

  val elements: Dictionary[Tag] =
    Dictionary(this.membersOfType[Tag].to(Seq).bi.map(_.tagname -> _)*)

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)

object Html extends Tag.Container
         (name       = "html",
          autoclose  = true,
          admissible = Set("head", "body"),
          content    = Html.TextContent.Whitespace,
          insertable = true), Format:
  type Topic = "html"
  type Transport = "head" | "body"

  given aggregable: [content <: Label: Reifiable to List[String]]
        =>  Tactic[ParseError]
        => (Html of content) is Aggregable by Text =

    input =>
      val root = Tag.root(content.reification().map(_.tt).to(Set))
      parse(input.iterator, root).asInstanceOf[Html of content]


  erased trait Vacant extends Transportive { this: Html => }
  erased trait Transparent extends Transportive { this: Html => }

  private enum Token:
    case Close, Comment, Empty, Open

  import Issue.*
  def name: Text = t"HTML"

  given conversion: [label >: "#text" <: Label] => Conversion[Text, Html of label] =
    Html.Textual(_).asInstanceOf[Html.Textual of label]

  given conversion2: [label >: "#text" <: Label] => Conversion[String, Html of label] =
    string => Html.Textual(string.tt).asInstanceOf[Html.Textual of label]

  given conversion3: [label <: Label, content >: label]
        =>  Conversion[Html.Node of label, Html of content] =
    _.asInstanceOf[Html.Node of content]

  enum Issue extends Format.Issue:
    case ExpectedMore
    case InvalidTag(name: Text)
    case InadmissibleTag(name: Text, parent: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)
    case Incomplete(tag: Text)

    def describe: Message = this match
      case ExpectedMore                  =>  m"the content ended prematurely"
      case InvalidTag(name)              =>  m"<$name> is not a valid tag"
      case InadmissibleTag(name, parent) =>  m"<$name> cannot be a child of <$parent>"
      case OnlyWhitespace(char)          =>  m"the character $char was found where only whitespace is permitted"
      case Unexpected(char)              =>  m"the character $char was not expected"
      case UnknownEntity(name)           =>  m"the entity &$name is not defined"
      case ForbiddenUnquoted(char)       =>  m"the character $char is forbidden in an unquoted attribute"
      case MismatchedTag(open, close)    =>  m"the tag </$close> did not match the opening tag <$open>"
      case Incomplete(tag)               =>  m"the content ended while the tag <$tag> was left open"

  case class Position(ordinal: Ordinal) extends Format.Position:
    def describe: Text = t"character ${ordinal.n1}"

  case class Document(nodes: Html*) extends Html

  case class Fragment(nodes: Html*) extends Html:
    override def hashCode: Int = nodes.hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(nodes0*) => nodes0 == nodes
      case node: Html    => nodes.length == 1 && nodes(0) == node
      case _                 => false

  case class Comment(text: Text) extends Html:
    override def hashCode: Int = List(this).hashCode

    override def equals(that: Any): Boolean = that match
      case Comment(text0)           => text0 == text
      case Fragment(Comment(text0)) => text0 == text
      case _                        => false

    override def toString(): String = t"<!--$text-->".s

  case class Textual(text: Text) extends Html:
    type Topic = "#text"
    override def toString(): String = text.s

    override def hashCode: Int = List(this).hashCode

    override def equals(that: Any): Boolean = that match
      case Textual(text0)           => text0 == text
      case Fragment(Textual(text0)) => text0 == text
      case _                        => false

  case class Node(tagname: Text, attributes: List[Attribute], children: Seq[Html])
  extends Html, Topical, Transportive:
    type Foo = this.Transport

    override def toString(): String =
      val tagContent = if attributes == Nil then t"" else
        attributes.map:
          case Attribute(key, value) =>
            value.lay(key): value =>
              t"""$key="$value""""
        . join(t" ", t" ", t"")

      t"<$tagname$tagContent>${children.map(_.toString.tt).join}</$tagname>".s

  enum TextContent:
    case Raw, Rcdata, Whitespace, Normal

  def parse[dom <: Dom](input: Iterator[Text], root: Tag)(using dom: Dom): Html raises ParseError =
    val cursor = Cursor(input)

    def next(): Unit =
      if !cursor.next()
      then raise(ParseError(Html, Html.Position(cursor.position), ExpectedMore))

    inline def expect(char: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum != char then fail(Unexpected(datum))

    def fail(issue: Html.Issue): Nothing =
      abort(ParseError(Html, Html.Position(cursor.position), issue))

    @tailrec
    def skip(): Unit = cursor.lay(fail(ExpectedMore)):
      case ' ' | '\f' | '\n' | '\r' | '\t' => next() yet skip()
      case _                               => ()

    @tailrec
    def whitespace(): Unit = cursor.lay(fail(ExpectedMore)):
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  next() yet whitespace()
      case '<'                             =>  ()
      case char                            =>  fail(OnlyWhitespace(char))

    @tailrec
    def tagname(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case char if char.isLetter           =>  next() yet tagname(mark)
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case '/'                             =>  cursor.grab(mark, cursor.mark)
      case '>'                             =>  cursor.grab(mark, cursor.mark)
      case char                            =>  fail(Unexpected(char))

    @tailrec
    def key(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '-'                             =>  next() yet key(mark)
      case char if char.isLetter           =>  next() yet key(mark)
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case '=' | '>'                       =>  cursor.grab(mark, cursor.mark)
      case char                            =>  fail(Unexpected(char))


    @tailrec
    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '"'  => cursor.grab(mark, cursor.mark).also(next())
      case char => next() yet value(mark)

    @tailrec
    def singleQuoted(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '\'' => cursor.grab(mark, cursor.mark).also(next())
      case char => next() yet singleQuoted(mark)

    @tailrec
    def unquoted(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '>' | ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case char@('"' | '\'' | '<' | '=' | '`')   =>  fail(ForbiddenUnquoted(char))
      case char                                  =>  next() yet unquoted(mark)

    def equality(): Boolean =
      skip()

      cursor.lay(fail(ExpectedMore)):
        case '='                                   =>  next() yet skip() yet true
        case '>' | ' ' | '\f' | '\n' | '\r' | '\t' =>  false
        case char                                  =>  fail(Unexpected(char))


    @tailrec
    def attributes(entries0: List[Attribute] = Nil)(using Cursor.Held): List[Attribute] =
      val name = key(cursor.mark)

      val entries = if equality() then
        val assignment = cursor.lay(fail(ExpectedMore)):
          case '"'  =>  next() yet value(cursor.mark)
          case '\'' =>  next() yet singleQuoted(cursor.mark)
          case _    =>  unquoted(cursor.mark)

        Attribute(name, assignment) :: entries0
      else Attribute(name, name) :: entries0

      skip()

      cursor.lay(fail(ExpectedMore)):
        case '>' =>  cursor.next() yet entries
        case '/' =>  expect('>') yet cursor.next() yet entries
        case _   =>  attributes(entries)

    @tailrec
    def entity(mark: Mark)(using Cursor.Held): Optional[Text] =
      cursor.lay(fail(ExpectedMore)):
        case char if char.isLetter | char.isDigit =>  next() yet entity(mark)
        case '='                                  =>  Unset

        case ';' =>
          cursor.next()
          val name = cursor.grab(mark, cursor.mark)
          dom.entities(name).or(fail(UnknownEntity(name)))

        case char =>
          dom.entities(cursor.grab(mark, cursor.mark))


    @tailrec
    def textual(mark: Mark)(using Cursor.Held): Text =
      cursor.lay(cursor.grab(mark, cursor.mark)):
        case '<' | '&'                        =>  cursor.grab(mark, cursor.mark)
        case ' ' | '\f' | '\n' | '\r' | '\t'  =>  next() yet textual(mark)

        case char =>
          if cursor.next() then textual(mark) else cursor.grab(mark, cursor.mark)

    @tailrec
    def raw(tag: Text, mark: Mark)(using Cursor.Held): Text =
      cursor.lay(cursor.grab(mark, cursor.mark)):
        case '<'  =>  val end = cursor.mark
                      cursor.next()
                      val resume = cursor.mark

                      if cursor.lay(false)(_ == '/') then
                        next()
                        val tagStart = cursor.mark
                        repeat(tag.length)(cursor.next())
                        val candidate = cursor.grab(tagStart, cursor.mark)
                        if cursor.more && candidate == tag then
                          if cursor.lay(false)(_ == '>')
                          then cursor.grab(mark, end).also(cursor.next())
                          else cursor.cue(resume) yet raw(tag, mark)
                        else cursor.cue(resume) yet raw(tag, mark)
                      else cursor.cue(resume) yet raw(tag, mark)

        case char =>  if cursor.next() then raw(tag, mark) else cursor.grab(mark, cursor.mark)

    // mutable state
    var content: Text = t""
    var extra: List[Attribute] = Nil

    def comment(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '-'  =>  val end = cursor.mark
                    next()
                    cursor.lay(fail(ExpectedMore)):
                      case '-' => expect('>') yet cursor.grab(mark, end)
                      case _   => comment(mark)
      case char =>  next() yet comment(mark)

    def tag(): Token =
      cursor.lay(fail(ExpectedMore)):
        case '!'  =>  expect('-')
                      expect('-')
                      next()
                      content = cursor.hold(comment(cursor.mark))
                      cursor.next()
                      Token.Comment
        case '/'  =>  next()
                      content = cursor.hold(tagname(cursor.mark))
                      Token.Close

        case char =>
          val name = cursor.hold(tagname(cursor.mark))
          skip() yet cursor.lay(fail(ExpectedMore)):
            case '/'  =>  expect('>')
                          cursor.next()
                          content = name
                          extra = Nil
                          Token.Empty
            case '>'  =>  cursor.next()
                          content = name
                          extra = Nil
                          Token.Open
            case _    =>  content = name
                          extra = cursor.hold(attributes())
                          Token.Open

    def descend(parent: Tag, children: List[Html] = Nil): Html = read(parent, children)

    def append(text: Text, children: List[Html]): List[Html] =
      if text == "" then children else children match
        case Html.Textual(text0) :: more => Html.Textual(text0+text) :: more
        case _                           => Html.Textual(text) :: children

    def finish(parent: Tag, children: List[Html]): Html =
      if parent != root then
        if parent.autoclose then Html.Node(parent.tagname, parent.attributes, children.reverse)
        else fail(Incomplete(parent.tagname))
      else if children.length > 1 then Html.Fragment(children.reverse*) else children(0)

    @tailrec
    def read(parent: Tag, children: List[Html]): Html =
      cursor.lay(finish(parent, children)):
        case '&'  => parent.content match
          case Html.TextContent.Whitespace => fail(OnlyWhitespace('&'))
          case _ =>
            val child = cursor.hold:
              val start = cursor.mark
              next()
              entity(cursor.mark).or(textual(start))

            read(parent, append(child, children))

        case '<'  =>
          var level: Int = 0
          var ascend: Boolean = false
          val node: Html = cursor.hold:
            val mark = cursor.mark

            def node(): Html.Node = Html.Node(content, extra, children.reverse)
            def close(): Html.Node = Html.Node(parent.tagname, parent.attributes, children.reverse)

            def infer(tag: Tag) =
              cursor.cue(mark)
              dom.infer(parent, tag).let(descend(_)).or:
                if parent.autoclose then close().also { ascend = true }
                else fail(InadmissibleTag(content, parent.tagname))

            next()
            tag() match
              case Token.Comment =>  Html.Comment(content)
              case Token.Empty   =>
                val tag = dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))
                if parent.admissible(content) then node() else infer(tag)


              case Token.Open =>
                val tag = dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))

                if tag.void then
                  if parent.admissible(content) then node() else infer(tag)
                else
                  if parent.admissible(content) then descend(tag) else infer(tag)

              case Token.Close =>
                if content != parent.tagname then
                  cursor.cue(mark)
                  if parent.autoclose then close().also { ascend = true }
                  else fail(MismatchedTag(parent.tagname, content))
                else
                  cursor.next()
                  ascend = true
                  Html.Node(content, parent.attributes, children.reverse)

          if ascend then node else read(parent, node :: children)

        case char => parent.content match
          case Html.TextContent.Whitespace =>
            whitespace() yet read(parent, children)

          case Html.TextContent.Raw =>
            val content = cursor.hold(raw(parent.tagname, cursor.mark))
            Html.Node(parent.tagname, parent.attributes, List(Html.Textual(content)))

          case Html.TextContent.Rcdata => // FIXME
            val content = cursor.hold(raw(parent.tagname, cursor.mark))
            Html.Node(parent.tagname, parent.attributes, List(Html.Textual(content)))

          case Html.TextContent.Normal =>
            read(parent, append(cursor.hold(textual(cursor.mark)), children))

    skip()
    read(root, Nil)

sealed into trait Html extends Topical:
  type Topic
