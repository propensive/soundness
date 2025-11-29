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
  def apply
       [label      <: Label: ValueOf,
        children   <: Label: Reifiable to List[String],
        insertable <: Label: Reifiable to List[String]]
       (autoclose: Boolean          = false,
        content:   Html.Content     = Html.Content.Normal,
        presets:   List[Attribute]       = Nil)
  : Tag of label over children =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)
      val implicitTags: Set[Text] = insertable.reification().map(_.tt).to(Set)

      new Tag(valueOf[label].tt, autoclose, content, presets, admissible, implicitTags):
        type Topic = label
        type Transport = children
        type Domain = insertable

  object Container:
    def apply
         [label      <: Label: ValueOf,
          children   <: Label: Reifiable to List[String],
          insertable <: Label: Reifiable to List[String]]
         (autoclose: Boolean      = false,
          content:   Html.Content = Html.Content.Normal,
          presets:   List[Attribute]   = Nil)
    : Container of label over children =

        val admissible: Set[Text] = children.reification().map(_.tt).to(Set)
        val implicitTags: Set[Text] = insertable.reification().map(_.tt).to(Set)

        new Container(valueOf[label].tt, autoclose, content, presets, admissible, implicitTags):
          type Topic = label
          type Transport = children
          type Domain = insertable

  class Container
         (name:      Text,
          autoclose:  Boolean         = false,
          content:    Html.Content    = Html.Content.Normal,
          presets:    List[Attribute] = Nil,
          admissible: Set[Text]       = Set(),
          insertable: Set[Text]       = Set())
  extends Tag(name, autoclose, content, presets, admissible, insertable):
    override def void = false

    def applyDynamic(method: "apply")(children: into[Html of Transport]*)
    : Html.Node of Topic =

        Html.Node(name, Nil, children).asInstanceOf[Html.Node of Topic]

class Tag
       (    tagname:    Text,
        val autoclose:  Boolean         = false,
        val content:    Html.Content    = Html.Content.Normal,
        val presets:    List[Attribute] = Nil,
        val admissible: Set[Text]       = Set(),
        val insertable: Set[Text]       = Set())
extends Html.Node(tagname, Nil, Nil), Dynamic:

  def void: Boolean = true

  def applyDynamicNamed(method: "apply")(attributes: into[Optional[Attribute of Topic]]*)
  : Html.Node & Vacant over Transport =

      Html.Node(tagname, attributes.to(List).compact, Nil)
      . asInstanceOf[Html.Node & Vacant over Transport]


erased trait Vacant extends Transportive

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

case class Attribute(key: Text, value: Text) extends Topical

extension (node: Html.Node & Vacant)
  def apply(children: into[Html of node.Transport]*): Html.Node =
    Html.Node(node.tagname, node.attributes, children)

trait Dom:
  def root: Tag
  val elements: Dictionary[Tag]
  val entities: Dictionary[Text]

  def infer(parent: Text, tag: Text): Optional[Tag]

given html5Dom: Dom:
  import Html.Issue.*

  private val inferences: scm.HashMap[Text, scm.HashMap[Text, Optional[Tag]]] = scm.HashMap()

  private def recur(tagname: Text, target: Text): Boolean =
    elements(tagname).lay(false): tag =>
      tag.admissible(target) || tag.insertable.exists(recur(_, target))

  def infer(parent: Text, target: Text): Optional[Tag] =
    inferences.establish(parent)(new scm.HashMap()).establish(target):
      elements(parent).let: parent =>
        parent.insertable.find(recur(_, target)).optional.let(elements(_))

  def root = Root

  private type InteractivePhrasing =
    "a" | "audio" | "button" | "embed" | "iframe" | "img" | "input" | "label" | "select"
    | "textarea" | "video"

  type Interactive = InteractivePhrasing | "details"

  type Flow =
    Heading | Phrasing | Sectioning | "address" | "blockquote" | "details" | "dialog" | "div" | "dl"
    | "fieldset" | "figure" | "footer" | "form" | "header" | "hr" | "main" | "menu" | "ol" | "p"
    | "pre" | "table" | "ul" | "search"

  type Phrasing =
    Embedded | InteractivePhrasing | "abbr" | "area" | "b" | "bdi" | "bdo" | "br" | "button"
    | "cite" | "code" | "data" | "datalist" | "del" | "dfn" | "em" | "i" | "input" | "ins" | "kbd"
    | "label" | "link" | "map" | "mark" | "meta" | "meter" | "noscript" | "output" | "progress"
    | "q" | "ruby" | "s" | "samp" | "script" | "select" | "slot" | "small" | "span" | "strong"
    | "sub" | "sup" | "template" | "textarea" | "time" | "u" | "var" | "wbr" | "selectedcontent"

  type Embedded =
    "audio" | "canvas" | "embed" | "iframe" | "img" | "object" | "picture" | "video" | "math"
    | "svg"

  type Sectioning = "article" | "aside" | "nav" | "section"
  type ScriptSupporting = "script" | "template"
  type Metadata = "base" | "link" | "meta" | "noscript" | "script" | "style" | "template" | "title"
  type Heading = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hgroup"



  // - should be transparent
  val A = Tag.Container["a", Flow, ""]()
  val Abbr = Tag.Container["abbr", Phrasing, ""]()

  val Address =
    Tag.Container
     ["address",
      "a" | "abbr" | "area" | "audio" | "b" | "bdi" | "bdo" | "blockquote" | "br" | "button"
      | "canvas" | "cite" | "code" | "data" | "datalist" | "del" | "details" | "dfn" | "dialog"
      | "div" | "dl" | "em" | "embed" | "fieldset" | "figure" | "form" | "hr" | "i" | "iframe"
      | "img" | "input" | "ins" | "kbd" | "label" | "link" | "main" | "map" | "mark" | "menu"
      | "meta" | "meter" | "noscript" | "object" | "ol" | "output" | "p" | "picture" | "pre"
      | "progress" | "q" | "ruby" | "s" | "samp" | "script" | "select" | "slot" | "small" | "span"
      | "strong" | "sub" | "sup" | "table" | "template" | "textarea" | "time" | "u" | "ul" | "var"
      | "video" | "wbr",
      ""]
     ()

  val Area = Tag["area", "", ""]()
  val Article = Tag.Container["article", Flow, ""]()

  val Base = Tag["base", "", ""]()
  val Br = Tag["br", "", ""]()
  val Col = Tag["col", "", ""]()
  val Command = Tag["command", "", ""]()
  val Embed = Tag["embed", "", ""]()
  val Em = Tag.Container["em", Phrasing, ""]()
  val Hr = Tag["hr", "", ""]()
  val Img = Tag["img", "", ""]()

  object Input extends Tag("input", autoclose = false):
    type Topic = "input"
    type Transport = ""

    val Button = Tag["input", "", ""](autoclose = autoclose, presets = List(Attribute(t"type", t"button")))

  val Link = Tag["link", "", ""]()
  val Meta = Tag["meta", "", ""]()
  val Param = Tag["param", "", ""]()
  val Source = Tag["source", "", ""]()
  val Script = Tag.Container["script", "#text", ""](content = Html.Content.Raw)
  val Style = Tag.Container["style", "", ""](content = Html.Content.Raw)
  val Track = Tag["track", "", ""]()
  val Wbr = Tag["wbr", "", ""]()

  val Root = Tag.Container["#root", "html", "html"]()
  val Head = Tag.Container["head", Metadata, ""](autoclose = true)
  val Title = Tag.Container["title", "#text", ""]()
  val Body = Tag.Container["body", Flow, ""](autoclose = true)
  val Div = Tag.Container["div", "p" | "ul" | "ol" | "area" | "#text", ""]()
  val Li = Tag.Container["li", "p" | "#text", ""](autoclose = true)
  val Ol = Tag.Container["ol", "li", ""]()
  val P = Tag.Container["p", "i" | "em" | "strong" | "#text", ""](autoclose = true)
  val B = Tag.Container["b", "i" | "em" | "strong" | "#text", ""]()
  val Ul = Tag.Container["ul", "li", ""]()

  val elements: Dictionary[Tag] =
    val list =
      List
       (Root, Html, Area, Base, Br, Col, Command, Embed, Hr, Img, Input, Link, Meta, Param,
        Source, Track, Wbr, Head, Body, Div, Li, Ol, P, Ul, Em, B, Script, Style)

    Dictionary(list.bi.map(_.tagname -> _)*)

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)

object Html
extends Tag.Container(name = "html", autoclose = true, admissible = Set("head", "body"), insertable = Set("head", "body")), Format:
  type Topic = "html"
  type Domain = "head" | "body"
  type Transport = "head" | "body"

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
      case InvalidTag(name)              =>  m"<$name> is not a valid HTML5 tag"
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
  extends Html, Topical, Transportive, Domainal:

    override def toString(): String =
      val tagContent = if attributes == Nil then t"" else
        attributes.map { case Attribute(key, value) => t"""$key="$value"""" }.join(t" ", t" ", t"")

      t"<$tagname$tagContent>${children.map(_.toString.tt).join}</$tagname>".s

  enum Content:
    case Raw, Rcdata, Whitespace, Normal

  def parse(input: Iterator[Text])(using dom: Dom): Html raises ParseError =
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
        case '='                             =>  next() yet skip() yet true
        case '>'                             =>  false
        case ' ' | '\f' | '\n' | '\r' | '\t' =>  false
        case char                            =>  fail(Unexpected(char))


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
      if parent != dom.root then
        if parent.autoclose then Html.Node(parent.tagname, parent.attributes, children)
        else fail(Incomplete(parent.tagname))
      else if children.length > 1 then Html.Fragment(children.reverse*) else children(0)

    @tailrec
    def read(parent: Tag, children: List[Html]): Html =
      cursor.lay(finish(parent, children)):
        case '&'  => parent.content match
          case Html.Content.Whitespace => fail(OnlyWhitespace('&'))
          case _ =>
            val child = cursor.hold:
              val start = cursor.mark
              next()
              entity(cursor.mark).or(textual(start))

            read(parent, append(child, children))

        case '<'  =>
          var level: Int = 0
          var descent: Tag = dom.root
          val node: Html = cursor.hold:
            val mark = cursor.mark

            next()
            tag() match
              case Token.Comment =>  Html.Comment(content)
              case Token.Empty   =>
                val tag = dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))
                if parent.admissible(content) then Html.Node(content, extra, Nil)
                else fail(InadmissibleTag(content, parent.tagname))


              case Token.Open =>
                val tag = dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))
                if tag.void then Html.Node(content, extra, Nil)
                else
                  if parent.admissible(tag.tagname) then descend(tag)
                  else dom.infer(parent.tagname, tag.tagname).let: child =>
                    cursor.cue(mark)
                    level = 1
                    descent = child
                    Html.Textual("")
                  . or:
                      if parent.autoclose then
                        cursor.cue(mark)
                        level = -1
                        Html.Node(parent.tagname, parent.attributes, children.reverse)
                      else fail(InadmissibleTag(content, parent.tagname))

              case Token.Close =>
                if content != parent.tagname then
                  cursor.cue(mark)
                  if parent.autoclose then
                    level = -1
                    Html.Node(parent.tagname, parent.attributes, children.reverse)
                  else fail(MismatchedTag(parent.tagname, content))
                else
                  cursor.next()
                  level = -1
                  Html.Node(content, parent.attributes, children.reverse)

          level match
            case -1 => node
            case  0 => read(parent, node :: children)
            case  1 => read(parent, descend(descent) :: children)

        case char => parent.content match
          case Html.Content.Whitespace =>
            whitespace() yet read(parent, children)

          case Html.Content.Raw =>
            val content = cursor.hold(raw(parent.tagname, cursor.mark))
            Html.Node(parent.tagname, parent.attributes, List(Html.Textual(content)))

          case Html.Content.Rcdata => // FIXME
            val content = cursor.hold(raw(parent.tagname, cursor.mark))
            Html.Node(parent.tagname, parent.attributes, List(Html.Textual(content)))

          case Html.Content.Normal =>
            read(parent, append(cursor.hold(textual(cursor.mark)), children))

    skip()
    read(dom.root, Nil)

sealed trait Html extends Topical:
  type Topic
  enum Token:
    case Close, Comment, Empty, Open
