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
import turbulence.*
import vacuous.*
import zephyrine.*

import classloaders.threadContext
import charDecoders.utf8
import textSanitizers.skip

object HtmlNode extends Format:
  def name: Text = t"HTML"

  given conversion: [label >: "#text" <: Label] => Conversion[Text, HtmlNode of label] =
    HtmlNode.Textual(_).asInstanceOf[HtmlNode.Textual { type Topic = label }]

  given conversion2: [label >: "#text" <: Label] => Conversion[String, HtmlNode of label] =
    string => HtmlNode.Textual(string.tt).asInstanceOf[HtmlNode.Textual { type Topic = label }]

  given conversion3: [label <: Label, content >: label]
        =>  Conversion[HtmlNode.Node over content, HtmlNode of label] =
    _.asInstanceOf[HtmlNode.Node { type Topic = label }]

  enum Issue extends Format.Issue:
    case ExpectedMore
    case UnknownTag(name: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)

    def describe: Message = this match
      case ExpectedMore               =>  m"the content ended prematurely"
      case UnknownTag(name)           =>  m"the tag <$name> is not valid HTML5"
      case OnlyWhitespace(char)       =>  m"the character $char was found where only whitespace is permitted"
      case Unexpected(char)           =>  m"the character $char was not expected"
      case UnknownEntity(name)        =>  m"the entity &$name is not defined"
      case ForbiddenUnquoted(char)    =>  m"the character $char is forbidden in an unquoted attribute"
      case MismatchedTag(open, close) =>  m"the tag </$close> did not match the opening tag <$open>"

  case class Position(ordinal: Ordinal) extends Format.Position:
    def describe: Text = t"character ${ordinal.n1}"

  case class Document(nodes: HtmlNode*) extends HtmlNode
  case class Comment(text: Text) extends HtmlNode:
    override def toString(): String = t"<!--$text-->".s

  case class Textual(text: Text) extends HtmlNode:
    override def toString(): String = text.s

  case class Node(name: Text, attributes: List[Pair], children: Seq[HtmlNode])
  extends HtmlNode, Topical, Transportive:

    override def toString(): String =
      val tagContent = if attributes == Nil then t"" else
        attributes.map { case Pair(key, value) => t"""$key="$value"""" }.join(t" ", t" ", t"")

      t"<$name$tagContent>${children.map(_.toString.tt).join}</$name>".s

  enum Content:
    case Raw, Rcdata, Whitespace, Normal

sealed trait HtmlNode extends Topical:
  type Topic

erased trait Vacant extends Transportive

object data extends Dynamic:
  def applyDynamic(name: String)(value: Text): (String, Text) = (name, value)
  def updateDynamic(name: String)(value: Text): (String, Text) = (name, value)

object Pair:
  given conversion: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, Text), Optional[Pair of tag]] =
    pair => Pair(pair(0), pair(1)).asInstanceOf[Pair of tag]

  given conversion2: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, String), Optional[Pair of tag]] =
    pair => Pair(pair(0), pair(1).tt).asInstanceOf[Pair of tag]

  given conversion3: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, Boolean), Optional[Pair of tag]] =
    pair => Pair(pair(0), pair(0).tt).asInstanceOf[Pair of tag].unless(!pair(1))

case class Pair(key: Text, value: Text) extends Topical

extension (node: HtmlNode.Node & Vacant)
  def apply(children: into[HtmlNode of node.Transport]*): HtmlNode.Node =
    HtmlNode.Node(node.name, node.attributes, children)



object Dom:
  import HtmlNode.Issue.*

  object Tag:
    def apply[label <: Label: {Precise, ValueOf}, children <: Label]
         (void:      Boolean          = false,
          autoclose: Boolean          = false,
          content:   HtmlNode.Content = HtmlNode.Content.Normal,
          presets:   List[Pair]       = Nil)
    : Tag of label over children =

        new Tag(valueOf[label].tt, void, autoclose, content, presets):
          type Topic = label
          type Transport = children


  class Tag
         (    name0:     Text,
          val void:      Boolean          = false,
          val autoclose: Boolean          = false,
          val content:   HtmlNode.Content = HtmlNode.Content.Normal,
          val presets:   List[Pair]       = Nil)
  extends HtmlNode.Node(name0, Nil, Nil), Dynamic:
    type Topic
    type Transport

    def applyDynamic(method: "apply")(children: into[HtmlNode of Transport]*)
    : HtmlNode.Node of Topic =

        HtmlNode.Node(name, Nil, children).asInstanceOf[HtmlNode.Node of Topic]


    def applyDynamicNamed(method: "apply")(attributes: into[Optional[Pair of Topic]]*)
    : HtmlNode.Node & Vacant over Transport =

        HtmlNode.Node(name, attributes.to(List).compact, Nil)
        . asInstanceOf[HtmlNode.Node & Vacant over Transport]

  val Area = Tag["area", Label](void = true)
  val Base = Tag["base", Label](void = true)
  val Br = Tag["br", Label](void = true)
  val Col = Tag["col", Label](void = true)
  val Command = Tag["command", Label](void = true)
  val Embed = Tag["embed", Label](void = true)
  val Em = Tag["em", Label]()
  val Hr = Tag["hr", Label](void = true)
  val Img = Tag["img", Label](void = true)

  object Input extends Tag("input", void = true, autoclose = false):
    type Topic = "input"
    type Transport = Label

    val Button = Tag["input", Label](void = true, presets = List(Pair(t"type", t"button")))

  val Link = Tag["link", Label](void = true)
  val Meta = Tag["meta", Label](void = true)
  val Param = Tag["param", Label](void = true)
  val Source = Tag["source", Label](void = true)
  val Script = Tag["script", Label](content = HtmlNode.Content.Raw)
  val Style = Tag["style", Label](content = HtmlNode.Content.Raw)
  val Track = Tag["track", Label](void = true)
  val Wbr = Tag["wbr", Label](void = true)

  val Head = Tag["head", "meta" | "style"](autoclose = true)
  val Body = Tag["body", "div"](autoclose = true)
  val Div = Tag["div", "p" | "ul" | "ol" | "#text"]()
  val Li = Tag["li", "p" | "#text"](autoclose = true)
  val Ol = Tag["ol", "li"]()
  val P = Tag["p", "i" | "em" | "strong" | "#text"](autoclose = true)
  val B = Tag["b", "i" | "em" | "strong" | "#text"]()
  val Ul = Tag["ul", "li"]()

  val elements: Dictionary[Tag] =
    val list =
      List
       (Area, Base, Br, Col, Command, Embed, Hr, Img, Input, Link, Meta, Param,
        Source, Track, Wbr, Head, Body, Div, Li, Ol, P, Ul, Em, B, Script, Style)

    Dictionary(list.bi.map(_.name -> _)*)

  // val elements: Set[Tag] =
  //   Set
  //    (A, Abbr, Address, Area, Article, Aside, Audio, B, Base, Bdi, Bdo, Blockquote, Body, Br,
  //     Button, Canvas, Caption, Cite, Code, Col, Colgroup, Data, Datalist, Dd, Del, Details, Dfn,
  //     Dialog, Div, Dl, Dt, Em, Embed, Fencedframe, Fieldset, Figcaption, Figure, Footer, Form, H1,
  //     H2, H3, H4, H5, H6, Head, Header, Hgroup, Hr, Html, I, Iframe, Img, Input, Ins, Kbd, Label,
  //     Legend, Li, Link, Main, Map, Mark, Menu, Meta, Meter, Nav, Noscript, Object, Ol, Optgroup,
  //     Option, Output, P, Picture, Pre, Progress, Q, Rp, Rt, Ruby, S, Samp, Script, Search, Section,
  //     Select, Selectedcontent, Slot, Small, Source, Span, Strong, Style, Sub, Summary, Sup, Table,
  //     Tbody, Td, Template, Textarea, Tfoot, Th, Thead, Time, Title, Tr, Track, U, Ul, Var, Video,
  //     Wbr)

  val rawElements: Set[Text] = Set("script", "style")

  val rcdataElements: Set[Text] = Set("textarea", "title")

  // val textless: Set[Text] =
  //   Set("html", "head", "table", "colgroup", "thead", "tbody", "tfoot", "tr", "ul", "ol", "menu",
  //       "dl", "select", "optgroup", "datalist", "picture", "template")

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)

  enum Token:
    case Close(name: Text)
    case Comment(text: Text)
    case Empty(name: Text, attributes: List[Pair])
    case Open(name: Text, attributes: List[Pair])

  def parse(input: Iterator[Text]): HtmlNode raises ParseError =
    val cursor = Cursor(input)

    def next(): Unit =
      if !cursor.next()
      then raise(ParseError(HtmlNode, HtmlNode.Position(cursor.position), ExpectedMore))

    inline def expect(char: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum != char
        then panic(m"expected $char but found $datum at ${cursor.position.n0}")

    def fail(issue: HtmlNode.Issue): Nothing =
      abort(ParseError(HtmlNode, HtmlNode.Position(cursor.position), issue))

    @tailrec
    def whitespace(): Unit = cursor.lay(fail(ExpectedMore)):
      case ' ' | '\f' | '\n' | '\r' | '\t' => next() yet whitespace()
      case _                               => ()

    @tailrec
    def onlyWhitespace(): Unit = cursor.lay(fail(ExpectedMore)):
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  next() yet onlyWhitespace()
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
    def singleQuotedValue(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '\'' => cursor.grab(mark, cursor.mark).also(next())
      case char => next() yet singleQuotedValue(mark)

    @tailrec
    def unquotedValue(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '>' | ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case char@('"' | '\'' | '<' | '=' | '`')   =>  fail(ForbiddenUnquoted(char))
      case char                                  =>  next() yet unquotedValue(mark)

    def equality(): Boolean =
      whitespace()

      cursor.lay(fail(ExpectedMore)):
        case '='                             =>  next() yet whitespace() yet true
        case '>'                             =>  false
        case ' ' | '\f' | '\n' | '\r' | '\t' =>  false
        case char                            =>  fail(Unexpected(char))


    @tailrec
    def attributes(entries0: List[Pair] = Nil)(using Cursor.Held): List[Pair] =
      val name = key(cursor.mark)

      val entries = if equality() then
        val assignment = cursor.lay(fail(ExpectedMore)):
          case '"'  =>  next() yet value(cursor.mark)
          case '\'' =>  next() yet singleQuotedValue(cursor.mark)
          case _    =>  unquotedValue(cursor.mark)

        Pair(name, assignment) :: entries0
      else Pair(name, name) :: entries0

      whitespace()

      cursor.lay(fail(ExpectedMore)):
        case '>' =>  cursor.next() yet entries
        case '/' =>  expect('>') yet cursor.next() yet entries
        case _   =>  attributes(entries)

    def entity(mark: Mark)(using Cursor.Held): Optional[HtmlNode.Textual] = cursor.lay(fail(ExpectedMore)):
      case char if char.isLetter | char.isDigit =>  next() yet entity(mark)
      case '='                                  =>  Unset

      case ';' =>
        next()
        val name = cursor.grab(mark, cursor.mark)
        HtmlNode.Textual(entities(name).or(fail(UnknownEntity(name))))

      case char =>
        entities(cursor.grab(mark, cursor.mark)).let(HtmlNode.Textual(_))


    def textual(mark: Mark)(using Cursor.Held): HtmlNode =
      cursor.lay(HtmlNode.Textual(cursor.grab(mark, cursor.mark))):
        case '<' | '&'                        =>  HtmlNode.Textual(cursor.grab(mark, cursor.mark))
        case ' ' | '\f' | '\n' | '\r' | '\t'  =>  next() yet textual(mark)
        case char                             =>  if cursor.next() then textual(mark)
                                                  else HtmlNode.Textual(cursor.grab(mark, cursor.mark))

    def rawText(tag: Text, mark: Mark)(using Cursor.Held): HtmlNode =
      cursor.lay(HtmlNode.Textual(cursor.grab(mark, cursor.mark))):
        case '<'  =>  val end = cursor.mark
                      cursor.next()
                      val resume = cursor.mark

                      if cursor.lay(false)(_ == '/') then
                        next()
                        val tagStart = cursor.mark
                        repeat(tag.length)(next())
                        val candidate = cursor.grab(tagStart, cursor.mark)
                        if cursor.more && candidate == tag then
                          if cursor.lay(false)(_ == '>')
                          then HtmlNode.Textual(cursor.grab(mark, end))
                          else cursor.cue(resume) yet rawText(tag, mark)
                        else rawText(tag, mark)
                      else rawText(tag, mark)

        case char =>  if cursor.next() then rawText(tag, mark)
                      else HtmlNode.Textual(cursor.grab(mark, cursor.mark))

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
                      val content = cursor.hold(comment(cursor.mark))
                      cursor.next() yet Token.Comment(content)
        case '/'  =>  next()
                      val name = cursor.hold(tagname(cursor.mark))
                      Token.Close(name)

        case char =>  val name = cursor.hold(tagname(cursor.mark))
                      whitespace()

                      cursor.lay(fail(ExpectedMore)):
                        case '/' =>  expect('>') yet cursor.next() yet Token.Empty(name, Nil)
                        case '>' =>  cursor.next() yet Token.Open(name, Nil)
                        case _   =>  Token.Open(name, cursor.hold(attributes()))

    def descend(parent: Tag, children: List[HtmlNode]): HtmlNode = read(parent, children)

    @tailrec
    def read(parent: Tag, children: List[HtmlNode]): HtmlNode =
      cursor.lay(children.head):
        case '&'  => parent.content match
          case HtmlNode.Content.Whitespace => fail(OnlyWhitespace('&'))
          case _ =>
            val child = cursor.hold:
              val start = cursor.mark
              next()
              entity(cursor.mark).or(textual(start))

            read(parent, child :: children)

        case '<'  =>
          var ascend: Boolean = false
          val node: HtmlNode = cursor.hold:
            val mark = cursor.mark
            next() yet tag() match
              case Token.Empty(name, attributes) => HtmlNode.Node(name, attributes, Nil)
              case Token.Comment(text)           => HtmlNode.Comment(text)

              case token@Token.Open(name, attributes) =>
                val tag = elements(name).or:
                  cursor.cue(mark)
                  fail(UnknownTag(name))
                if tag.void then HtmlNode.Node(name, attributes, Nil) else descend(tag, Nil)

              case Token.Close(name) =>
                if name != parent.name then
                  if parent.autoclose then
                    cursor.cue(mark)
                    ascend = true
                    HtmlNode.Node(name, parent.attributes, children.reverse)
                  else fail(MismatchedTag(parent.name, name))
                else
                  cursor.next()
                  ascend = true
                  HtmlNode.Node(name, parent.attributes, children.reverse)



          if ascend then node else read(parent, node :: children)

        case char => parent.content match
          case HtmlNode.Content.Whitespace =>
            onlyWhitespace() yet read(parent, children)

          case HtmlNode.Content.Raw =>
            cursor.hold:
              HtmlNode.Node(parent.name, parent.attributes, List(rawText(parent.name, cursor.mark)))

          case HtmlNode.Content.Rcdata =>
            cursor.hold:
              HtmlNode.Node(parent.name, parent.attributes, List(rawText(parent.name, cursor.mark)))

          case HtmlNode.Content.Normal =>
            val child = cursor.hold(textual(cursor.mark))
            if child != HtmlNode.Textual("") then read(parent, child :: children)
            else read(parent, children)

    whitespace()
    read(Div, Nil)
