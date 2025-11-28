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
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case ForbiddenUnquoted(char: Char)

    def describe: Message = this match
      case ExpectedMore             =>  m"the content ended prematurely"
      case OnlyWhitespace(char)     =>  m"the character $char was found where only whitespace is permitted"
      case Unexpected(char)         =>  m"the character $char was not expected"
      case ForbiddenUnquoted(char)  =>  m"the character $char is forbidden in an unquoted attribute"


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

sealed trait HtmlNode extends Topical:
  type Topic

erased trait Childless extends Transportive

object Pair:
  given conversion: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, Text), Pair of tag] =
    pair => Pair(pair(0), pair(1)).asInstanceOf[Pair of tag]

  given conversion2: [key <: String: Precise, tag <: String: Precise]
        => Conversion[(key, String), Pair of tag] =
    pair => Pair(pair(0), pair(1).tt).asInstanceOf[Pair of tag]

case class Pair(key: Text, value: Text) extends Topical

extension (node: HtmlNode.Node & Childless)
  def apply(children: into[HtmlNode of node.Transport]*): HtmlNode.Node =
    HtmlNode.Node(node.name, node.attributes, children)

object Dom:
  import HtmlNode.Issue.*

  object Tag

  class Tag[label <: Label, children <: Label](name0: Text, void: Boolean = false)
  extends HtmlNode.Node(name0, Nil, Nil), Dynamic:
    type Topic = label
    type Transport = children

    def applyDynamic(method: "apply")(children: into[HtmlNode of children]*)
    : HtmlNode.Node of label =

        HtmlNode.Node(name, Nil, children).asInstanceOf[HtmlNode.Node of label]


    def applyDynamicNamed(method: "apply")(attributes: into[Pair of label]*)
    : HtmlNode.Node & Childless over children =

        HtmlNode.Node(name, attributes.to(List), Nil)
        . asInstanceOf[HtmlNode.Node & Childless over children]


  val entitiesList = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
    case List(key, value) => (key, value)

  object Area extends Tag["area", Label]("area", void = true)
  object Base extends Tag["base", Label]("base", void = true)
  object Br extends Tag["br", Label]("br", void = true)
  object Col extends Tag["col", Label]("col", void = true)
  object Command extends Tag["command", Label]("command", void = true)
  object Embed extends Tag["embed", Label]("embed", void = true)
  object Hr extends Tag["hr", Label]("hr", void = true)
  object Img extends Tag["img", Label]("img", void = true)
  object Input extends Tag["input", Label]("input", void = true)
  object Link extends Tag["link", Label]("link", void = true)
  object Meta extends Tag["meta", Label]("meta", void = true)
  object Param extends Tag["param", Label]("param", void = true)
  object Source extends Tag["source", Label]("source", void = true)
  object Track extends Tag["track", Label]("track", void = true)
  object Wbr extends Tag["wbr", Label]("wbr", void = true)

  object Head extends Tag["head", "meta" | "style"]("head")
  object Body extends Tag["body", "div"]("body")
  object Div extends Tag["div", "p" | "ul" | "ol" | "#text"]("div")
  object Li extends Tag["li", "p" | "#text"]("li")
  object Ol extends Tag["ol", "li"]("ol")
  object P extends Tag["p", "i" | "em" | "strong" | "#text"]("p")
  object Ul extends Tag["ul", "li"]("ul")

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

  val void: Set[Text] =
    Set("area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link",
        "meta", "param", "source", "track", "wbr")

  val rawElements: Set[Text] = Set("script", "style")

  val rcdataElements: Set[Text] = Set("textarea", "title")

  val textless: Set[Text] =
    Set("html", "head", "table", "colgroup", "thead", "tbody", "tfoot", "tr", "ul", "ol", "menu",
        "dl", "select", "optgroup", "datalist", "picture", "template")

  val optionalEndTag: Set[Text] =
    Set("html", "head", "body", "p", "li", "dt", "dd", "rt", "rp", "optgroup", "option", "colgroup",
        "thead", "tbody", "tfoot", "tr", "td", "th")

  val entities: Dictionary[Text] = Dictionary(entitiesList*)

  enum Token:
    case Close(name: Text)
    case Comment(text: Text)
    case SelfClosing(name: Text, attributes: List[Pair])
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
      case '='                             =>  cursor.grab(mark, cursor.mark)
      case char                            =>  panic(m"Did not expect $char at ${cursor.position.n0}")


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

    def equality(): Unit =
      whitespace()

      cursor.lay(fail(ExpectedMore)):
        case '='  => next() yet whitespace()
        case char => panic(m"expected = at ${cursor.position.n0}")


    @tailrec
    def attributes(entries0: List[Pair] = Nil)(using Cursor.Held): List[Pair] =
      val name = key(cursor.mark)
      equality()

      val assignment = cursor.lay(fail(ExpectedMore)):
        case '"'  =>  next() yet value(cursor.mark)
        case '\'' =>  next() yet singleQuotedValue(cursor.mark)
        case _    =>  unquotedValue(cursor.mark)

      val entries = Pair(name, assignment) :: entries0

      whitespace()

      cursor.lay(fail(ExpectedMore)):
        case '>' =>  cursor.next() yet entries
        case '/' =>  expect('>') yet cursor.next() yet entries
        case _   =>  attributes(entries)

    def entity(mark: Mark)(using Cursor.Held): Optional[HtmlNode.Textual] = cursor.lay(fail(ExpectedMore)):
      case char if char.isLetter | char.isDigit =>  next() yet entity(mark)
      case '='                                  => Unset

      case ';' =>
        next()
        HtmlNode.Textual(entities(cursor.grab(mark, cursor.mark)).or(panic(m"Unknown entity")))

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
                      Token.Comment(cursor.hold(comment(cursor.mark)))
        case '/'  =>  next()
                      val name = cursor.hold(tagname(cursor.mark))
                      Token.Close(name)

        case char =>  val name = cursor.hold(tagname(cursor.mark))
                      whitespace()

                      cursor.lay(fail(ExpectedMore)):
                        case '/' =>  expect('>') yet Token.SelfClosing(name, Nil)
                        case '>' =>  cursor.next() yet Token.Open(name, Nil)
                        case _   =>  Token.Open(name, cursor.hold(attributes()))

    def descend(parent: Token.Open, children: List[HtmlNode]): HtmlNode = read(parent, children)

    @tailrec
    def read(parent: Token.Open, children: List[HtmlNode]): HtmlNode =
      cursor.lay(children.head):
        case '&'  =>
          if textless(parent.name) then fail(OnlyWhitespace('&')) else
            val child = cursor.hold:
              val start = cursor.mark
              next()
              entity(cursor.mark).or(textual(start))

            read(parent, child :: children)

        case '<'  =>  next() yet tag() match
          case tag@Token.Open(name, attributes) =>
            if Dom.void(name)
            then read(parent, HtmlNode.Node(name, attributes, Nil) :: children)
            else read(parent, descend(tag, Nil) :: children)

          case Token.Close(name) =>
            cursor.next()
            HtmlNode.Node(name, parent.attributes, children.reverse)

          case Token.Comment(text) =>
            read(parent, HtmlNode.Comment(text) :: children)

          case Token.SelfClosing(name, attributes) =>
            read(parent, HtmlNode.Node(name, attributes, Nil) :: children)

        case char =>
          if textless(parent.name) then onlyWhitespace() yet read(parent, children) else
            if rawElements(parent.name)
            then cursor.hold:
              HtmlNode.Node(parent.name, parent.attributes, List(rawText(parent.name, cursor.mark)))
            else if rcdataElements(parent.name)
            then cursor.hold:
              HtmlNode.Node(parent.name, parent.attributes, List(rawText(parent.name, cursor.mark))) // FIXME
            else
              val child = cursor.hold(textual(cursor.mark))
              if child != HtmlNode.Textual("") then read(parent, child :: children)
              else read(parent, children)

    whitespace()
    read(Token.Open("", Nil), Nil)
