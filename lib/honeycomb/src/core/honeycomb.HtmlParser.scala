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
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

import classloaders.threadContext
import charDecoders.utf8
import textSanitizers.skip

object HtmlNode extends Format:
  def name: Text = t"HTML"

  given conversion: [label >: "#text" <: Label] => Conversion[Text, HtmlNode of label] =
    HtmlNode.Textual(_).asInstanceOf[HtmlNode.Textual of label]

  given conversion2: [label >: "#text" <: Label] => Conversion[String, HtmlNode of label] =
    string => HtmlNode.Textual(string.tt).asInstanceOf[HtmlNode.Textual of label]

  given conversion3: [label <: Label, content >: label]
        =>  Conversion[HtmlNode.Node over content, HtmlNode of label] =
    _.asInstanceOf[HtmlNode.Node { type Topic = label }]

  enum Issue extends Format.Issue:
    case ExpectedMore
    case InvalidTag(name: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)

    def describe: Message = this match
      case ExpectedMore               =>  m"the content ended prematurely"
      case InvalidTag(name)           =>  m"the tag <$name> is not valid HTML5"
      case OnlyWhitespace(char)       =>  m"the character $char was found where only whitespace is permitted"
      case Unexpected(char)           =>  m"the character $char was not expected"
      case UnknownEntity(name)        =>  m"the entity &$name is not defined"
      case ForbiddenUnquoted(char)    =>  m"the character $char is forbidden in an unquoted attribute"
      case MismatchedTag(open, close) =>  m"the tag </$close> did not match the opening tag <$open>"

  case class Position(ordinal: Ordinal) extends Format.Position:
    def describe: Text = t"character ${ordinal.n1}"

  case class Document(nodes: HtmlNode*) extends HtmlNode

  case class Fragment(nodes: HtmlNode*) extends HtmlNode:
    override def hashCode: Int = nodes.hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(nodes0*) => nodes0 == nodes
      case node: HtmlNode    => nodes.length == 1 && nodes(0) == node
      case _                 => false

  case class Comment(text: Text) extends HtmlNode:
    override def hashCode: Int = List(this).hashCode

    override def equals(that: Any): Boolean = that match
      case Comment(text0)           => text0 == text
      case Fragment(Comment(text0)) => text0 == text
      case _                        => false

    override def toString(): String = t"<!--$text-->".s

  case class Textual(text: Text) extends HtmlNode:
    type Topic = "#text"
    override def toString(): String = text.s

    override def hashCode: Int = List(this).hashCode

    override def equals(that: Any): Boolean = that match
      case Textual(text0)           => text0 == text
      case Fragment(Textual(text0)) => text0 == text
      case _                        => false

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
  val Script = Tag["script", "#text"](content = HtmlNode.Content.Raw)
  val Style = Tag["style", Label](content = HtmlNode.Content.Raw)
  val Track = Tag["track", Label](void = true)
  val Wbr = Tag["wbr", Label](void = true)

  val Head = Tag["head", "script"](autoclose = true)
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

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)

  enum Token:
    case Close, Comment, Empty, Open

  def parse(input: Iterator[Text]): HtmlNode raises ParseError =
    val cursor = Cursor(input)

    def next(): Unit =
      if !cursor.next()
      then raise(ParseError(HtmlNode, HtmlNode.Position(cursor.position), ExpectedMore))

    inline def expect(char: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum != char then fail(Unexpected(datum))

    def fail(issue: HtmlNode.Issue): Nothing =
      abort(ParseError(HtmlNode, HtmlNode.Position(cursor.position), issue))

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
    def attributes(entries0: List[Pair] = Nil)(using Cursor.Held): List[Pair] =
      val name = key(cursor.mark)

      val entries = if equality() then
        val assignment = cursor.lay(fail(ExpectedMore)):
          case '"'  =>  next() yet value(cursor.mark)
          case '\'' =>  next() yet singleQuoted(cursor.mark)
          case _    =>  unquoted(cursor.mark)

        Pair(name, assignment) :: entries0
      else Pair(name, name) :: entries0

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
          entities(name).or(fail(UnknownEntity(name)))

        case char =>
          entities(cursor.grab(mark, cursor.mark))


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
                        println(s"candidate: $candidate")
                        if cursor.more && candidate == tag then
                          if cursor.lay(false)(_ == '>')
                          then cursor.grab(mark, end).also(cursor.next())
                          else cursor.cue(resume) yet raw(tag, mark)
                        else cursor.cue(resume) yet raw(tag, mark)
                      else cursor.cue(resume) yet raw(tag, mark)

        case char =>  if cursor.next() then raw(tag, mark) else cursor.grab(mark, cursor.mark)

    // mutable state
    var content: Text = t""
    var tokenAttributes: List[Pair] = Nil

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

        case char =>  val name = cursor.hold(tagname(cursor.mark))
                      skip()

                      cursor.lay(fail(ExpectedMore)):
                        case '/' =>  expect('>')
                                     cursor.next()
                                     content = name
                                     tokenAttributes = Nil
                                     Token.Empty
                        case '>' =>  cursor.next()
                                     content = name
                                     tokenAttributes = Nil
                                     Token.Open
                        case _   =>  content = name
                                     tokenAttributes = cursor.hold(attributes())
                                     Token.Open

    def descend(parent: Tag, children: List[HtmlNode]): HtmlNode = read(parent, children)

    def append(text: Text, children: List[HtmlNode]): List[HtmlNode] =
      if text == "" then children else children match
        case HtmlNode.Textual(text0) :: more => HtmlNode.Textual(text0+text) :: more
        case _                              => HtmlNode.Textual(text) :: children

    def fragment(children: List[HtmlNode]): HtmlNode =
      if children.length > 1 then HtmlNode.Fragment(children.reverse*) else children(0)

    @tailrec
    def read(parent: Tag, children: List[HtmlNode]): HtmlNode =
      cursor.lay(fragment(children)):
        case '&'  => parent.content match
          case HtmlNode.Content.Whitespace => fail(OnlyWhitespace('&'))
          case _ =>
            val child = cursor.hold:
              val start = cursor.mark
              next()
              entity(cursor.mark).or(textual(start))

            read(parent, append(child, children))

        case '<'  =>
          var ascend: Boolean = false
          val node: HtmlNode = cursor.hold:
            val mark = cursor.mark

            next()
            tag() match
              case Token.Empty   =>  HtmlNode.Node(content, tokenAttributes, Nil)
              case Token.Comment =>  HtmlNode.Comment(content)

              case Token.Open =>
                val tag = elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))
                if tag.void then HtmlNode.Node(content, tokenAttributes, Nil)
                else descend(tag, Nil)

              case Token.Close =>
                if content != parent.name then
                  if parent.autoclose then
                    cursor.cue(mark)
                    ascend = true
                    HtmlNode.Node(content, parent.attributes, children.reverse)
                  else fail(MismatchedTag(parent.name, content))
                else
                  cursor.next()
                  ascend = true
                  HtmlNode.Node(content, parent.attributes, children.reverse)

          if ascend then node else read(parent, node :: children)

        case char => parent.content match
          case HtmlNode.Content.Whitespace =>
            whitespace() yet read(parent, children)

          case HtmlNode.Content.Raw =>
            val content = cursor.hold(raw(parent.name, cursor.mark))
            HtmlNode.Node(parent.name, parent.attributes, List(HtmlNode.Textual(content)))

          case HtmlNode.Content.Rcdata => // FIXME
            val content = cursor.hold(raw(parent.name, cursor.mark))
            HtmlNode.Node(parent.name, parent.attributes, List(HtmlNode.Textual(content)))

          case HtmlNode.Content.Normal =>
            read(parent, append(cursor.hold(textual(cursor.mark)), children))

    skip()
    read(Div, Nil)
