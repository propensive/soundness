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

object HtmlNode:
  type Attributes = ListMap[Text, Optional[Text]]

  given conversion: [label <: "p" | "li"] => Conversion[Text, HtmlNode of label] =
    HtmlNode.Textual(_).asInstanceOf[HtmlNode.Textual { type Topic = label }]

  given conversion2: [label <: "p" | "li"] => Conversion[String, HtmlNode of label] =
    string => HtmlNode.Textual(string.tt).asInstanceOf[HtmlNode.Textual { type Topic = label }]

enum HtmlNode:
  type Topic
  case Document(nodes: HtmlNode*)
  case Textual(text: Text)
  case Node(name: Text, attributes: HtmlNode.Attributes, children: Seq[HtmlNode])

object Dom:

  object Tag
  trait Taglike(val name: Text)

  class Tag[label <: Label](name: Text) extends Taglike(name), Dynamic:
    def apply(children: into[HtmlNode of label]*): HtmlNode.Node =
      HtmlNode.Node(name, ListMap(), children)


  val entitiesList = cp"/honeycomb/entities.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
    case List(key, value) => (key, value)

  val P = Tag["p"]("p")

  val elements: Set[Text] =
    Set
     ("a", "abbr", "address", "area", "article", "aside", "audio", "b", "base", "bdi", "bdo",
      "blockquote", "body", "br", "button", "canvas", "caption", "cite", "code", "col", "colgroup",
      "data", "datalist", "dd", "del", "details", "dfn", "dialog", "div", "dl", "dt", "em", "embed",
      "fencedframe", "fieldset", "figcaption", "figure", "footer", "form", "h1", "head", "header",
      "hgroup", "hr", "html", "i", "iframe", "img", "input", "ins", "kbd", "label", "legend", "li",
      "link", "main", "map", "mark", "menu", "meta", "meter", "nav", "noscript", "object", "ol",
      "optgroup", "option", "output", "p", "picture", "pre", "progress", "q", "rp", "rt", "ruby",
      "s", "samp", "script", "search", "section", "select", "selectedcontent", "slot", "small",
      "source", "span", "strong", "style", "sub", "summary", "sup", "table", "tbody", "td",
      "template", "textarea", "tfoot", "th", "thead", "time", "title", "tr", "track", "u", "ul",
      "var", "video", "wbr")

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
    case SelfClosing(name: Text, attributes: ListMap[Text, Optional[Text]])
    case Open(name: Text, attributes: ListMap[Text, Optional[Text]])

  def parse(input: Iterator[Text]): HtmlNode =
    val cursor = Cursor(input)
    inline def next(): Unit = cursor.next() || panic(m"expected more content at ${cursor.position.n0}")

    inline def expect(char: Char): Unit =
      cursor.next()
      cursor.lay(fail()): datum =>
        if datum != char
        then panic(m"expected $char but found $datum at ${cursor.position.n0}")

    inline def fail(): Nothing = panic(m"Expected more content")

    @tailrec
    def whitespace(): Unit = cursor.lay(fail()):
      case ' ' | '\f' | '\n' | '\r' | '\t' => next() yet whitespace()
      case _                               => ()

    @tailrec
    def tagname(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail()):
      case char if char.isLetter           =>  next() yet tagname(mark)
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case '/'                             =>  cursor.grab(mark, cursor.mark)
      case '>'                             =>  cursor.grab(mark, cursor.mark)
      case char                            =>  panic(m"Did not expect $char at ${cursor.position.n0}")

    @tailrec
    def key(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail()):
      case '-'                             =>  next() yet key(mark)
      case char if char.isLetter           =>  next() yet key(mark)
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case '='                             =>  cursor.grab(mark, cursor.mark)
      case char                            =>  panic(m"Did not expect $char at ${cursor.position.n0}")


    @tailrec
    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail()):
      case '"'  => cursor.grab(mark, cursor.mark).also(next())
      case char => next() yet value(mark)

    @tailrec
    def unquotedValue(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail()):
      case '>' | ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case '"' | '\'' | '<' | '=' | '`'          =>  panic(m"forbidden character in unquoted value at ${cursor.position.n0}")
      case char                                  =>  next() yet unquotedValue(mark)

    def equality(): Unit =
      whitespace()
      cursor.lay(fail()):
        case '='  => next() yet whitespace()
        case char => panic(m"expected = at ${cursor.position.n0}")


    @tailrec
    def attributes(entries0: HtmlNode.Attributes = ListMap())(using Cursor.Held)
    : HtmlNode.Attributes =

        val name = key(cursor.mark)
        equality()

        val assignment = cursor.lay(fail()):
          case '"' =>  next() yet value(cursor.mark)
          case _   =>  unquotedValue(cursor.mark)

        val entries = entries0.updated(name, assignment)

        whitespace()

        cursor.lay(fail()):
          case '>' =>  next() yet entries
          case '/' =>  expect('>') yet next() yet entries
          case _   =>  attributes(entries)

    def entity(mark: Mark)(using Cursor.Held): Optional[HtmlNode.Textual] = cursor.lay(fail()):
      case char if char.isLetter | char.isDigit =>  next() yet entity(mark)
      case '='                                  => Unset

      case ';' =>
        next()
        HtmlNode.Textual(entities(cursor.grab(mark, cursor.mark)).or(panic(m"Unknown entity")))

      case char =>
        entities(cursor.grab(mark, cursor.mark)).let(HtmlNode.Textual(_))


    def text(mark: Mark, whitespace: Boolean = true)(using Cursor.Held): Text =
      cursor.lay(cursor.grab(mark, cursor.mark)):
        case '<' | '&'                        =>  cursor.grab(mark, cursor.mark)
        case ' ' | '\f' | '\n' | '\r' | '\t'  =>  next() yet text(mark, whitespace)
        case char                             =>  if cursor.next() then text(mark, false)
                                                  else cursor.grab(mark, cursor.mark)

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

    def tag(): Token =
      cursor.lay(fail()):
        case '/'  =>  next()
                      val name = cursor.hold(tagname(cursor.mark))
                      Token.Close(name)

        case char =>  val name = cursor.hold(tagname(cursor.mark))
                      whitespace()

                      cursor.lay(fail()):
                        case '/' =>  expect('>') yet next() yet Token.SelfClosing(name, ListMap())
                        case '>' =>  next() yet Token.Open(name, ListMap())
                        case _   =>  Token.Open(name, cursor.hold(attributes()))

    def read(parent: Token.Open, children: List[HtmlNode]): HtmlNode =
      cursor.lay(HtmlNode.Document(children*)):
        case '&'  =>  cursor.hold:
                        val start = cursor.mark
                        next()
                        read(parent, entity(cursor.mark).or(text(start)) :: children)

        case '<'  =>  next() yet tag() match
          case tag@Token.Open(name, attributes) =>
            if Dom.void(name)
            then read(parent, HtmlNode.Node(name, attributes, Nil) :: children)
            else read(parent, read(tag, Nil) :: children)

          case Token.Close(name) =>
            HtmlNode.Node(name, parent.attributes, children.reverse)

          case Token.SelfClosing(name, attributes) =>
            read(parent, HtmlNode.Node(name, attributes, Nil) :: children)

        case char => cursor.hold:
          val mark = cursor.mark
          if rawElements(parent.name)
          then HtmlNode.Node(parent.name, parent.attributes, List(rawText(parent.name, mark)))
          else if rcdataElements(parent.name)
          then HtmlNode.Node(parent.name, parent.attributes, List(rawText(parent.name, mark))) // FIXME
          else read(parent, HtmlNode.Textual(text(mark)) :: children)

    whitespace()
    read(Token.Open("", ListMap()), Nil)
