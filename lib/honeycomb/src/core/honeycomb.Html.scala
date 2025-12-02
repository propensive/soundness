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
import java.util as ju

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
import spectacular.*
import symbolism.*
import turbulence.*
import typonym.*
import vacuous.*
import zephyrine.*

import classloaders.threadContext
import charDecoders.utf8
import textSanitizers.skip

object Html extends Tag.Container
         (name       = "html",
          autoclose  = true,
          admissible = Set("head", "body"),
          content    = Html.TextContent.Whitespace,
          insertable = true), Format:
  type Topic = "html"
  type Transport = "head" | "body"

  given aggregable: [content <: Label: Reifiable to List[String]] => (dom: Dom)
        =>  Tactic[ParseError]
        =>  (Html of content) is Aggregable by Text =

    input =>
      val root = Tag.root(content.reification().map(_.tt).to(Set))
      parse(input.iterator, root).asInstanceOf[Html of content]

  inline given aggregable2: (dom: Dom) => Tactic[ParseError] => Html is Aggregable by Text =
    input => parse(input.iterator, dom.generic)

  given showable: Html is Showable =
    case Fragment(nodes*) => nodes.map(_.show).join
    case Textual(text)    => text
    case Comment(text) => t"<!--$text-->"

    case Foreign(tag, attributes, children) =>
      val tagContent = if attributes == Nil then t"" else
        attributes.map:
          case Attribute(key, value) => value.lay(key) { value => t"""$key="$value"""" }
        . join(t" ", t" ", t"")

      t"<$tagname$tagContent>${children.map(_.toString.tt).join}</$tagname>"

    case Node(tag, attributes, children) =>
      val tagContent = if attributes == Nil then t"" else
        attributes.map:
          case Attribute(key, value) => value.lay(key) { value => t"""$key="$value"""" }
        . join(t" ", t" ", t"")

      t"<$tagname$tagContent>${children.map(_.toString.tt).join}</$tagname>"


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

  given conversion4: [label <: Label, content >: label]
        =>  Conversion[Html.Foreign, Html of content] =
    _.asInstanceOf[Html.Foreign of content]

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

  case class Fragment(nodes: Html*) extends Html:
    override def hashCode: Int = nodes.hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(nodes0*) => nodes0 == nodes
      case node: Html        => nodes.length == 1 && nodes(0) == node
      case _                 => false

  case class Foreign(tagname: Text, attributes: List[Attribute], children: IArray[Html]) extends Html:
    override def hashCode: Int = content.hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(foreign: Foreign) => foreign == this

      case Foreign(tagname0, attributes0, children0) =>
        tagname0 == tagname && attributes0 == attributes
        && ju.Arrays.equals(children0.mutable(using Unsafe), children.mutable(using Unsafe))

      case _ =>
        false


  case class Comment(text: Text) extends Html:
    override def hashCode: Int = List(this).hashCode

    override def equals(that: Any): Boolean = that match
      case Comment(text0)           => text0 == text
      case Fragment(Comment(text0)) => text0 == text
      case _                        => false

  case class Textual(text: Text) extends Html:
    type Topic = "#text"

    override def hashCode: Int = List(this).hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(textual: Textual) => this == textual
      case Textual(text0)             => text0 == text
      case _                          => false

  case class Node(tagname: Text, attributes: List[Attribute], children: IArray[Html])
  extends Html, Topical, Transportive:
    type Foo = this.Transport

    override def equals(that: Any): Boolean = that match
      case Fragment(node: Node) => this == node

      case Node(tagname0, attributes0, children0) =>
        tagname0 == tagname && attributes0 == attributes
        && ju.Arrays.equals(children0.mutable(using Unsafe), children.mutable(using Unsafe))

      case _ =>
        false

    override def hashCode: Int =
      ju.Arrays.hashCode(children.mutable(using Unsafe)) ^ attributes.hashCode ^ tagname.hashCode


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
    def attributes(entries: List[Attribute] = Nil)(using Cursor.Held): List[Attribute] =
      skip()
      cursor.lay(fail(ExpectedMore)):
        case '>' | '/' => entries
        case _         =>
          val name = key(cursor.mark)

          if equality() then
            val assignment = cursor.lay(fail(ExpectedMore)):
              case '"'  =>  next() yet value(cursor.mark)
              case '\'' =>  next() yet singleQuoted(cursor.mark)
              case _    =>  unquoted(cursor.mark) // FIXME: Only alphanumeric characters

            attributes(Attribute(name, assignment) :: entries)
          else attributes(Attribute(name, Unset) :: entries)

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
          content = cursor.hold(tagname(cursor.mark))
          extra = cursor.hold(attributes())

          cursor.lay(fail(ExpectedMore)):
            case '/'  =>  expect('>')
                          cursor.next()
                          Token.Empty
            case '>'  =>  cursor.next()
                          Token.Open
            case char =>  fail(Unexpected(char))

    def descend(parent: Tag): Html = read(parent, Nil, 0)

    inline def append(parent: Tag, text: Text, children: List[Html], count0: Int): Html =
      var count = count0

      val children2 =
        if text == "" then children else children match
          case Html.Textual(text0) :: more => Html.Textual(text0+text) :: more
          case _ =>
            count += 1
            Html.Textual(text) :: children

      read(parent, children2, count)

    def finish(parent: Tag, children: List[Html], count: Int): Html =
      if parent != root then
        if parent.autoclose
        then Html.Node(parent.tagname, parent.attributes, array(children, count))
        else fail(Incomplete(parent.tagname))
      else if count > 1 then Html.Fragment(children.reverse*) else children(0)

    def array(list: List[Html], count: Int): IArray[Html] =
      var todo = list
      var i = count
      val array = new Array[Html](count)

      while i > 0 do
        i -= 1
        array(i) = todo.head
        todo = todo.tail

      array.immutable(using Unsafe)

    @tailrec
    def read(parent: Tag, children: List[Html], count: Int): Html =
      cursor.lay(finish(parent, children, count)):
        case '&'  => parent.content match
          case Html.TextContent.Whitespace => fail(OnlyWhitespace('&'))
          case _ =>
            val child = cursor.hold:
              val start = cursor.mark
              next()
              entity(cursor.mark).or(textual(start))

            append(parent, child, children, count)

        case '<'  =>
          var level: Int = 0
          var ascend: Boolean = false
          val node: Html = cursor.hold:
            val mark = cursor.mark

            def node(): Html =
              if parent.foreign
              then Html.Foreign(content, extra, array(children, count))
              else Html.Node(content, extra, array(children, count))

            def close(): Html =
              if parent.foreign
              then Html.Foreign(parent.tagname, parent.attributes, array(children, count))
              else Html.Node(parent.tagname, parent.attributes, array(children, count))

            def infer(tag: Tag) =
              cursor.cue(mark)
              dom.infer(parent, tag).let(descend(_)).or:
                if parent.autoclose then close().also { ascend = true }
                else fail(InadmissibleTag(content, parent.tagname))

            next()

            tag() match
              case Token.Comment => Html.Comment(content)

              case Token.Empty   =>
                val tag =
                  if parent.foreign then Tag.foreign(content, extra)
                  else dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))

                if parent.foreign || parent.admissible(content) then node() else infer(tag)


              case Token.Open =>
                val tag =
                  if parent.foreign then Tag.foreign(content, extra)
                  else dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))

                if tag.void then if parent.admissible(content) then node() else infer(tag)
                else if parent.foreign || parent.admissible(content) then descend(tag) else infer(tag)

              case Token.Close =>
                if content != parent.tagname then
                  cursor.cue(mark)
                  if parent.autoclose then close().also { ascend = true }
                  else fail(MismatchedTag(parent.tagname, content))
                else
                  cursor.next()
                  ascend = true
                  if parent.foreign
                  then Html.Foreign(content, parent.attributes, array(children, count))
                  else Html.Node(content, parent.attributes, array(children, count))

          if ascend then node else read(parent, node :: children, count + 1)

        case char => parent.content match
          case Html.TextContent.Whitespace =>
            whitespace() yet read(parent, children, count)

          case Html.TextContent.Raw =>
            val content = cursor.hold(raw(parent.tagname, cursor.mark))
            Html.Node(parent.tagname, parent.attributes, IArray(Html.Textual(content)))

          case Html.TextContent.Rcdata => // FIXME
            val content = cursor.hold(raw(parent.tagname, cursor.mark))
            Html.Node(parent.tagname, parent.attributes, IArray(Html.Textual(content)))

          case Html.TextContent.Normal =>
            append(parent, cursor.hold(textual(cursor.mark)), children, count)

    skip()
    read(root, Nil, 0)

sealed into trait Html extends Topical:
  type Topic <: Label
