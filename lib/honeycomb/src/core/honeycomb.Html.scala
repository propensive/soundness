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
import hypotenuse.*
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
import scala.annotation.tailrec

object Html extends Tag.Container
         (label       = "html",
          autoclose   = true,
          admissible  = Set("head", "body"),
          content     = Html.TextContent.Whitespace,
          insertable  = true,
          foreign     = false), Format:
  type Topic = "html"
  type Transport = "head" | "body"

  erased trait Integral
  erased trait Decimal
  erased trait Id

  given aggregable: [content <: Label: Reifiable to List[String]] => (dom: Dom)
        =>  Tactic[ParseError]
        =>  (Html of content) is Aggregable by Text =

    input =>
      val root = Tag.root(content.reification().map(_.tt).to(Set))
      parse(input.iterator, root).of[content]

  given aggregable2: (dom: Dom) => Tactic[ParseError] => Html is Aggregable by Text =
    input => parse(input.iterator, dom.generic)

  given showable: [html <: Html] => html is Showable =
    case Fragment(nodes*) => nodes.map(_.show).join
    case Textual(text)    => text
    case Comment(text) => t"<!--$text-->"

    case Node(tagname, attributes, children, _) =>
      val tagContent = if attributes.isEmpty then t"" else
        attributes.map:
          case (key, value) => value.lay(key) { value => t"""$key="$value"""" }
        . join(t" ", t" ", t"")

      t"<$tagname$tagContent>${children.map(_.show).join}</$tagname>"


  private enum Token:
    case Close, Comment, Empty, Open

  private enum Level:
    case Ascend, Descend, Peer

  trait Populable:
    node: Html.Node =>
      def apply(children: Html of node.Transport*): Html.Node of node.Topic =
        new Html.Node(node.label, node.attributes, IArray.from(children), node.foreign):
          type Topic = node.Topic

  trait Transparent:
    node: Html.Node =>
      def apply[labels <: Label](children: Html of labels | node.Transport*): Html.Node of labels =
        new Html.Node(node.label, node.attributes, IArray.from(children), node.foreign):
          type Topic = labels


  import Issue.*
  def name: Text = t"HTML"

  given conversion: [label >: "#text" <: Label] => Conversion[Text, Html of label] =
    Textual(_).of[label]

  given conversion2: [label >: "#text" <: Label] => Conversion[String, Html of label] =
    string => Textual(string.tt).of[label]

  given conversion3: [label <: Label, content >: label <: Label]
        =>  Conversion[Node of label, Html of content] =
    _.of[content]

  given conversion4: [content <: Label] =>  Conversion[Comment, Html of content] = _.of[content]

  given conversion5: Conversion[String, Html of "#foreign"] =
    string => Textual(string.tt).of["#foreign"]

  given conversion6: [content <: Label, value: Renderable in content]
        => Conversion[value, Html of content] =
    value.render(_)


  enum Issue extends Format.Issue:
    case BadInsertion
    case ExpectedMore
    case InvalidTag(name: Text)
    case InvalidTagStart(prefix: Text)
    case DuplicateAttribute(name: Text)
    case InadmissibleTag(name: Text, parent: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)
    case Incomplete(tag: Text)
    case UnknownAttribute(name: Text)
    case UnknownAttributeStart(name: Text)
    case InvalidAttributeUse(attribute: Text, element: Text)

    def describe: Message = this match
      case BadInsertion                   =>  m"a value cannot be inserted into HTML at this point"
      case ExpectedMore                   =>  m"the content ended prematurely"
      case InvalidTag(name)               =>  m"<$name> is not a valid tag"
      case InvalidTagStart(prefix)        =>  m"there is no valid tag whose name starts $prefix"
      case DuplicateAttribute(name)       =>  m"the attribute $name already exists on this tag"
      case InadmissibleTag(name, parent)  =>  m"<$name> cannot be a child of <$parent>"
      case OnlyWhitespace(char)           =>  m"the character $char was found where only whitespace is permitted"
      case Unexpected(char)               =>  m"the character $char was not expected"
      case UnknownEntity(name)            =>  m"the entity &$name is not defined"
      case ForbiddenUnquoted(char)        =>  m"the character $char is forbidden in an unquoted attribute"
      case MismatchedTag(open, close)     =>  m"the tag </$close> did not match the opening tag <$open>"
      case Incomplete(tag)                =>  m"the content ended while the tag <$tag> was left open"
      case UnknownAttribute(name)         =>  m"$name is not a recognized attribute"
      case UnknownAttributeStart(name)    =>  m"there is no valid attribute whose name starts $name"
      case InvalidAttributeUse(name, tag) =>  m"the attribute $name cannot be used on the tag <$tag>"

  case class Position(ordinal: Ordinal) extends Format.Position:
    def describe: Text = t"character ${ordinal.n1}"

  case class Fragment(nodes: Html*) extends Html:
    override def hashCode: Int = nodes.hashCode

    override def equals(that: Any): Boolean = that match
      case Fragment(nodes0*) => nodes0 == nodes
      case node: Html        => nodes.length == 1 && nodes(0) == node
      case _                 => false

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


  object Node:
    def foreign(label: Text, attributes: Map[Text, Optional[Text]], children: Html of "#foreign"*)
    : Node of "#foreign" =

        Node(label, attributes, IArray.from(children), true).of["#foreign"]


  case class Node
         (label:      Text,
          attributes: Map[Text, Optional[Text]],
          children:   IArray[Html],
          foreign:    Boolean)
  extends Html, Topical, Transportive:

    override def equals(that: Any): Boolean = that match
      case Fragment(node: Node) => this == node

      case Node(label, attributes, children, foreign) =>
        label == this.label && attributes == this.attributes && foreign == this.foreign
        && ju.Arrays.equals(children.mutable(using Unsafe), this.children.mutable(using Unsafe))

      case _ =>
        false

    override def hashCode: Int =
      ju.Arrays.hashCode(children.mutable(using Unsafe)) ^ attributes.hashCode ^ label.hashCode

  enum TextContent:
    case Raw, Rcdata, Whitespace, Normal

  enum Hole:
    case Text, Tagbody, Comment
    case Attribute(tag: Text, attribute: Text)
    case Node(parent: Text)

  def parse[dom <: Dom]
       (input:  Iterator[Text],
        root:   Tag,
        insert: Optional[(Ordinal, Hole) => Unit] = Unset)
       (using dom: Dom): Html raises ParseError =
    val cursor = Cursor(input)
    val buffer: jl.StringBuilder = jl.StringBuilder()
    def result(): Text = buffer.toString.tt.also(buffer.setLength(0))
    var content: Text = t""
    var extra: Map[Text, Optional[Text]] = ListMap()

    def next(): Unit =
      if !cursor.next() then raise(ParseError(Html, Position(cursor.position), ExpectedMore))

    inline def expect(char: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum != char then fail(Unexpected(datum))

    def fail(issue: Issue): Nothing =
      abort(ParseError(Html, Position(cursor.position), issue))

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
    def tagname(mark: Mark, dictionary: Dictionary[Tag])(using Cursor.Held): Tag =
      cursor.lay(fail(ExpectedMore)):
        case char if char.isLetter => dictionary(char.minuscule) match
          case Dictionary.Empty => val name = cursor.grab(mark, cursor.mark)
                                   cursor.cue(mark) yet fail(InvalidTagStart(name))
          case other            => next() yet tagname(mark, other)
        case ' ' | '\f' | '\n' | '\r' | '\t' | '/' | '>' => dictionary match
          case Dictionary.Just("", tag)       =>  tag
          case Dictionary.Branch(tag: Tag, _) =>  tag
          case other                          =>  val name = cursor.grab(mark, cursor.mark)
                                                  cursor.cue(mark) yet fail(InvalidTag(name))
        case '\u0000' =>
          fail(BadInsertion)

        case char =>
          fail(Unexpected(char))

    @tailrec
    def foreignTag(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case char if char.isLetter                       =>  next() yet foreignTag(mark)
      case ' ' | '\f' | '\n' | '\r' | '\t' | '/' | '>' =>  cursor.grab(mark, cursor.mark).lower
      case '\u0000'                                    =>  fail(BadInsertion)
      case char                                        =>  fail(Unexpected(char))

    @tailrec
    def key(mark: Mark, dictionary: Dictionary[Attribute])(using Cursor.Held): Attribute =
      cursor.lay(fail(ExpectedMore)):
        case char if char.isLetter || char == '-' => dictionary(char) match
          case Dictionary.Empty =>  fail(UnknownAttributeStart(cursor.grab(mark, cursor.mark)))
          case dictionary       =>  next() yet key(mark, dictionary)

        case ' ' | '\f' | '\n' | '\r' | '\t' | '=' | '>' =>
          dictionary.element.or:
            val name = cursor.grab(mark, cursor.mark)
            cursor.cue(mark)
            fail(UnknownAttribute(name))

        case char =>
          fail(Unexpected(char))

    @tailrec
    def foreignKey(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case char if char.isLetter || char == '-'        =>  next() yet foreignKey(mark)
      case ' ' | '\f' | '\n' | '\r' | '\t' | '=' | '>' =>  cursor.grab(mark, cursor.mark)
      case '\u0000'                                    =>  fail(BadInsertion)
      case char                                        =>  fail(Unexpected(char))


    @tailrec
    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '&'      =>  val start = cursor.mark
                        next()
                        val mark2 = entity(cursor.mark).lay(mark): text =>
                          cursor.clone(mark, start)(buffer)
                          buffer.append(text)
                          cursor.mark
                        value(mark2)
      case '"'      =>  cursor.clone(mark, cursor.mark)(buffer)
                        next() yet result()
      case '\u0000' =>  insert.let(_(cursor.position, Hole.Text)) yet next() yet value(mark)
      case char     =>  next() yet value(mark)

    @tailrec
    def singleQuoted(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '\'' => cursor.grab(mark, cursor.mark).also(next())
      case char => next() yet singleQuoted(mark)

    @tailrec
    def unquoted(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '>' | ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.grab(mark, cursor.mark)
      case char@('"' | '\'' | '<' | '=' | '`')   =>  fail(ForbiddenUnquoted(char))
      case '\u0000'                              =>  fail(BadInsertion)
      case char                                  =>  next() yet unquoted(mark)

    def equality(): Boolean = skip() yet cursor.lay(fail(ExpectedMore)):
      case '='                                   =>  next() yet skip() yet true
      case '>' | ' ' | '\f' | '\n' | '\r' | '\t' =>  false
      case '\u0000'                              =>  fail(BadInsertion)
      case char                                  =>  fail(Unexpected(char))


    @tailrec
    def attributes(tag: Text, foreign: Boolean, entries: Map[Text, Optional[Text]] = ListMap())
         (using Cursor.Held)
    : Map[Text, Optional[Text]] =

        skip() yet cursor.lay(fail(ExpectedMore)):
          case '>' | '/' => entries
          case '\u0000'  => insert.let(_(cursor.position, Hole.Tagbody))
                            next()
                            skip()
                            attributes(tag, foreign, entries.updated(t"\u0000", Unset))
          case _         =>
            val key2 = if foreign then foreignKey(cursor.mark) else
              key(cursor.mark, dom.attributes).tap: key =>
                if !key.targets(tag) then fail(InvalidAttributeUse(key.label, tag))
              . label

            if entries.has(key2) then fail(DuplicateAttribute(key2))

            val assignment = if !equality() then Unset else cursor.lay(fail(ExpectedMore)):
              case '\u0000' =>  insert.let(_(cursor.position, Hole.Attribute(tag, key2)))
                                next() yet t"\u0000"
              case '"'      =>  next() yet value(cursor.mark)
              case '\''     =>  next() yet singleQuoted(cursor.mark)
              case _        =>  unquoted(cursor.mark) // FIXME: Only alphanumeric characters

            attributes(tag, foreign, entries.updated(key2, assignment))


    def entity(mark: Mark)(using Cursor.Held): Optional[Text] = cursor.lay(fail(ExpectedMore)):
      case '#'   => next() yet numericEntity(mark)
      case other => textEntity(mark, dom.entities)

    def numericEntity(mark: Mark)(using Cursor.Held): Optional[Text] = cursor.lay(fail(ExpectedMore)):
      case 'x' => next() yet hexEntity(mark, 0)
      case _   => decimalEntity(mark, 0)

    @tailrec
    def hexEntity(mark: Mark, value: Int)(using Cursor.Held): Optional[Text] =
      cursor.lay(fail(ExpectedMore)):
        case digit if digit.isDigit         => cursor.next() yet hexEntity(mark, 16*value + (digit - '0'))
        case letter if 'a' <= letter <= 'f' => cursor.next() yet hexEntity(mark, 16*value + (letter - 87))
        case letter if 'A' <= letter <= 'F' => cursor.next() yet hexEntity(mark, 16*value + (letter - 55))
        case ';'                            => cursor.next() yet value.unicode
        case char                           => Unset

    @tailrec
    def decimalEntity(mark: Mark, value: Int): Optional[Text] = cursor.lay(fail(ExpectedMore)):
      case digit if digit.isDigit => next() yet decimalEntity(mark, 10*value + (digit - '0'))
      case ';'                    => next() yet t"${value.toChar}"
      case char                   => Unset

    @tailrec
    def textEntity(mark: Mark, dictionary: Dictionary[Text])(using Cursor.Held): Optional[Text] =
      cursor.lay(fail(ExpectedMore)):
        case char if char.isLetter | char.isDigit =>  dictionary(char) match
          case Dictionary.Empty                   =>  Unset
          case dictionary                         =>  cursor.next() yet textEntity(mark, dictionary)
        case ';'                                  =>  cursor.next() yet dictionary(';').element
        case '='                                  =>  Unset
        case '\u0000'                             =>  fail(BadInsertion)
        case char                                 =>  dictionary.element


    @tailrec
    def textual(mark: Mark, close: Optional[Text], entities: Boolean)(using Cursor.Held): Text =
      cursor.lay(cursor.clone(mark, cursor.mark)(buffer) yet result()):
        case '<' | '\u0000' =>
          close.lay(cursor.clone(mark, cursor.mark)(buffer) yet result()): tag =>
            val end = cursor.mark
            cursor.next()
            val resume = cursor.mark

            if cursor.lay(false)(_ == '/') then
              next()
              val tagStart = cursor.mark
              repeat(tag.length)(cursor.next())
              val candidate = cursor.grab(tagStart, cursor.mark)
              if cursor.more && candidate == tag then
                if cursor.lay(false)(_ == '>')
                then
                  cursor.clone(mark, end)(buffer) yet result().also(cursor.next())
                else cursor.cue(resume) yet textual(mark, tag, entities)
              else cursor.cue(resume) yet textual(mark, tag, entities)
            else cursor.cue(resume) yet textual(mark, tag, entities)

        case '&' if entities =>
          val start = cursor.mark
          next()
          val mark2 = entity(cursor.mark).lay(mark): text =>
            cursor.clone(mark, start)(buffer)
            buffer.append(text)
            cursor.mark
          textual(mark2, close, entities)

        case char =>
          cursor.next() yet textual(mark, close, entities)

    def comment(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '-'      =>  val end = cursor.mark
                        next()
                        cursor.lay(fail(ExpectedMore)):
                          case '-' => expect('>') yet cursor.grab(mark, end)
                          case _   => comment(mark)
      case '\u0000' =>  insert.let(_(cursor.position, Hole.Comment))
                        next() yet comment(mark)
      case char     =>  next() yet comment(mark)

    def tag(foreign: Boolean): Token = cursor.lay(fail(ExpectedMore)):
      case '!'  =>  expect('-')
                    expect('-')
                    next()
                    content = cursor.hold(comment(cursor.mark))
                    cursor.next()
                    Token.Comment
      case '/'  =>  next()
                    content = cursor.hold:
                      if foreign then foreignTag(cursor.mark)
                      else tagname(cursor.mark, dom.elements).label
                    Token.Close

      case '\u0000' => fail(BadInsertion)
      case char =>
        content = cursor.hold(if foreign then foreignTag(cursor.mark) else tagname(cursor.mark, dom.elements).label)
        extra = cursor.hold(attributes(content, foreign))

        cursor.lay(fail(ExpectedMore)):
          case '/'       =>  expect('>') yet cursor.next() yet Token.Empty
          case '>'       =>  cursor.next() yet Token.Open
          case '\u0000'  =>  fail(BadInsertion)
          case char      =>  fail(Unexpected(char))

    def descend(parent: Tag, admissible: Set[Text]): Html =
      val admissible2 = if parent.transparent then admissible else parent.admissible
      read(parent, admissible2, extra, Nil, 0)

    def finish(parent: Tag, children: List[Html], count: Int): Html =
      if parent != root then
        if parent.autoclose
        then Node(parent.label, parent.attributes, array(children, count), false)
        else fail(Incomplete(parent.label))
      else if count > 1 then Fragment(children.reverse*) else children(0)

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
    def read(parent: Tag, admissible: Set[Text], atts: Map[Text, Optional[Text]], children: List[Html], count: Int)
    : Html =

        def admit(child: Text): Boolean =
          parent.foreign || parent.admissible(child) || parent.transparent && admissible(child)

        cursor.lay(finish(parent, children, count)):
          case '\u0000' => insert.let(_(cursor.position, Hole.Node(parent.label)))
                           next()
                           read(parent, admissible, atts, Textual("\u0000") :: children, count + 1)
          case '<' if parent.content != TextContent.Raw && parent.content != TextContent.Rcdata =>
            var level: Level = Level.Peer
            var current: Html = parent
            var currentTag: Tag = parent

            cursor.hold:
              val mark = cursor.mark

              def node(): Unit =
                current = Node(content, extra, array(children, count), parent.foreign)

              def close(): Unit =
                current = Node(parent.label, parent.attributes, array(children, count), parent.foreign)
                level = Level.Ascend

              def infer(tag: Tag): Unit =
                cursor.cue(mark)
                dom.infer(parent, tag).let: tag =>
                  currentTag = tag
                  level = Level.Descend
                . or:
                    if parent.autoclose then close()
                    else fail(InadmissibleTag(content, parent.label))

              next()

              tag(parent.foreign) match
                case Token.Comment => current = Comment(content)

                case Token.Empty   =>
                  if admit(content) then node() else infer:
                    if parent.foreign then Tag.foreign(content, extra)
                    else dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))

                case Token.Open =>
                  currentTag =
                    if parent.foreign then Tag.foreign(content, extra)
                    else dom.elements(content).or:
                      cursor.cue(mark)
                      fail(InvalidTag(content))

                  if !admit(content) then infer(currentTag) else if currentTag.void then node()
                  else level = Level.Descend

                case Token.Close =>
                  if content != parent.label then
                    cursor.cue(mark)
                    if parent.autoclose then close() else fail(MismatchedTag(parent.label, content))
                  else
                    cursor.next()
                    level = Level.Ascend
                    current = Node(content, atts, array(children, count), parent.foreign)

            level match
              case Level.Ascend  =>  current
              case Level.Peer    =>  read(parent, admissible, atts, current :: children, count + 1)
              case Level.Descend =>  val child = descend(currentTag, admissible)
                                     read(parent, admissible, atts, child :: children, count + 1)

          case char => parent.content match
            case TextContent.Whitespace =>
              whitespace() yet read(parent, admissible, atts, children, count)

            case TextContent.Raw =>
              val text = cursor.hold(textual(cursor.mark, parent.label, false))
              if text.s.isEmpty then Node(parent.label, parent.attributes, IArray(), parent.foreign)
              else Node(parent.label, parent.attributes, IArray(Textual(text)), parent.foreign)

            case TextContent.Rcdata =>
              val text = cursor.hold(textual(cursor.mark, parent.label, true))
              if text.s.isEmpty then Node(parent.label, parent.attributes, IArray(), parent.foreign)
              else Node(parent.label, parent.attributes, IArray(Textual(text)), parent.foreign)

            case TextContent.Normal =>
              val text = cursor.hold(textual(cursor.mark, Unset, true))
              if text.length == 0 then read(parent, admissible, atts, children, count + 1)
              else read(parent, admissible, atts, Textual(text) :: children, count + 1)

    skip()
    read(root, root.admissible, ListMap(), Nil, 0)

sealed into trait Html extends Topical:
  type Topic <: Label
  type Transport <: Label

  private[honeycomb] def of[topic <: Label]: this.type of topic =
    asInstanceOf[this.type of topic]

  private[honeycomb] def over[transport <: Label]: this.type over transport =
    asInstanceOf[this.type over transport]

  private[honeycomb] def in[form]: this.type in form = asInstanceOf[this.type in form]
