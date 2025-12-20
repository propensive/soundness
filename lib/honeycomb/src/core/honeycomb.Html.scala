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
import parasite.*
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
          mode        = Html.Mode.Whitespace,
          insertable  = true,
          foreign     = false), Format:
  type Topic = "html"
  type Transport = "head" | "body"

  erased trait Integral
  erased trait Decimal
  erased trait Id

  def doctype: Doctype = Doctype(t"html")

  given aggregable: [content <: Label: Reifiable to List[String]] => (dom: Dom)
        =>  Tactic[ParseError]
        =>  (Html of content) is Aggregable by Text =

    input =>
      val root = Tag.root(content.reification().map(_.tt).to(Set))
      parse(input.iterator, root).of[content]

  given aggregable2: (dom: Dom) => Tactic[ParseError] => Html is Aggregable by Text =
    input => parse(input.iterator, dom.generic, doctypes = false)

  given loadable: (dom: Dom) => Tactic[ParseError] => Html is Loadable by Text = stream =>
    val root = Tag.root(Set(t"html"))
    parse(stream.iterator, root, doctypes = true) match
      case Fragment(Doctype(doctype), content) => Document(content, Doctype(doctype))
      case html@Element("html", _, _, _)       => Document(html, doctype)
      case _                                   =>
        abort(ParseError(Html, Position(1.u, 1.u), Issue.BadDocument))

  // Up to 32 levels of two-space indentation
  private val indentation: Text =
    "\n                                                                "

  given streamable: (Dom, Monitor, Codicil) => Document[Html] is Streamable by Text =
    emit(_).to(Stream)

  def emit(document: Document[Html], flat: Boolean = false)(using dom: Dom)(using Monitor, Codicil)
  : Iterator[Text] =

      val emitter = Emitter[Text](4096)
      async:
        def recur(node: Html, indent: Int, block: Boolean, mode: Mode): Unit =
          node match
            case Fragment(nodes*) => nodes.each(recur(_, indent, block, mode))
            case Comment(comment) => emitter.put("<!--")
                                     emitter.put(comment)
                                     emitter.put("-->")
            case Doctype(text)    => emitter.put("<!DOCTYPE ")
                                     emitter.put(text) // FIXME: entities
                                     emitter.put(">")

            case TextNode(text) =>
              mode match
                case Mode.Raw =>
                  emitter.put(text)
                case _ =>
                  var pos: Int = 0
                  while pos < text.length do
                    val amp = text.s.indexOf('&', pos)
                    val lt = text.s.indexOf('<', pos)
                    val next = if amp < 0 then lt else if lt < 0 then amp else amp.min(lt)

                    if next >= 0 then
                      emitter.put(text, pos.z, next - pos)
                      if next == lt then emitter.put("&lt;")
                      if next == amp then emitter.put("&amp;")
                      pos = next + 1
                    else
                      emitter.put(text, pos.z, text.length - pos)
                      pos = text.length

            case Element(label, attributes, nodes, _) =>
              if block then emitter.put(indentation, Prim, indent*2 + 1)
              emitter.put("<")
              emitter.put(label)

              if !attributes.isEmpty then
                attributes.each: (key, value) =>
                  emitter.put(" ")
                  emitter.put(key)
                  value.let: value =>
                    emitter.put("=\"")
                    var pos: Int = 0

                    while pos < value.length do
                      val amp = value.s.indexOf('&', pos)
                      val quot = value.s.indexOf('\"', pos)
                      val next = if amp < 0 then quot else if quot < 0 then amp else amp.min(quot)

                      if next >= 0 then
                        emitter.put(value, pos.z, next - pos)
                        if next == quot then emitter.put("&quot;")
                        if next == amp then emitter.put("&amp;")
                        pos = next + 1
                      else
                        emitter.put(value, pos.z, value.length - pos)
                        pos = value.length

                    emitter.put("\"")

              emitter.put(">")

              val mode = dom.elements(label).lay(Mode.Normal)(_.mode)

              val whitespace =
                (mode == Mode.Whitespace || !nodes.exists(_.isInstanceOf[TextNode]))
                && block

              if !dom.elements(label).lay(false)(_.void) then
                nodes.each(recur(_, indent + 1, whitespace, mode))

                if block && whitespace
                then emitter.put(indentation, Prim, (indent*2 + 1).min(indentation.length))

                emitter.put("</")
                emitter.put(label)
                emitter.put(">")

        recur(document.metadata, 0, true, Mode.Whitespace)
        recur(document.root, 0, true, Mode.Whitespace)
        emitter.finish()

      emitter.iterator


  given showable: [html <: Html] => html is Showable =
    case Fragment(nodes*) => nodes.map(_.show).join
    case TextNode(text)    => text
    case Comment(text) => t"<!--$text-->"
    case Doctype(text) => t"<!$text>"

    case Element(tagname, attributes, children, _) =>
      val tagContent = if attributes.isEmpty then t"" else
        attributes.map:
          case (key, value) => value.lay(key) { value => t"""$key="$value"""" }
        . join(t" ", t" ", t"")

      t"<$tagname$tagContent>${children.map(_.show).join}</$tagname>"


  private enum Token:
    case Close, Comment, Empty, Open, Doctype, Cdata

  private enum Level:
    case Ascend, Descend, Peer

  trait Populable:
    node: Element =>
      def apply(children: Optional[Html of (? <: node.Transport)]*): Element of node.Topic =
        new Element(node.label, node.attributes, children.compact.nodes, node.foreign):
          type Topic = node.Topic

  trait Transparent:
    node: Element =>
      def apply[labels <: Label](children: Optional[Html of (? <: (labels | node.Transport))]*): Element of labels =
        new Element(node.label, node.attributes, children.compact.nodes, node.foreign):
          type Topic = labels


  import Issue.*
  def name: Text = t"HTML"

  given text: [label >: "#text" <: Label] => Conversion[Text, Html of label] =
    TextNode(_).of[label]

  given string: [label >: "#text" <: Label] => Conversion[String, Html of label] =
    string => TextNode(string.tt).of[label]

  given conversion3: [label <: Label, content >: label <: Label]
        =>  Conversion[Html of label, Html of content] =
    _.of[content]

  given comment: [content <: Label] =>  Conversion[Comment, Html of content] =
    _.of[content]

  given string2: Conversion[String, Html of "#foreign"] =
    string => TextNode(string.tt).of["#foreign"]

  given renderable: [content <: Label, value: Renderable in content]
        => Conversion[value, Html of content] =
    value.render(_)

  given sequences: [nodal, html <: Html] => (conversion: Conversion[nodal, html])
        =>  Conversion[Seq[nodal], Seq[html]] =
    (seq: Seq[nodal]) =>
      seq.map(conversion(_))

  enum Issue extends Format.Issue:
    case BadInsertion
    case ExpectedMore
    case UnexpectedDoctype
    case BadDocument
    case InvalidCdata
    case InvalidTag(name: Text)
    case InvalidTagStart(prefix: Text)
    case DuplicateAttribute(name: Text)
    case InadmissibleTag(name: Text, parent: Text)
    case OnlyWhitespace(char: Char)
    case Unexpected(char: Char)
    case UnknownEntity(name: Text)
    case ForbiddenUnquoted(char: Char)
    case MismatchedTag(open: Text, close: Text)
    case UnopenedTag(close: Text)
    case Incomplete(tag: Text)
    case UnknownAttribute(name: Text)
    case UnknownAttributeStart(name: Text)
    case InvalidAttributeUse(attribute: Text, element: Text)

    def describe: Message = this match
      case BadInsertion                   =>  m"a value cannot be inserted into HTML at this point"
      case ExpectedMore                   =>  m"the content ended prematurely"
      case UnexpectedDoctype              =>  m"the document type declaration was not expected here"
      case BadDocument                    =>  m"the document did not contain a single root tag"
      case InvalidCdata                   =>  m"CDATA content is only permitted in foreign namespaces"
      case InvalidTag(name)               =>  m"<$name> is not a valid tag"
      case InvalidTagStart(prefix)        =>  m"there is no valid tag whose name starts $prefix"
      case DuplicateAttribute(name)       =>  m"the attribute $name already exists on this tag"
      case InadmissibleTag(name, parent)  =>  m"<$name> cannot be a child of <$parent>"
      case OnlyWhitespace(char)           =>  m"the character $char was found where only whitespace is permitted"
      case Unexpected(char)               =>  m"the character $char was not expected"
      case UnknownEntity(name)            =>  m"the entity &$name is not defined"
      case ForbiddenUnquoted(char)        =>  m"the character $char is forbidden in an unquoted attribute"
      case MismatchedTag(open, close)     =>  m"the tag </$close> did not match the opening tag <$open>"
      case UnopenedTag(close)             =>  m"the tag </$close> has no corresponding opening tag"
      case Incomplete(tag)                =>  m"the content ended while the tag <$tag> was left open"
      case UnknownAttribute(name)         =>  m"$name is not a recognized attribute"
      case UnknownAttributeStart(name)    =>  m"there is no valid attribute whose name starts $name"
      case InvalidAttributeUse(name, tag) =>  m"the attribute $name cannot be used on the tag <$tag>"

  case class Position(line: Ordinal, column: Ordinal) extends Format.Position:
    def describe: Text = t"line ${line.n1}, column ${column.n1}"



  enum Mode:
    case Raw, Rcdata, Whitespace, Normal

  enum Hole:
    case Text, Tagbody, Comment
    case Element(tag: Text)
    case Attribute(tag: Text, attribute: Text)
    case Node(parent: Text)

  private[honeycomb] def parse[dom <: Dom]
       (input:       Iterator[Text],
        root:        Tag,
        callback:    Optional[(Ordinal, Hole) => Unit] = Unset,
        fastforward: Int                               = 0,
        doctypes:    Boolean = false)
       (using dom: Dom): Html raises ParseError =

    import lineation.linefeedChars

    val cursor = Cursor(input)
    val buffer: jl.StringBuilder = jl.StringBuilder()
    def result(): Text = buffer.toString.tt.also(buffer.setLength(0))
    var content: Text = t""
    var extra: Map[Text, Optional[Text]] = ListMap()
    var nodes: Array[Node] = new Array(4)
    var index: Int = 0
    var fragment: IArray[Node] = IArray()

    def push(node: Node): Unit =
      if index >= nodes.length then
        val nodes2 = new Array[Node](nodes.length*2)
        System.arraycopy(nodes, 0, nodes2, 0, nodes.length)
        nodes = nodes2

      nodes(index) = node
      index += 1

    def pop(): Unit = index -= 1

    def next(): Unit =
      if !cursor.next()
      then raise(ParseError(Html, Position(cursor.line, cursor.column), ExpectedMore))

    inline def expect(char: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum != char then fail(Unexpected(datum))

    inline def expectInsensitive(char: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum.minuscule != char.minuscule then fail(Unexpected(datum))

    def fail(issue: Issue): Nothing =
      abort(ParseError(Html, Position(cursor.line, cursor.column), issue))

    @tailrec
    def skip(): Unit = cursor.let:
      case ' ' | '\f' | '\n' | '\r' | '\t' => cursor.next() yet skip()
      case _                               => ()

    @tailrec
    def whitespace(): Unit = cursor.lay(()):
      case ' ' | '\f' | '\n' | '\r' | '\t' =>  cursor.next() yet whitespace()
      case '<'                             =>  ()
      case char                            =>  fail(OnlyWhitespace(char))

    @tailrec
    def tagname(mark: Mark, dictionary: Dictionary[Tag])(using Cursor.Held): Tag =
      cursor.lay(fail(ExpectedMore)):
        case char if char.isLetter || char.isDigit => dictionary(char.minuscule) match
          case Dictionary.Empty => cursor.next()
                                   val name = cursor.grab(mark, cursor.mark)
                                   cursor.cue(mark) yet fail(InvalidTagStart(name.lower))
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
        case char if char.isLetter || char == '-' => dictionary(char.minuscule) match
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
      case '\u0000' =>  callback.let(_(cursor.position, Hole.Text)) yet next() yet value(mark)
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
          case '\u0000'  => callback.let(_(cursor.position, Hole.Tagbody))
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
              case '\u0000' =>  callback.let(_(cursor.position, Hole.Attribute(tag, key2)))
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
      case '\u0000' =>  callback.let(_(cursor.position, Hole.Comment))
                        next() yet comment(mark)
      case char     =>  next() yet comment(mark)

    def cdata(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case ']'      =>  val end = cursor.mark
                        next()
                        cursor.lay(fail(ExpectedMore)):
                          case ']' => expect('>') yet cursor.grab(mark, end)
                          case _   => cdata(mark)
      case char     =>  next() yet cdata(mark)

    def doctype(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '>'   => cursor.grab(mark, cursor.mark).also(next())
      case other => next() yet doctype(mark)

    def tag(doctypes: Boolean, foreign: Boolean): Token = cursor.lay(fail(ExpectedMore)):
      case '!'  =>  next()
                    cursor.lay(fail(ExpectedMore)):
                      case '-' =>
                        expect('-')
                        next()
                        content = cursor.hold(comment(cursor.mark))
                        cursor.next()
                        Token.Comment

                      case '[' =>
                        expectInsensitive('c')
                        expectInsensitive('d')
                        expectInsensitive('a')
                        expectInsensitive('t')
                        expectInsensitive('a')
                        expect('[')
                        next()
                        content = cursor.hold(cdata(cursor.mark))
                        Token.Cdata

                      case 'D' | 'd' if doctypes =>
                        expectInsensitive('o')
                        expectInsensitive('c')
                        expectInsensitive('t')
                        expectInsensitive('y')
                        expectInsensitive('p')
                        expectInsensitive('e')
                        next()
                        skip()
                        content = cursor.hold(doctype(cursor.mark))
                        skip()
                        Token.Doctype

                      case char =>
                        fail(Unexpected(char))
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

    def finish(parent: Tag, count: Int): Node =
      if parent != root then
        if parent.autoclose then Element(parent.label, parent.attributes, array(count), false)
        else fail(Incomplete(parent.label))
      else
        if count > 1 then fragment = array(count)
        nodes(index - 1)

    def array(count: Int): IArray[Node] =
      val result = new Array[Node](count)
      System.arraycopy(nodes, 0.max(index - count), result, 0, count)
      index -= count
      result.immutable(using Unsafe)

    def descend(parent: Tag, admissible: Set[Text]): Node =
      val admissible2 = if parent.transparent then admissible else parent.admissible
      read(parent, admissible2, extra, 0)

    @tailrec
    def read(parent: Tag, admissible: Set[Text], map: Map[Text, Optional[Text]], count: Int): Node =

      def admit(child: Text): Boolean =
        parent.foreign || parent.admissible(child) || parent.transparent && admissible(child)

      cursor.lay(finish(parent, count)):
        case '\u0000' => callback.let(_(cursor.position, Hole.Node(parent.label)))
                         next()
                         push(TextNode("\u0000"))
                         read(parent, admissible, map, count + 1)

        case '<' if parent.mode != Mode.Raw && parent.mode != Mode.Rcdata =>
          var level: Level = Level.Peer
          var current: Node = parent
          var focus: Tag = parent

          cursor.hold:
            val mark = cursor.mark

            def node(): Unit =
              current = Element(content, extra, array(count), parent.foreign)

            def empty(): Unit =
              current = Element(content, extra, IArray(), parent.foreign)

            def close(): Unit =
              current = Element(parent.label, parent.attributes, array(count), parent.foreign)
              level = Level.Ascend

            def infer(tag: Tag): Unit =
              cursor.cue(mark)

              dom.infer(parent, tag).let: tag =>
                focus = tag
                level = Level.Descend

              . or:
                  if parent.autoclose then close()
                  else fail(InadmissibleTag(content, parent.label))

            next()
            if cursor.lay(false)(_ == '\u0000') then
              callback.let(_(cursor.position, Hole.Element(parent.label)))
              content = t"\u0000"
              node()
              expect('>')
              next()
            else tag(doctypes && parent == root, parent.foreign) match
              case Token.Comment => current = Comment(content)
              case Token.Doctype => current = Doctype(content)
              case Token.Cdata   => current =
                if parent.foreign then TextNode(content) else
                  fail(InvalidCdata)
                  Comment(t"[CDATA[${content}]]")

              case Token.Empty   =>
                if admit(content) then empty() else infer:
                  if parent.foreign then Tag.foreign(content, extra)
                  else dom.elements(content).or(cursor.cue(mark) yet fail(InvalidTag(content)))

              case Token.Open =>
                focus =
                  if parent.foreign then Tag.foreign(content, extra)
                  else dom.elements(content).or:
                    cursor.cue(mark)
                    fail(InvalidTag(content))

                if !admit(content) then infer(focus) else if focus.void then empty()
                else level = Level.Descend

              case Token.Close =>
                if content != parent.label then
                  cursor.cue(mark)
                  if parent.autoclose then close()
                  else if parent == root then fail(UnopenedTag(content))
                  else fail(MismatchedTag(parent.label, content))
                else
                  cursor.next()
                  level = Level.Ascend
                  current = Element(content, map, array(count), parent.foreign)

          level match
            case Level.Ascend  =>  current
            case Level.Peer    =>  push(current)
                                   read(parent, admissible, map, count + 1)
            case Level.Descend =>  val child = descend(focus, admissible)
                                   push(child)
                                   read(parent, admissible, map, count + 1)

        case char => parent.mode match
          case Mode.Whitespace =>
            whitespace() yet read(parent, admissible, map, count)

          case Mode.Raw =>
            val text = cursor.hold(textual(cursor.mark, parent.label, false))
            if text.s.isEmpty then Element(parent.label, parent.attributes, IArray(), parent.foreign)
            else Element(parent.label, parent.attributes, IArray(TextNode(text)), parent.foreign)

          case Mode.Rcdata =>
            val text = cursor.hold(textual(cursor.mark, parent.label, true))
            if text.s.isEmpty then Element(parent.label, parent.attributes, IArray(), parent.foreign)
            else Element(parent.label, parent.attributes, IArray(TextNode(text)), parent.foreign)

          case Mode.Normal =>
            val text = cursor.hold(textual(cursor.mark, Unset, true))
            if text.length == 0 then read(parent, admissible, map, count + 1)
            else push(TextNode(text)) yet read(parent, admissible, map, count + 1)

    if cursor.finished then Fragment() else
      skip()
      push(root)
      val head = read(root, root.admissible, ListMap(), 0)
      if fragment.isEmpty then head else Fragment(fragment*)

sealed into trait Html extends Topical, Documentary:
  type Topic <: Label
  type Transport <: Label
  type Metadata = Doctype
  type Chunks = Text

  private[honeycomb] def of[topic <: Label]: this.type of topic = asInstanceOf[this.type of topic]
  private[honeycomb] def in[form]: this.type in form = asInstanceOf[this.type in form]

  private[honeycomb] def over[transport <: Label]: this.type over transport =
    asInstanceOf[this.type over transport]

sealed trait Node extends Html

case class Comment(text: Text) extends Node:
  override def hashCode: Int = List(this).hashCode

  override def equals(that: Any): Boolean = that match
    case Comment(text0)           => text0 == text
    case Fragment(Comment(text0)) => text0 == text
    case _                        => false

case class TextNode(text: Text) extends Node:
  type Topic = "#text"

  override def hashCode: Int = List(this).hashCode

  override def equals(that: Any): Boolean = that match
    case Fragment(textual: TextNode) => this == textual
    case TextNode(text0)             => text0 == text
    case _                           => false

object Element:
  def foreign(label: Text, attributes: Map[Text, Optional[Text]], children: Html of "#foreign"*)
  : Element of "#foreign" =

      Element(label, attributes, children.nodes, true).of["#foreign"]

case class Element
            (label:      Text,
             attributes: Map[Text, Optional[Text]],
             children:   IArray[Node],
             foreign:    Boolean)
extends Node, Topical, Transportive, Dynamic:

  override def toString(): String =
    s"<$label>${children.mkString}</$label>"

  override def equals(that: Any): Boolean = that match
    case Fragment(node: Element) => this == node

    case Element(label, attributes, children, foreign) =>
      label == this.label && attributes == this.attributes && foreign == this.foreign
      && ju.Arrays.equals(children.mutable(using Unsafe), this.children.mutable(using Unsafe))

    case _ =>
      false

  override def hashCode: Int =
    ju.Arrays.hashCode(children.mutable(using Unsafe)) ^ attributes.hashCode ^ label.hashCode


  def selectDynamic(name: Label)(using attribute: name.type is Attribute on Topic in Whatwg)
  : Optional[Text] =

      attributes.at(name.tt)


  def updateDynamic[value](name: Label)(using attribute: name.type is Attribute in Whatwg)
       //(using Topic <:< attribute.Plane) - disabled to allow global attributes
       (value: value)
       (using attributive: value is Attributive to attribute.Topic)
  : Element of Topic over Transport in Whatwg =

      attributive.attribute(name, value).match
        case Unset        => Element(label, attributes - name, children, foreign)
        case (key, value) => Element(label, attributes.updated(key, value), children, foreign)

      . of[Topic]
      . over[Transport]
      . in[Whatwg]

object Fragment:
  @targetName("make")
  def apply[topic <: Label](nodes: Html of (? <: topic)*): Fragment of topic =
    new Fragment(nodes.nodes*).of[topic]

case class Fragment(nodes: Node*) extends Html:
  override def hashCode: Int = if nodes.length == 1 then nodes(0).hashCode else nodes.hashCode

  override def equals(that: Any): Boolean = that match
    case Fragment(nodes0*) => nodes0 == nodes
    case node: Html        => nodes.length == 1 && nodes(0) == node
    case _                 => false

case class Doctype(text: Text) extends Node:
  override def hashCode: Int = List(this).hashCode

  override def equals(that: Any): Boolean = that match
    case Doctype(text0)           => text0 == text
    case Fragment(Doctype(text0)) => text0 == text
    case _                        => false
