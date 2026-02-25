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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package xylophone

import language.dynamics

import java.lang as jl
import java.util as ju

import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import parasite.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import typonym.*
import vacuous.*
import zephyrine.*

import scala.annotation.tailrec

object Xml extends Tag.Container
  ( label = "xml", admissible = Set("head", "body") ), Format:
  type Topic = "xml"
  type Transport = "head" | "body"

  sealed trait Integral
  sealed trait Decimal
  sealed trait Id

  given textDecodable: [value: Decodable in Text] => Tactic[XmlError] => value is Decodable in Xml =
    case TextNode(text)                        =>  value.decoded(text)
    case Element(_, _, IArray(TextNode(text))) =>  value.decoded(text)
    case _                                     =>  abort(XmlError())

  case class attribute() extends StaticAnnotation

  case class XmlAttribute(label: Text, elements: Set[Text], global: Boolean):
    type Self <: Label
    type Topic
    type Plane <: Label

    def targets(tag: Text): Boolean = global || elements(tag)

    def merge(that: XmlAttribute): XmlAttribute =
      XmlAttribute(label, elements ++ that.elements, global || that.global)

  def header: Header = Header("1.0", Unset, Unset)

  inline given interpolator: Xml is Interpolable:
    type Result = Xml

    transparent inline def interpolate[parts <: Tuple](inline insertions: Any*): Xml =
      ${Xylophone.interpolator[parts]('insertions)}

  inline given extrapolator: Xml is Extrapolable:

    transparent inline def extrapolate[parts <: Tuple](scrutinee: Xml)
    :   Boolean | Option[Tuple | Xml] =

      ${Xylophone.extractor[parts]('scrutinee)}


  given aggregable: [content <: Label: Reifiable to List[String]] => (schema: XmlSchema)
  =>  Tactic[ParseError]
  =>  (Xml of content) is Aggregable by Text =

    input =>
      val root = Tag.root(content.reification().map(_.tt).to(Set))
      parse(input.iterator, root).of[content]

  given aggregable2: (schema: XmlSchema) => Tactic[ParseError] => Xml is Aggregable by Text =
    input => parse(input.iterator, schema.generic, headers = false)

  given loadable: (schema: XmlSchema) => Tactic[ParseError] => Xml is Loadable by Text = stream =>
    val root = Tag.root(Set(t"xml"))
    parse(stream.iterator, root, headers = true) match
      case Fragment(header: Header, content) => Document(content, header)

      case other =>
        abort(ParseError(Xml, Position(1.u, 1.u), Issue.BadDocument))

  given streamable: (Monitor, Codicil) => Document[Xml] is Streamable by Text =
    emit(_).to(Stream)

  def emit(document: Document[Xml], flat: Boolean = false)(using Monitor, Codicil): Iterator[Text] =

    val emitter = Emitter[Text](4096)
    async:
      def recur(node: Xml, indent: Int): Unit =
        node match
          case Fragment(nodes*) => nodes.each(recur(_, indent))

          case Comment(comment) =>
            emitter.put("<!--")
            emitter.put(comment)
            emitter.put("-->")

          case Cdata(text) =>
            emitter.put("<![CDATA[")
            emitter.put(text)
            emitter.put("]]>")

          case ProcessingInstruction(target, data) =>
            emitter.put("<?")
            emitter.put(target)

            if !data.nil then
              emitter.put(" ")
              emitter.put(data)

            emitter.put("?>")

          case Header(version, encoding, standalone) =>
            emitter.put("""<?xml version="""")
            emitter.put(version)
            emitter.put("\"")

            encoding.let: encoding =>
              emitter.put(""" encoding="""")
              emitter.put(encoding)
              emitter.put("\"")

            standalone.let: standalone =>
              emitter.put(if standalone then """ standalone="yes"""" else """ standalone="no"""")

            emitter.put("?>")

          case TextNode(text) =>
            var position: Int = 0
            while position < text.length do
              val amp = text.s.indexOf('&', position)
              val lt = text.s.indexOf('<', position)
              val next = if amp < 0 then lt else if lt < 0 then amp else amp.min(lt)

              if next >= 0 then
                emitter.put(text, position.z, next - position)
                if next == lt then emitter.put("&lt;")
                if next == amp then emitter.put("&amp;")
                position = next + 1
              else
                emitter.put(text, position.z, text.length - position)
                position = text.length

          case Element(label, attributes, nodes) =>
            emitter.put("<")
            emitter.put(label)

            if !attributes.nil then
              attributes.each: (key, value) =>
                emitter.put(" ")
                emitter.put(key)
                emitter.put("=\"")
                var position: Int = 0

                while position < value.length do
                  val amp = value.s.indexOf('&', position)
                  val quot = value.s.indexOf(Sqt, position)
                  val next = if amp < 0 then quot else if quot < 0 then amp else amp.min(quot)

                  if next >= 0 then
                    emitter.put(value, position.z, next - position)
                    if next == quot then emitter.put("&quot;")
                    if next == amp then emitter.put("&amp;")
                    position = next + 1
                  else
                    emitter.put(value, position.z, value.length - position)
                    position = value.length

                emitter.put("\"")

            emitter.put(">")

            nodes.each(recur(_, indent + 1))

            emitter.put("</")
            emitter.put(label)
            emitter.put(">")

      recur(document.metadata, 0)
      recur(document.root, 0)
      emitter.finish()

    emitter.iterator


  given showable: [xml <: Xml] => xml is Showable =
    case Fragment(nodes*) => nodes.map(_.show).join
    case TextNode(text)   => text
    case Comment(text)    => t"<!--$text-->"
    case Cdata(text)      => t"<![CDATA[$text]]>"

    case ProcessingInstruction(target, data) =>
      t"<?$target $data?>"

    case Header(version, encoding, standalone) =>
      val encodingText = encoding.lay(t""): encoding =>
        t" encoding=\"$encoding\""

      val standaloneText = standalone.lay(t""): standalone =>
        if standalone then t" standalone=\"yes\"" else t" standalone=\"no\""

      t"<?xml version=\"$version\"$encodingText$standaloneText>"

    case Element(tagname, attributes, children) =>
      val tagContent = if attributes.nil then t"" else
        attributes.map:
          case (key, value) => t"""$key="$value""""
        . join(t" ", t" ", t"")

      if children.nil then t"<$tagname$tagContent/>"
      else t"<$tagname$tagContent>${children.map(_.show).join}</$tagname>"


  private enum Token:
    case Close, Comment, Empty, Open, Header, Cdata, Pi

  private enum Level:
    case Ascend, Descend, Peer

  trait Populable:
    node: Element =>
      def apply(children: Optional[Xml of (? <: node.Transport)]*): Element of node.Topic =
        new Element(node.label, node.attributes, children.compact.nodes):
          type Topic = node.Topic

  import Issue.*
  def name: Text = t"XML"

  given text: [label >: "#text" <: Label] => Conversion[Text, Xml of label] =
    TextNode(_).of[label]

  given string: [label >: "#text" <: Label] => Conversion[String, Xml of label] =
    string => TextNode(string.tt).of[label]


  given conversion3: [label <: Label, content >: label <: Label]
  =>  Conversion[Xml of label, Xml of content] =

    _.of[content]


  given comment: [content <: Label] =>  Conversion[Comment, Xml of content] =
    _.of[content]

  given encodable: [value: Encodable in Xml] => Conversion[value, Xml] =
    value.encoded(_)

  given sequences: [nodal, xml <: Xml] => (conversion: Conversion[nodal, xml])
  =>  Conversion[Seq[nodal], Seq[xml]] =

    (sequence: Seq[nodal]) =>
      sequence.map(conversion(_))

  enum Issue extends Format.Issue:
    case BadInsertion
    case ExpectedMore
    case BadDocument
    case UnquotedAttribute
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
      case BadInsertion                   => m"a value cannot be inserted into XML at this point"
      case ExpectedMore                   => m"the content ended prematurely"
      case BadDocument                    => m"the document did not contain a single root tag"
      case UnquotedAttribute              => m"the attribute value must be single- or double-quoted"
      case InvalidTag(name)               => m"<$name> is not a valid tag"
      case InvalidTagStart(prefix)        => m"there is no valid tag whose name starts $prefix"
      case DuplicateAttribute(name)       => m"the attribute $name already exists on this tag"
      case InadmissibleTag(name, parent)  => m"<$name> cannot be a child of <$parent>"
      case Unexpected(char)               => m"the character $char was not expected"
      case UnknownEntity(name)            => m"the entity &$name is not defined"
      case UnopenedTag(close)             => m"the tag </$close> has no corresponding opening tag"
      case Incomplete(tag)                => m"the content ended while the tag <$tag> was left open"
      case UnknownAttribute(name)         => m"$name is not a recognized attribute"
      case UnknownAttributeStart(name)    => m"there is no valid attribute whose name starts $name"
      case InvalidAttributeUse(name, tag) => m"the attribute $name cannot be used on the tag <$tag>"

      case MismatchedTag(open, close) =>
        m"the tag </$close> did not match the opening tag <$open>"

      case ForbiddenUnquoted(char) =>
        m"the character $char is forbidden in an unquoted attribute"

      case OnlyWhitespace(char) =>
        m"the character $char was found where only whitespace is permitted"

  case class Position(line: Ordinal, column: Ordinal) extends Format.Position:
    def describe: Text = t"line ${line.n1}, column ${column.n1}"

  enum Hole:
    case Text, Tagbody, Comment
    case Element(tag: Text)
    case Attribute(tag: Text, attribute: Text)
    case Node(parent: Text)

  private[xylophone] def parse[schema <: XmlSchema]
    ( input:       Iterator[Text],
      root:        Tag,
      callback:    Optional[(Ordinal, Hole) => Unit] = Unset,
      fastforward: Int                               = 0,
      headers:     Boolean                           = false )
    ( using schema: XmlSchema ): Xml raises ParseError =

    import lineation.linefeedChars

    val cursor = Cursor(input)
    val buffer: jl.StringBuilder = jl.StringBuilder()
    def result(): Text = buffer.toString.tt.also(buffer.setLength(0))
    var content: Text = t""
    var extra: Map[Text, Text] = ListMap()
    var nodes: Array[Node] = new Array(4)
    var index: Int = 0
    var stack: Array[Tag] = new Array(4)
    var depth: Int = 0
    var fragment: IArray[Node] = IArray()

    def append(node: Node): Unit =
      if index >= nodes.length then
        val nodes2 = new Array[Node](nodes.length*2)
        System.arraycopy(nodes, 0, nodes2, 0, nodes.length)
        nodes = nodes2

      nodes(index) = node
      index += 1

    def push(tag: Tag): Unit =
      if depth >= stack.length then
        val stack2 = new Array[Tag](stack.length*2)
        System.arraycopy(stack, 0, stack2, 0, stack.length)
        stack = stack2

      stack(depth) = tag
      depth += 1

    def pop(): Unit = depth -= 1

    def next(): Unit =
      if !cursor.next()
      then raise(ParseError(Xml, Position(cursor.line, cursor.column), ExpectedMore))

    inline def ensure(chr: Char): Unit =
      cursor.let: datum =>
        if datum != chr then fail(Unexpected(datum))

    inline def expect(chr: Char): Unit =
      cursor.next()
      cursor.lay(fail(ExpectedMore)): datum =>
        if datum != chr then fail(Unexpected(datum))

    def fail(issue: Issue): Nothing =
      abort(ParseError(Xml, Position(cursor.line, cursor.column), issue))

    @tailrec
    def skip(): Unit = cursor.let:
      case ' ' | Ff | Lf | Cr | Ht => cursor.next() yet skip()
      case _                       => ()

    @tailrec
    def tagname(mark: Mark, dictionary: Optional[Dictionary[Tag]])(using Cursor.Held): Tag =
      cursor.lay(fail(ExpectedMore)):
        case chr if chr.isLetter || chr.isDigit =>
          dictionary.lay(next() yet tagname(mark, Unset)): dictionary =>
            dictionary(chr.minuscule) match
              case Dictionary.Empty =>
                cursor.next()
                val name = cursor.grab(mark, cursor.mark)
                cursor.cue(mark) yet fail(InvalidTagStart(name.lower))

              case other =>
                next() yet tagname(mark, other)

        case ' ' | Ff | Lf | Cr | Ht | '/' | '>' => dictionary match
          case Dictionary.Just("", tag)            =>  tag
          case Dictionary.Branch(tag: Tag, _)      =>  tag
          case _                                   =>  Tag.freeform(cursor.grab(mark, cursor.mark))

        case Nul =>
          fail(BadInsertion)

        case chr =>
          fail(Unexpected(chr))

    @tailrec
    def key(mark: Mark, dictionary: Optional[Dictionary[XmlAttribute]])(using Cursor.Held)
    :   XmlAttribute =
      cursor.lay(fail(ExpectedMore)):
        case chr if chr.isLetter || chr == '-' => dictionary.let(_(chr.minuscule)) match
          case Unset            => next() yet key(mark, Unset)
          case Dictionary.Empty => fail(UnknownAttributeStart(cursor.grab(mark, cursor.mark)))
          case dictionary       => next() yet key(mark, dictionary)

        case ' ' | Ff | Lf | Cr | Ht | '=' | '>' =>
          dictionary.let: dictionary =>
            dictionary.element.or:
              val name = cursor.grab(mark, cursor.mark)
              cursor.cue(mark)
              fail(UnknownAttribute(name))
          . or:
              XmlAttribute(cursor.grab(mark, cursor.mark), Set(), true)

        case chr =>
          fail(Unexpected(chr))

    @tailrec
    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '&' =>
        val start = cursor.mark
        next()
        val mark2 = entity(cursor.mark).lay(mark): text =>
          cursor.clone(mark, start)(buffer)
          buffer.append(text)
          cursor.mark
        value(mark2)

      case '"' =>
        cursor.clone(mark, cursor.mark)(buffer)
        next() yet result()

      case Nul =>
        callback.let(_(cursor.position, Hole.Text)) yet next() yet value(mark)

      case chr =>
        next() yet value(mark)

    @tailrec
    def singleQuoted(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case Sqt => cursor.grab(mark, cursor.mark).also(next())
      case chr => next() yet singleQuoted(mark)

    @tailrec
    def unquoted(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '>' | ' ' | Ff | Lf | Cr | Ht     => cursor.grab(mark, cursor.mark)
      case chr@('"' | Sqt | '<' | '=' | '`') => fail(ForbiddenUnquoted(chr))
      case Nul                               => fail(BadInsertion)
      case chr                               => next() yet unquoted(mark)

    def equality(): Unit = skip() yet cursor.lay(fail(ExpectedMore)):
      case '=' =>  next() yet skip() yet true
      case Nul =>  fail(BadInsertion)
      case chr =>  fail(Unexpected(chr))


    @tailrec
    def attributes(tag: Text, entries: Map[Text, Text] = ListMap())(using Cursor.Held)
    :   Map[Text, Text] =

      skip() yet cursor.lay(fail(ExpectedMore)):
        case '>' | '/' => entries

        case Nul =>
          callback.let(_(cursor.position, Hole.Tagbody))
          next()
          skip()
          attributes(tag, entries.updated(t"\u0000", t""))

        case _ =>
          val key2 =
            key(cursor.mark, schema.attributes.unless(schema.freeform)).tap: key =>
              if !schema.freeform && !key.targets(tag)
              then fail(InvalidAttributeUse(key.label, tag))
            . label

          if entries.has(key2) then fail(DuplicateAttribute(key2))

          equality()

          val assignment: Text = cursor.lay(fail(ExpectedMore)):
            case Nul =>
              callback.let(_(cursor.position, Hole.Attribute(tag, key2)))
              next() yet t"\u0000"

            case Dqt =>
              next() yet value(cursor.mark)

            case Sqt =>
              next() yet singleQuoted(cursor.mark)

            case _ =>
              fail(UnquotedAttribute)

          attributes(tag, entries.updated(key2, assignment))


    def entity(mark: Mark)(using Cursor.Held): Optional[Text] = cursor.lay(fail(ExpectedMore)):
      case '#'   => next() yet numericEntity(mark)
      case other => textEntity(mark, schema.entities)

    def numericEntity(mark: Mark)(using Cursor.Held): Optional[Text] =
      cursor.lay(fail(ExpectedMore)):
        case 'x' => next() yet hexEntity(mark, 0)
        case _   => decimalEntity(mark, 0)

    @tailrec
    def hexEntity(mark: Mark, value: Int)(using Cursor.Held): Optional[Text] =
      cursor.lay(fail(ExpectedMore)):
        case digit if digit.isDigit         =>
          cursor.next() yet hexEntity(mark, 16*value + (digit - '0'))

        case letter if 'a' <= letter <= 'f' =>
          cursor.next() yet hexEntity(mark, 16*value + (letter - 87))

        case letter if 'A' <= letter <= 'F' =>
          cursor.next() yet hexEntity(mark, 16*value + (letter - 55))

        case ';' =>
          cursor.next() yet value.unicode

        case chr =>
          Unset

    @tailrec
    def decimalEntity(mark: Mark, value: Int): Optional[Text] = cursor.lay(fail(ExpectedMore)):
      case digit if digit.isDigit => next() yet decimalEntity(mark, 10*value + (digit - '0'))
      case ';'                    => next() yet t"${value.toChar}"
      case chr                    => Unset

    @tailrec
    def textEntity(mark: Mark, dictionary: Dictionary[Text])(using Cursor.Held): Optional[Text] =
      cursor.lay(fail(ExpectedMore)):
        case chr if chr.isLetter | chr.isDigit =>  dictionary(chr) match
          case Dictionary.Empty                  =>  Unset
          case dictionary                        =>  cursor.next() yet textEntity(mark, dictionary)
        case ';'                               =>  cursor.next() yet dictionary(';').element
        case '='                               =>  Unset
        case Nul                               =>  fail(BadInsertion)
        case chr                               =>  dictionary.element


    @tailrec
    def textual(mark: Mark, close: Optional[Text], entities: Boolean)(using Cursor.Held): Text =
      cursor.lay(cursor.clone(mark, cursor.mark)(buffer) yet result()):
        case '<' | Nul =>
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

        case chr =>
          cursor.next() yet textual(mark, close, entities)

    def comment(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '-' =>
        val end = cursor.mark
        next()
        cursor.lay(fail(ExpectedMore)):
          case '-' => expect('>') yet cursor.grab(mark, end)
          case _   => comment(mark)

      case Nul =>
        callback.let(_(cursor.position, Hole.Comment))
        next() yet comment(mark)

      case chr =>
        next() yet comment(mark)

    def cdata(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case ']' =>
        val end = cursor.mark
        next()
        cursor.lay(fail(ExpectedMore)):
          case ']' => expect('>') yet cursor.grab(mark, end)
          case _   => cdata(mark)

      case chr =>
        next() yet cdata(mark)

    def piData(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case '?' =>
        val end = cursor.mark
        next()
        cursor.lay(fail(ExpectedMore)):
          case '>' => cursor.grab(mark, end)
          case _   => piData(mark)

      case chr =>
        next() yet piData(mark)

    def piTarget(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail(ExpectedMore)):
      case ' ' | Lf | Cr | Ff | Ht => cursor.grab(mark, cursor.mark)
      case chr                     =>  next() yet piTarget(mark)

    def tag(headers: Boolean): Token = cursor.lay(fail(ExpectedMore)):
      case '?' if headers =>
        cursor.consume(fail(ExpectedMore))("xml")
        next()
        skip()
        cursor.lay(fail(ExpectedMore)):
          case 'v'  => ()
          case chr => fail(Unexpected(chr))

        cursor.consume(fail(ExpectedMore))("ersion")
        next()
        equality()
        content = cursor.hold:
          cursor.lay(fail(ExpectedMore)):
            case Dqt =>  next() yet value(cursor.mark)
            case Sqt =>  next() yet singleQuoted(cursor.mark)
            case _   =>  fail(UnquotedAttribute)

        ensure('?')
        expect('>')
        next()
        skip()
        Token.Header

      case '?' =>
        next()
        content = cursor.hold(piTarget(cursor.mark))
        skip()
        Token.Pi

      case '!' =>
        next()
        cursor.lay(fail(ExpectedMore)):
          case '-' =>
            expect('-')
            next()
            content = cursor.hold(comment(cursor.mark))
            cursor.next()
            Token.Comment

          case '[' =>
            cursor.consume(fail(ExpectedMore))("CDATA[")
            next()
            content = cursor.hold(cdata(cursor.mark))
            Token.Cdata

          case chr =>
            fail(Unexpected(chr))

      case '/'  =>
        next()
        content = cursor.hold(tagname(cursor.mark, schema.elements.unless(schema.freeform)).label)
        Token.Close

      case Nul =>
        fail(BadInsertion)

      case chr =>
        content = cursor.hold(tagname(cursor.mark, schema.elements.unless(schema.freeform)).label)
        extra = cursor.hold(attributes(content))

        cursor.lay(fail(ExpectedMore)):
          case '/' =>  expect('>') yet cursor.next() yet Token.Empty
          case '>' =>  cursor.next() yet Token.Open
          case Nul =>  fail(BadInsertion)
          case chr =>  fail(Unexpected(chr))

    def finish(parent: Tag, count: Int): Node =
      if parent != root then fail(Incomplete(parent.label)) else
        if count > 1 then fragment = array(count)
        nodes(index - 1)

    def array(count: Int): IArray[Node] =
      val result = new Array[Node](count)
      System.arraycopy(nodes, 0.max(index - count), result, 0, count)
      index -= count
      result.immutable(using Unsafe)

    def descend(parent: Tag, admissible: Set[Text]): Node = read(parent, extra, 0)

    @tailrec
    def read(parent: Tag, map: Map[Text, Text], count: Int): Node =

      def admit(child: Text): Boolean = schema.freeform || parent.admissible(child)

      cursor.lay(finish(parent, count)):
        case Nul => callback.let(_(cursor.position, Hole.Node(parent.label)))
                    next()
                    append(TextNode("\u0000"))
                    read(parent, map, count + 1)

        case '<' =>
          var level: Level = Level.Peer
          var current: Node = parent
          var focus: Tag = parent

          cursor.hold:
            val mark = cursor.mark

            def node(): Unit =
              current = Element(content, extra, array(count))

            def empty(): Unit =
              current = Element(content, extra, IArray())

            def close(): Unit =
              current = Element(parent.label, parent.attributes, array(count))
              level = Level.Ascend

            next()
            if cursor.lay(false)(_ == Nul) then
              callback.let(_(cursor.position, Hole.Element(parent.label)))
              content = t"\u0000"
              node()
              expect('>')
              next()
            else tag(headers && parent == root) match
              case Token.Comment => current = Comment(content)
              case Token.Header  => current = Header(content, Unset, Unset)
              case Token.Cdata   => current = Cdata(content)
              case Token.Pi      =>
                current = ProcessingInstruction(content, cursor.hold(piData(cursor.mark)))

              case Token.Empty   =>
                if admit(content) then empty() else fail(InvalidTag(content))

              case Token.Open =>
                focus =
                  if schema.freeform then Tag.freeform(content) else schema.elements(content).or:
                    cursor.cue(mark)
                    fail(InvalidTag(content))

                if !admit(content) then fail(InvalidTag(content)) else level = Level.Descend

              case Token.Close =>
                if content != parent.label then
                  cursor.cue(mark)
                  if parent == root then fail(UnopenedTag(content))
                  else fail(MismatchedTag(parent.label, content))
                else
                  cursor.next()
                  level = Level.Ascend
                  current = Element(content, map, array(count))

          level match
            case Level.Ascend  =>  current

            case Level.Peer =>
              append(current)
              read(parent, map, count + 1)

            case Level.Descend =>
              push(focus)
              val child = descend(focus, admissible)
              pop()
              append(child)
              read(parent, map, count + 1)

        case chr =>
          val text = cursor.hold(textual(cursor.mark, Unset, true))
          if text.length == 0 then read(parent, map, count + 1)
          else append(TextNode(text)) yet read(parent, map, count + 1)

    if cursor.finished then Fragment() else
      skip()
      append(root)
      val head = read(root, ListMap(), 0)
      if fragment.nil then head else Fragment(fragment*)

sealed into trait Xml extends Dynamic, Topical, Documentary, Formal:
  type Topic <: Label
  type Transport <: Label
  type Metadata = Header
  type Chunks = Text
  type Form <: XmlSchema

  private[xylophone] def of[topic <: Label]: this.type of topic = asInstanceOf[this.type of topic]
  private[xylophone] def in[form]: this.type in form = asInstanceOf[this.type in form]

  private[xylophone] def over[transport <: Label]: this.type over transport =
    asInstanceOf[this.type over transport]

  def as[result: Decodable in Xml]: result = this match
    case Fragment(value) => result.decoded(value)
    case xml: Xml        => result.decoded(xml)

sealed trait Node extends Xml

case class Comment(text: Text) extends Node:
  override def hashCode: Int = List(this).hashCode

  override def equals(that: Any): Boolean = that match
    case Comment(text0)           => text0 == text
    case Fragment(Comment(text0)) => text0 == text
    case _                        => false

case class Cdata(text: Text) extends Node:
  override def hashCode: Int = List(this).hashCode // FIXME: infinite recursion

  override def equals(that: Any): Boolean = that match
    case Cdata(text0)           => text0 == text
    case Fragment(Cdata(text0)) => text0 == text
    case _                      => false

case class ProcessingInstruction(target: Text, data: Text) extends Node:
  override def hashCode: Int = List(this).hashCode // FIXME

  override def equals(that: Any): Boolean = that match
    case ProcessingInstruction(target0, data0)           => target0 == target && data0 == data
    case Fragment(ProcessingInstruction(target0, data0)) => target0 == target && data0 == data
    case _                                               => false

case class TextNode(text: Text) extends Node:
  type Topic = "#text"

  override def hashCode: Int = List(this).hashCode // FIXME: infinite recursion

  override def equals(that: Any): Boolean = that match
    case Fragment(textual: TextNode) => this == textual
    case TextNode(text0)             => text0 == text
    case _                           => false

case class Element
  ( label:      Text,
    attributes: Map[Text, Text],
    children:   IArray[Node] )
extends Node, Topical, Transportive:

  override def toString(): String =
    s"<$label>${children.mkString}</$label>"

  override def equals(that: Any): Boolean = that match
    case Fragment(node: Element) => this == node

    case Element(label, attributes, children) =>
      label == this.label && attributes == this.attributes
      && ju.Arrays.equals(children.mutable(using Unsafe), this.children.mutable(using Unsafe))

    case _ =>
      false

  override def hashCode: Int =
    ju.Arrays.hashCode(children.mutable(using Unsafe)) ^ attributes.hashCode ^ label.hashCode


  def selectDynamic(name: Label)(using attribute: name.type is Xml.XmlAttribute on Topic in Form)
  :   Optional[Text] =

    attributes.at(name.tt)


  def updateDynamic(name: Label)(using attribute: name.type is Xml.XmlAttribute in Form)
    ( value: Text )
  :   Element of Topic over Transport in Form =

    Element(label, attributes.updated(name, value), children)
    . of[Topic]
    . over[Transport]
    . in[Form]

object Fragment:
  @targetName("make")
  def apply[topic <: Label](nodes: Xml of (? <: topic)*): Fragment of topic =
    new Fragment(nodes.nodes*).of[topic]

case class Fragment(nodes: Node*) extends Xml:
  override def hashCode: Int = if nodes.length == 1 then nodes(0).hashCode else nodes.hashCode

  override def equals(that: Any): Boolean = that match
    case Fragment(nodes0*) => nodes0 == nodes
    case node: Xml         => nodes.length == 1 && nodes(0) == node
    case _                 => false

case class Header(version: Text, encoding: Optional[Text], standalone: Optional[Boolean])
extends Node:

  override def hashCode: Int = List(this).hashCode

  override def equals(that: Any): Boolean = that match
    case Fragment(header: Header) => equals(header)

    case Header(version0, encoding0, standalone0) =>
      version0 == version && encoding0 == encoding && standalone0 == standalone

    case _ =>
      false
