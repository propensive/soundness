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

import scala.annotation.tailrec
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

object Xml extends Tag.Container
  ( label = "xml", admissible = Set("head", "body") ), Format:
  type Topic = "xml"
  type Transport = "head" | "body"

  sealed trait Integral
  sealed trait Decimal
  sealed trait Id

  given textDecodable: [value: Decodable in Text] => Tactic[XmlError] => value is Decodable in Xml =
    case TextNode(text)                        => value.decoded(text)
    case Element(_, _, IArray(TextNode(text))) => value.decoded(text)
    case _                                     => abort(XmlError())

  case class attribute() extends StaticAnnotation

  case class XmlAttribute(label: Text, elements: Set[Text], global: Boolean):
    type Self <: Label
    type Topic
    type Plane <: Label

    def targets(tag: Text): Boolean = global || elements(tag)

    def merge(that: XmlAttribute): XmlAttribute =
      XmlAttribute(label, elements ++ that.elements, global || that.global)

  def header: Header = Header("1.0", Unset, Unset)

  extension (xml: Seq[Xml])
    def nodes: IArray[Node] =
      var count = 0

      for item <- xml do item match
        case fragment: Fragment => count += fragment.nodes.length
        case _                  => count += 1

      val array = new Array[Node](count)

      var index = 0
      for item <- xml do item match
        case Fragment(nodes*) =>
          for node <- nodes do
          array(index) = node
          index += 1

        case node: Node =>
          array(index) = node
          index += 1

      array.immutable(using Unsafe)

  inline given interpolator: Xml is Interpolable:
    type Result = Xml

    transparent inline def interpolate[parts <: Tuple](inline insertions: Any*): Xml =
      ${xylophone.internal.interpolator[parts]('insertions)}

  inline given extrapolator: Xml is Extrapolable:

    transparent inline def extrapolate[parts <: Tuple](scrutinee: Xml)
    :   Boolean | Option[Tuple | Xml] =

      ${xylophone.internal.extractor[parts]('scrutinee)}


  given aggregable: [content <: Label: Reifiable to List[String]] => (schema: XmlSchema)
  =>  Tactic[ParseError]
  =>  (Xml of content) is Aggregable by Text =

    input => XmlParser.fromIterator(input.iterator).parseXml(headers0 = false).of[content]

  given aggregable2: (schema: XmlSchema) => Tactic[ParseError] => Xml is Aggregable by Text =
    input => XmlParser.fromIterator(input.iterator).parseXml(headers0 = false)

  given loadable: (schema: XmlSchema) => Tactic[ParseError] => Xml is Loadable by Text = stream =>
    XmlParser.fromIterator(stream.iterator).parseXml(headers0 = true) match
      case Fragment((header: Header), rest*) =>
        if rest.length == 1 then Document(rest.head, header)
        else Document(Fragment(rest*), header)

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

          case Doctype(text) =>
            emitter.put("<!DOCTYPE ")
            emitter.put(text)
            emitter.put(">")

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


  private def appendEscapedText(builder: jl.StringBuilder, text: Text): Unit =
    val source = text.s
    val length = source.length
    var index = 0

    while index < length do
      source.charAt(index) match
        case '<' => builder.append("&lt;")
        case '&' => builder.append("&amp;")
        case chr => builder.append(chr)

      index += 1

  private def appendEscapedAttribute(builder: jl.StringBuilder, text: Text): Unit =
    val source = text.s
    val length = source.length
    var index = 0

    while index < length do
      source.charAt(index) match
        case '<' => builder.append("&lt;")
        case '&' => builder.append("&amp;")
        case '"' => builder.append("&quot;")
        case chr => builder.append(chr)

      index += 1

  private def appendXml(builder: jl.StringBuilder, node: Xml): Unit = node match
    case fragment: Fragment =>
      val nodes = fragment.nodes
      var index = 0

      while index < nodes.length do
        appendXml(builder, nodes(index))
        index += 1

    case TextNode(text) =>
      appendEscapedText(builder, text)

    case Comment(text) =>
      builder.append("<!--")
      builder.append(text.s)
      builder.append("-->")

    case Cdata(text) =>
      builder.append("<![CDATA[")
      builder.append(text.s)
      builder.append("]]>")

    case Doctype(text) =>
      builder.append("<!DOCTYPE ")
      builder.append(text.s)
      builder.append('>')

    case ProcessingInstruction(target, data) =>
      builder.append("<?")
      builder.append(target.s)

      if data.length > 0 then
        builder.append(' ')
        builder.append(data.s)

      builder.append("?>")

    case Header(version, encoding, standalone) =>
      builder.append("<?xml version=\"")
      builder.append(version.s)
      builder.append('"')

      encoding.let: encoding =>
        builder.append(" encoding=\"")
        builder.append(encoding.s)
        builder.append('"')

      standalone.let: standalone =>
        builder.append(if standalone then " standalone=\"yes\"" else " standalone=\"no\"")

      builder.append("?>")

    case Element(tagname, attributes, children) =>
      builder.append('<')
      builder.append(tagname.s)

      if !attributes.nil then attributes.foreach: (key, value) =>
        builder.append(' ')
        builder.append(key.s)
        builder.append("=\"")
        appendEscapedAttribute(builder, value)
        builder.append('"')

      if children.nil then builder.append("/>") else
        builder.append('>')
        var index = 0

        while index < children.length do
          appendXml(builder, children(index))
          index += 1

        builder.append("</")
        builder.append(tagname.s)
        builder.append('>')

  given showable: [xml <: Xml] => xml is Showable = node =>
    val builder = jl.StringBuilder()
    appendXml(builder, node)
    builder.toString.tt


  private enum Token:
    case Close, Comment, Empty, Open, Header, Cdata, Pi, Doctype

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

  // ───────────────────────────────────────────────────────────────────────
  // Unified parser: a single algorithm split across substrates.
  //
  // The abstract `XmlParser` base implements the entire XML parsing
  // algorithm (tags, attributes, text, entities, comments, CDATA,
  // processing instructions, doctype, header) in terms of a small
  // substrate API: `more`/`peek`/`advance`, `position`, `begin`/`slice`/
  // `reset`/`appendSlice`, and `computePosition` (for lazy line/column on
  // error). Two concrete substrates supply that API:
  //
  //   * `XmlDirect`   — operates directly on an underlying `String` with a
  //                     `var pos`. Used by `aggregable` / `loadable`. Faster
  //                     because all primitives are trivial integer
  //                     arithmetic; `final` lets the JIT devirtualise them.
  //   * `XmlStreaming` — operates over a `Cursor[Text]` against an
  //                     `Iterator[Text]`. Used by macro interpolators
  //                     (which need callbacks for `\u0000` placeholders);
  //                     handles unbounded streaming inputs.
  //
  // Both substrates share the same parsing algorithm — schema-validation,
  // header parsing, error reporting, entity expansion etc. all live in the
  // base class.

  private[xylophone] object XmlParser:
    import zephyrine.lineation.linefeedChars

    def fromText(text: Text)(using XmlSchema): XmlParser = new XmlParser(Cursor[Text](text))

    def fromIterator(input: Iterator[Text])(using XmlSchema): XmlParser =
      new XmlParser(Cursor[Text](input))

  private[xylophone] final class XmlParser(cursor: Cursor[Text])(using schema: XmlSchema):
    type Region = Cursor.Mark

    private var heldToken: Cursor.Held | Null = null

    protected inline def more: Boolean = cursor.more

    protected inline def peek: Char =
      cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Char]](cursor.unsafePos(using Unsafe))

    protected inline def advance(): Unit = cursor.next()
    protected inline def position: Int = cursor.position.n0

    protected inline def begin(): Cursor.Mark = cursor.mark(using heldToken.nn)

    protected inline def slice(start: Cursor.Mark): Text =
      val end = cursor.mark(using heldToken.nn)
      cursor.grab(start, end).asInstanceOf[Text]

    protected inline def slice(start: Cursor.Mark, end: Cursor.Mark): Text =
      cursor.grab(start, end).asInstanceOf[Text]

    protected inline def reset(start: Cursor.Mark): Unit = cursor.cue(start)

    protected def appendSlice(start: Cursor.Mark, buf: jl.StringBuilder): Unit =
      val end = cursor.mark(using heldToken.nn)
      cursor.clone(start, end)(buf.asInstanceOf[cursor.addressable.Target])

    protected def computePosition(): Position =
      // Lineation increments column AFTER each `advance`, so it tracks the
      // column of the next char to read. At end-of-input we want the column
      // of the LAST char read, matching the Direct/Streaming convention.
      val col = cursor.column.n1 - (if cursor.more then 0 else 1)
      Position(cursor.line.n1.u, col.max(1).u)

    // Optional callback invoked when a `\u0000` placeholder is encountered.
    // Used by the macro interpolators to record hole positions; the
    // substrate must still recognise `\u0000` as a special character at
    // each insertion point. Default no-op for non-macro use.
    var callback: Optional[(Ordinal, Hole) => Unit] = Unset

    protected inline def fail(issue: Issue)(using Tactic[ParseError]): Nothing =
      abort(ParseError(Xml, computePosition(), issue))

    protected inline def isAsciiLetter(c: Char): Boolean =
      ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

    protected inline def isAsciiDigit(c: Char): Boolean = '0' <= c && c <= '9'

    protected inline def isNameStart(c: Char): Boolean =
      isAsciiLetter(c) || c == '_' || c == ':' || (c > 127 && c.isLetter)

    protected inline def isNameChar(c: Char): Boolean =
      isAsciiLetter(c) || isAsciiDigit(c) || c == '_' || c == '-' || c == '.' || c == ':'
      || (c > 127 && (c == '·' || c.isLetter || c.isDigit))

    protected inline def isWs(c: Char): Boolean =
      c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\f'

    protected def skipWs(): Unit = while more && isWs(peek) do advance()

    protected def expectChar(chr: Char)(using Tactic[ParseError]): Unit =
      if !more then fail(Issue.ExpectedMore)
      if peek != chr then fail(Issue.Unexpected(peek))
      advance()

    protected def readName()(using Tactic[ParseError]): Text =
      val start = begin()
      if !more then fail(Issue.ExpectedMore)
      val first = peek
      if !isNameStart(first) then fail(Issue.Unexpected(first))
      advance()
      while more && isNameChar(peek) do advance()
      slice(start)

    // Parse an entity reference. Position must be just after the '&'.
    // Returns the expansion as a Text; leaves position just after the ';'.
    protected def readEntity()(using Tactic[ParseError]): Text =
      if !more then fail(Issue.ExpectedMore)
      if peek == '#' then
        advance()
        if !more then fail(Issue.ExpectedMore)
        var value = 0
        if peek == 'x' || peek == 'X' then
          advance()
          while more && peek != ';' do
            val c = peek
            value =
              if '0' <= c && c <= '9' then 16*value + (c - '0')
              else if 'a' <= c && c <= 'f' then 16*value + (c - 87)
              else if 'A' <= c && c <= 'F' then 16*value + (c - 55)
              else fail(Issue.Unexpected(c))
            advance()
        else
          while more && peek != ';' do
            val c = peek
            if '0' <= c && c <= '9' then value = 10*value + (c - '0')
            else fail(Issue.Unexpected(c))
            advance()
        if !more then fail(Issue.ExpectedMore)
        advance()
        if value <= 0xffff then String.valueOf(value.toChar).nn.tt
        else String.valueOf(Character.toChars(value).nn).nn.tt
      else
        val nameStart = begin()
        while more && peek != ';' do
          val c = peek
          if !isNameChar(c) then fail(Issue.Unexpected(c))
          advance()
        if !more then fail(Issue.ExpectedMore)
        val name = slice(nameStart)
        advance()
        schema.entities(name).or(fail(Issue.UnknownEntity(name)))

    // Read attribute value enclosed in `quote`. Returns the unescaped
    // value as Text. Position starts just after the opening quote and
    // ends just after the closing quote.
    protected def readAttrValue(tag: Text, quote: Char)(using Tactic[ParseError]): Text =
      val start = begin()
      var hasEntity = false
      var hasHole = false
      while more && peek != quote do
        val c = peek
        if c == '<' then fail(Issue.Unexpected('<'))
        if c == '&' then hasEntity = true
        if c == '\u0000' then hasHole = true
        advance()
      if !more then fail(Issue.ExpectedMore)
      val end = begin()
      advance() // consume closing quote
      if !hasEntity && !hasHole then slice(start, end)
      else
        // Mixed: entities and/or holes. Walk again with a buffer.
        // We rewind to start and re-scan with appendSlice between events.
        val buf = jl.StringBuilder()
        reset(start)
        var segStart = begin()
        while more && peek != quote do
          val c = peek
          if c == '&' then
            appendSlice(segStart, buf)
            advance()
            buf.append(readEntity().s)
            segStart = begin()
          else if c == '\u0000' then
            // Macro hole inside attribute value. Per existing semantics,
            // we report it but include U+0000 in the value text so the
            // macro post-processor can locate it.
            appendSlice(segStart, buf)
            callback.let(_(position.z, Hole.Attribute(tag, t"")))
            buf.append('\u0000')
            advance()
            segStart = begin()
          else
            advance()
        if !more then fail(Issue.ExpectedMore)
        appendSlice(segStart, buf)
        advance() // consume closing quote
        buf.toString.nn.tt

    protected def readAttributes(tag: Text)(using Tactic[ParseError]): Map[Text, Text] =
      var entries: Map[Text, Text] = ListMap()
      var done = false
      while !done do
        skipWs()
        if !more then fail(Issue.ExpectedMore)
        val ch = peek
        if ch == '>' || ch == '/' || ch == '?' then done = true
        else if ch == '\u0000' then
          callback.let(_(position.z, Hole.Tagbody))
          advance()
          skipWs()
          entries = entries.updated(t"\u0000", t"")
        else
          val key = readName()
          if entries.contains(key) then fail(Issue.DuplicateAttribute(key))
          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore)
          val q = peek
          val value =
            if q == '\u0000' then
              callback.let(_(position.z, Hole.Attribute(tag, key)))
              advance()
              t"\u0000"
            else if q == '"' || q == '\'' then
              advance()
              readAttrValue(tag, q)
            else fail(Issue.UnquotedAttribute)
          entries = entries.updated(key, value)
      entries

    // Read text up to the next '<'; returns the (possibly entity-expanded)
    // Text. Detects literal `]]>` as an error. Reports `\u0000` holes via
    // the callback.
    protected def readText(parentLabel: Text)(using Tactic[ParseError]): Text =
      val start = begin()
      var hasEntity = false
      var hasHole = false
      var bracketCount = 0
      while more && peek != '<' do
        val c = peek
        if c == '&' then hasEntity = true
        if c == '\u0000' then hasHole = true
        if c == ']' then bracketCount += 1
        else
          if bracketCount >= 2 && c == '>' then fail(Issue.Unexpected('>'))
          bracketCount = 0
        advance()
      if !hasEntity && !hasHole then slice(start)
      else
        val buf = jl.StringBuilder()
        reset(start)
        var segStart = begin()
        while more && peek != '<' do
          val c = peek
          if c == '&' then
            appendSlice(segStart, buf)
            advance()
            buf.append(readEntity().s)
            segStart = begin()
          else if c == '\u0000' then
            appendSlice(segStart, buf)
            callback.let(_(position.z, Hole.Node(parentLabel)))
            buf.append('\u0000')
            advance()
            segStart = begin()
          else
            advance()
        appendSlice(segStart, buf)
        buf.toString.nn.tt

    protected def readComment()(using Tactic[ParseError]): Text =
      val start = begin()
      while
        if !more then fail(Issue.ExpectedMore)
        !(peek == '-')
      do advance()
      // Try to match `-->`
      val end = begin()
      advance()
      if !more then fail(Issue.ExpectedMore)
      if peek != '-' then
        // Not the end; continue from here
        readComment_continue(start)
      else
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peek))
        advance()
        slice(start, end)

    private def readComment_continue(start: Region)(using Tactic[ParseError]): Text =
      // We saw '-' but the next wasn't '-' or '>'. Continue scanning.
      while more && peek != '-' do advance()
      if !more then fail(Issue.ExpectedMore)
      val end = begin()
      advance()
      if !more then fail(Issue.ExpectedMore)
      if peek != '-' then readComment_continue(start)
      else
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peek))
        advance()
        slice(start, end)

    protected def readCdata()(using Tactic[ParseError]): Text =
      val start = begin()
      var done = false
      var endRegion: Region = start
      while !done do
        if !more then fail(Issue.ExpectedMore)
        if peek == ']' then
          val maybeEnd = begin()
          advance()
          if more && peek == ']' then
            advance()
            if more && peek == '>' then
              endRegion = maybeEnd
              advance()
              done = true
        else advance()
      slice(start, endRegion)

    // Position must be just after '<?'. Reads PI target + data, returning
    // the appropriate Node.
    protected def readProcessingInstruction()(using Tactic[ParseError]): Node =
      val nameStart = begin()
      if !more then fail(Issue.ExpectedMore)
      val first = peek
      if !isNameStart(first) then fail(Issue.Unexpected(first))
      advance()
      while more && isNameChar(peek) do advance()
      val target = slice(nameStart)

      val isXmlName =
        target.s.length == 3
        && (target.s.charAt(0) == 'x' || target.s.charAt(0) == 'X')
        && (target.s.charAt(1) == 'm' || target.s.charAt(1) == 'M')
        && (target.s.charAt(2) == 'l' || target.s.charAt(2) == 'L')

      if isXmlName then
        if !headers then fail(Issue.InvalidTag(target))
        headers = false
        skipWs()
        val versionKey = readName()
        if versionKey != t"version" then fail(Issue.Unexpected(versionKey.s.charAt(0)))
        skipWs()
        expectChar('=')
        skipWs()
        if !more then fail(Issue.ExpectedMore)
        val q = peek
        if q != '"' && q != '\'' then fail(Issue.UnquotedAttribute)
        advance()
        val version = readAttrValue(target, q)
        skipWs()
        var encoding: Optional[Text] = Unset
        var standalone: Optional[Boolean] = Unset
        if more && peek == 'e' then
          val key = readName()
          if key != t"encoding" then fail(Issue.Unexpected(key.s.charAt(0)))
          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore)
          val q2 = peek
          if q2 != '"' && q2 != '\'' then fail(Issue.UnquotedAttribute)
          advance()
          encoding = readAttrValue(target, q2)
          skipWs()
        if more && peek == 's' then
          val key = readName()
          if key != t"standalone" then fail(Issue.Unexpected(key.s.charAt(0)))
          skipWs()
          expectChar('=')
          skipWs()
          if !more then fail(Issue.ExpectedMore)
          val q2 = peek
          if q2 != '"' && q2 != '\'' then fail(Issue.UnquotedAttribute)
          advance()
          val v = readAttrValue(target, q2)
          standalone = v.s match
            case "yes" => true
            case "no"  => false
            case _     => fail(Issue.Unexpected(v.s.charAt(0)))
          skipWs()
        if !more then fail(Issue.ExpectedMore)
        if peek != '?' then fail(Issue.Unexpected(peek))
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peek))
        advance()
        Header(version, encoding, standalone)
      else
        skipWs()
        val dataStart = begin()
        while
          if !more then fail(Issue.ExpectedMore)
          !(peek == '?')
        do advance()
        // Now at '?'. Need '?>'.
        val dataEnd = begin()
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then
          // Not the terminator, continue
          readPiData(dataStart, target)
        else
          advance()
          val data = slice(dataStart, dataEnd)
          ProcessingInstruction(target, data)

    private def readPiData(dataStart: Region, target: Text)
      (using Tactic[ParseError])
    :   ProcessingInstruction =

      while more && peek != '?' do advance()
      if !more then fail(Issue.ExpectedMore)
      val dataEnd = begin()
      advance()
      if !more then fail(Issue.ExpectedMore)
      if peek != '>' then readPiData(dataStart, target)
      else
        advance()
        ProcessingInstruction(target, slice(dataStart, dataEnd))

    protected def readDoctype()(using Tactic[ParseError]): Text =
      skipWs()
      val start = begin()
      while more && peek != '>' do advance()
      if !more then fail(Issue.ExpectedMore)
      val end = begin()
      advance()
      slice(start, end)

    // Read a single element starting just after '<'.
    protected def readElement()(using Tactic[ParseError]): Element =
      // Detect `<\u0000` (macro element hole)
      if more && peek == '\u0000' then
        callback.let(_(position.z, Hole.Element(t"")))
        advance()
        if !more then fail(Issue.ExpectedMore)
        if peek != '>' then fail(Issue.Unexpected(peek))
        advance()
        Element(t"\u0000", ListMap(), IArray.empty[Node])
      else
        val name = readName()
        val attrs = readAttributes(name)
        if !more then fail(Issue.ExpectedMore)
        if peek == '/' then
          advance()
          if !more then fail(Issue.ExpectedMore)
          if peek != '>' then fail(Issue.Unexpected(peek))
          advance()
          Element(name, attrs, IArray.empty[Node])
        else
          if peek != '>' then fail(Issue.Unexpected(peek))
          advance()
          val children = readChildren(name)
          Element(name, attrs, children)

    protected def readChildren(parentName: Text)(using Tactic[ParseError]): IArray[Node] =
      val children = scala.collection.mutable.ArrayBuffer[Node]()
      var done = false
      while !done do
        if !more then fail(Issue.Incomplete(parentName))
        val c = peek
        if c == '<' then
          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek
          if c2 == '/' then
            advance()
            val close = readName()
            skipWs()
            if !more then fail(Issue.ExpectedMore)
            if peek != '>' then fail(Issue.Unexpected(peek))
            advance()
            if close != parentName then fail(Issue.MismatchedTag(parentName, close))
            done = true
          else if c2 == '!' then
            advance()
            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peek))
              advance()
              children += Comment(readComment())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              children += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peek))
          else if c2 == '?' then
            advance()
            children += readProcessingInstruction()
          else
            children += readElement()
        else
          val text = readText(parentName)
          if text.length > 0 then children += TextNode(text)
      IArray.from(children)

    protected def consumeLiteral(literal: String)(using Tactic[ParseError]): Unit =
      var i = 0
      while i < literal.length do
        if !more then fail(Issue.ExpectedMore)
        if peek != literal.charAt(i) then fail(Issue.Unexpected(peek))
        advance()
        i += 1

    private var headers: Boolean = false

    def parseXml(headers0: Boolean)(using Tactic[ParseError]): Xml =
      cursor.hold:
        heldToken = summon[Cursor.Held]
        try parseXml0(headers0) finally heldToken = null

    private def parseXml0(headers0: Boolean)(using Tactic[ParseError]): Xml =
      headers = headers0
      skipWs()
      val nodes = scala.collection.mutable.ArrayBuffer[Node]()
      while more do
        if peek != '<' then
          val text = readText(t"")
          if text.length > 0 then nodes += TextNode(text)
        else
          advance()
          if !more then fail(Issue.ExpectedMore)
          val c2 = peek
          if c2 == '!' then
            advance()
            if more && peek == '-' then
              advance()
              if !more then fail(Issue.ExpectedMore)
              if peek != '-' then fail(Issue.Unexpected(peek))
              advance()
              nodes += Comment(readComment())
            else if more && (peek == 'D' || peek == 'd') then
              consumeLiteralCi("DOCTYPE")
              nodes += Doctype(readDoctype())
            else if more && peek == '[' then
              advance()
              consumeLiteral("CDATA[")
              nodes += Cdata(readCdata())
            else
              if !more then fail(Issue.ExpectedMore)
              fail(Issue.Unexpected(peek))
          else if c2 == '?' then
            advance()
            nodes += readProcessingInstruction()
          else if c2 == '/' then
            advance()
            val close = readName()
            fail(Issue.UnopenedTag(close))
          else
            nodes += readElement()
        skipWs()

      if nodes.length == 1 then nodes(0)
      else Fragment(nodes.toSeq*)

    protected def consumeLiteralCi(literal: String)(using Tactic[ParseError]): Unit =
      var i = 0
      while i < literal.length do
        if !more then fail(Issue.ExpectedMore)
        val expected = literal.charAt(i)
        val got = peek
        val matches =
          got == expected
          || (isAsciiLetter(expected) && (got == (expected | 0x20).toChar || got == (expected & ~0x20).toChar))
        if !matches then fail(Issue.Unexpected(got))
        advance()
        i += 1

  // ───────────────────────────────────────────────────────────────────────
  // Public entry points.

  // Back-compat for macro interpolators: matches the previous cursor-based
  // signature (Iterator[Text] + callback).
  private[xylophone] def parse[schema <: XmlSchema]
    ( input:    Iterator[Text],
      root:     Tag,
      callback: Optional[(Ordinal, Hole) => Unit] = Unset,
      headers0: Boolean                           = false )
    ( using schema: XmlSchema )
  :   Xml raises ParseError =

    val parser = XmlParser.fromIterator(input)
    parser.callback = callback
    parser.parseXml(headers0)


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
  override def hashCode: Int = text.hashCode*31 + 0x436F6D6D

  override def equals(that: Any): Boolean = that match
    case Comment(text0)           => text0 == text
    case Fragment(Comment(text0)) => text0 == text
    case _                        => false

case class Doctype(text: Text) extends Node:
  override def hashCode: Int = text.hashCode*31 + 0x44637470

  override def equals(that: Any): Boolean = that match
    case Doctype(text0)           => text0 == text
    case Fragment(Doctype(text0)) => text0 == text
    case _                        => false

case class Cdata(text: Text) extends Node:
  override def hashCode: Int = text.hashCode*31 + 0x43646174

  override def equals(that: Any): Boolean = that match
    case Cdata(text0)           => text0 == text
    case Fragment(Cdata(text0)) => text0 == text
    case _                      => false

case class ProcessingInstruction(target: Text, data: Text) extends Node:
  override def hashCode: Int = (target.hashCode*31 + data.hashCode)*31 + 0x50494E73

  override def equals(that: Any): Boolean = that match
    case ProcessingInstruction(target0, data0)           => target0 == target && data0 == data
    case Fragment(ProcessingInstruction(target0, data0)) => target0 == target && data0 == data
    case _                                               => false

case class TextNode(text: Text) extends Node:
  type Topic = "#text"

  override def hashCode: Int = text.hashCode*31 + 0x54657874

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
  override def hashCode: Int =
    ((version.hashCode*31 + encoding.hashCode)*31 + standalone.hashCode)*31 + 0x48646572

  override def equals(that: Any): Boolean = that match
    case Fragment(header: Header) => equals(header)

    case Header(version0, encoding0, standalone0) =>
      version0 == version && encoding0 == encoding && standalone0 == standalone

    case _ =>
      false
