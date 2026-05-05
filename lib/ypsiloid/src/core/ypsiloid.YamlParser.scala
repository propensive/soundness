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
package ypsiloid

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*

import YamlError.Reason

object YamlParser:
  private case class Line(indent: Int, content: Text)

  private val positiveInfinityLiterals =
    Set(t".inf", t".Inf", t".INF", t"+.inf", t"+.Inf", t"+.INF")

  private val negativeInfinityLiterals = Set(t"-.inf", t"-.Inf", t"-.INF")
  private val nanLiterals = Set(t".nan", t".NaN", t".NAN")
  private val nullLiterals = Set(t"null", t"Null", t"NULL", t"~")
  private val trueLiterals = Set(t"true", t"True", t"TRUE")
  private val falseLiterals = Set(t"false", t"False", t"FALSE")

  def parse(input: Text)(using Tactic[YamlError]): YamlAst = new YamlParser().parse(input)

  def parseAll(input: Text)(using Tactic[YamlError]): List[YamlAst] =
    splitDocuments(input).map(new YamlParser().parse(_))

  private def splitDocuments(input: Text): List[Text] =
    val docs = scala.collection.mutable.ArrayBuffer[Text]()
    val current = scala.collection.mutable.ArrayBuffer[Text]()

    def flush(): Unit =
      if current.exists(!_.trim.nil) then docs.append(current.join(t"\n"))
      current.clear()

    for line <- input.cut(t"\n", -1) do
      if line.trim == t"---" || line.trim == t"..." then flush() else current.append(line)

    flush()
    docs.toList

class YamlParser(using Tactic[YamlError]):
  import YamlParser.Line

  private val anchors = scala.collection.mutable.Map.empty[Text, YamlAst]

  def parse(input: Text): YamlAst =
    val lines = preprocess(input)
    if lines.isEmpty then YamlAst.Null else parseBlock(lines)

  private def preprocess(input: Text): List[Line] =
    quoteAwareLines(input).flatMap: rawLine =>
      val rightTrimmed = stripLineComment(rawLine).trim(Rtl)
      val indent = rightTrimmed.keep(_ == ' ', Ltr).length
      val content = rightTrimmed.skip(_ == ' ', Ltr)

      if content.nil || content == t"---" || content == t"..." then None
      else Some(Line(indent, content))

  private def quoteAwareLines(input: Text): List[Text] =
    val source = input.s
    val lines = scala.collection.mutable.ArrayBuffer[Text]()
    val current = new StringBuilder
    var inSingleQuote = false
    var inDoubleQuote = false
    var index = 0

    while index < source.length do
      val char = source.charAt(index)
      val escaped = index > 0 && source.charAt(index - 1) == '\\'

      if char == '\'' && !inDoubleQuote then inSingleQuote = !inSingleQuote
      else if char == '"' && !inSingleQuote && !escaped then inDoubleQuote = !inDoubleQuote

      if char == '\n' && !inSingleQuote && !inDoubleQuote then
        lines.append(current.toString.tt)
        current.clear()
      else
        current.append(char)

      index += 1

    lines.append(current.toString.tt)
    lines.toList

  private def parseBlock(lines: List[Line]): YamlAst =
    if lines.isEmpty then YamlAst.Null
    else
      val head = lines.head

      if isBareAnchor(head.content) then
        val name = head.content.skip(1)
        val value = if lines.tail.isEmpty then YamlAst.Null else parseBlock(lines.tail)
        anchors.update(name, value)
        value
      else if isBlockSequenceItem(head.content) then
        parseBlockSequence(lines)
      else if isBlockMappingHead(head.content) then
        parseBlockMapping(lines)
      else if isBlockScalarIndicator(head.content) && lines.tail.nonEmpty then
        parseBlockScalar(head.content, lines.tail)
      else if lines.lengthCompare(1) == 0 then
        parseTrimmed(head.content)
      else
        parseTrimmed(lines.map(_.content).join(t" ").trim)

  private def isBareAnchor(content: Text): Boolean =
    content.length >= 2 && content.s.charAt(0) == '&' && !content.contains(' ')

  private def isBlockSequenceItem(content: Text): Boolean =
    content == t"-" || content.starts(t"- ")

  private def isBlockMappingHead(content: Text): Boolean =
    val colon = findTopLevelColon(content)
    colon >= 0 && (colon == content.length - 1 || content.s.charAt(colon + 1) == ' ')

  private def isBlockScalarIndicator(content: Text): Boolean =
    content == t"|" || content == t">" || content == t"|-" || content == t"|+"
    || content == t">-" || content == t">+"

  private def parseBlockScalar(indicator: Text, lines: List[Line]): YamlAst =
    if lines.isEmpty then YamlAst.Str(t"")
    else
      val literal = indicator.s.charAt(0) == '|'
      val chomp = if indicator.length > 1 then indicator.s.charAt(1) else 'c'
      val baseIndent = lines.head.indent
      val builder = new StringBuilder

      lines.zipWithIndex.foreach: (line, position) =>
        val padding = (line.indent - baseIndent).max(0)
        if position > 0 then builder.append(if literal then '\n' else ' ')
        var space = 0
        while space < padding do
          builder.append(' ')
          space += 1
        builder.append(line.content.s)

      chomp match
        case '-' => ()
        case _   => builder.append('\n')

      YamlAst.Str(builder.toString.tt)

  private def parseBlockSequence(lines: List[Line]): YamlAst =
    val baseIndent = lines.head.indent
    val items = scala.collection.mutable.ArrayBuffer[YamlAst]()
    var rest: List[Line] = lines

    def headIsItem: Boolean =
      rest.nonEmpty && rest.head.indent == baseIndent && isBlockSequenceItem(rest.head.content)

    while headIsItem do
      val first = rest.head
      rest = rest.tail
      val firstContent = if first.content == t"-" then t"" else first.content.skip(2)
      val itemLines = scala.collection.mutable.ArrayBuffer[Line]()

      if !firstContent.nil then itemLines.append(Line(0, firstContent))

      while rest.nonEmpty && rest.head.indent > baseIndent do
        itemLines.append(Line(rest.head.indent - baseIndent - 2, rest.head.content))
        rest = rest.tail

      items.append(parseBlock(itemLines.toList))

    YamlAst.Sequence(IArray.from(items))

  private def parseBlockMapping(lines: List[Line]): YamlAst =
    val baseIndent = lines.head.indent
    val entries = scala.collection.mutable.ArrayBuffer[(YamlAst, YamlAst)]()
    var rest: List[Line] = lines

    def gatherContinuation(): List[Line] =
      val buffer = scala.collection.mutable.ArrayBuffer[Line]()
      while rest.nonEmpty && rest.head.indent > baseIndent do
        buffer.append(rest.head)
        rest = rest.tail
      buffer.toList

    def headIsEntry: Boolean =
      rest.nonEmpty && rest.head.indent == baseIndent && isBlockMappingHead(rest.head.content)

    while headIsEntry do
      val line = rest.head
      rest = rest.tail
      val colon = findTopLevelColon(line.content)
      val keyText = line.content.before(colon.z).trim

      val inline =
        if colon + 1 < line.content.length then line.content.skip(colon + 1).trim else t""

      val key = parseTrimmed(keyText)

      val bareAnchorWithBlock =
        isBareAnchor(inline) && rest.nonEmpty && rest.head.indent > baseIndent

      if bareAnchorWithBlock then
        val value = parseBlock(gatherContinuation())
        anchors.update(inline.skip(1), value)
        entries.append((key, value))
      else if !inline.nil && !isBlockScalarIndicator(inline) then
        entries.append((key, parseTrimmed(inline)))
      else
        val valueLines = gatherContinuation()

        val value =
          if inline.nil then parseBlock(valueLines)
          else parseBlockScalar(inline, valueLines)

        entries.append((key, value))

    YamlAst.Mapping(IArray.from(entries))

  private def parseTrimmed(input: Text): YamlAst =
    if input.nil then YamlAst.Null
    else if input.s.charAt(0) == '*' then resolveAlias(input.skip(1).trim)
    else if input.s.charAt(0) == '&' then
      val spaceIndex = input.s.indexOf(' ')

      if spaceIndex < 0 then
        val name = input.skip(1)
        anchors.update(name, YamlAst.Null)
        YamlAst.Null
      else
        val name = input.segment(Sec till spaceIndex.z)
        val value = parseTrimmedCore(input.skip(spaceIndex + 1).trim)
        anchors.update(name, value)
        value
    else
      parseTrimmedCore(input)

  private def resolveAlias(name: Text): YamlAst =
    anchors.get(name) match
      case Some(value) => value

      case None =>
        raise(YamlError(Reason.UnknownAlias(name))) yet YamlAst.Null

  private def parseTrimmedCore(input: Text): YamlAst =
    val length = input.length

    if length == 0 then YamlAst.Null
    else
      val first = input.s.charAt(0)
      val last = input.s.charAt(length - 1)

      if length >= 2 && first == '"' && last == '"'
      then YamlAst.Str(unescapeDoubleQuoted(input.segment(Sec till (length - 1).z)))
      else if length >= 2 && first == '\'' && last == '\''
      then YamlAst.Str(unescapeSingleQuoted(input.segment(Sec till (length - 1).z)))
      else if length >= 2 && first == '[' && last == ']'
      then parseFlowSequence(input.segment(Sec till (length - 1).z))
      else if length >= 2 && first == '{' && last == '}'
      then parseFlowMapping(input.segment(Sec till (length - 1).z))
      else resolveScalar(input)

  private def parseFlowSequence(body: Text): YamlAst =
    if body.trim.nil then YamlAst.Sequence(IArray.empty)
    else YamlAst.Sequence(IArray.from(splitFlowItems(body).map(item => parseTrimmed(item.trim))))

  private def parseFlowMapping(body: Text): YamlAst =
    if body.trim.nil then YamlAst.Mapping(IArray.empty)
    else
      val entries = splitFlowItems(body).map(item => parseFlowMappingEntry(item.trim))
      YamlAst.Mapping(IArray.from(entries))

  private def parseFlowMappingEntry(entry: Text): (YamlAst, YamlAst) =
    val colonIndex = findTopLevelColon(entry)

    if colonIndex < 0 then (parseTrimmed(entry), YamlAst.Null)
    else
      val key = parseTrimmed(entry.before(colonIndex.z).trim)
      val value = parseTrimmed(entry.skip(colonIndex + 1).trim)
      (key, value)

  private def findTopLevelColon(input: Text): Int =
    val source = input.s
    var depth = 0
    var inSingleQuote = false
    var inDoubleQuote = false
    var index = 0
    var found = -1

    while index < source.length && found < 0 do
      val char = source.charAt(index)

      if inSingleQuote then
        if char == '\'' then inSingleQuote = false
      else if inDoubleQuote then
        val escaped = index > 0 && source.charAt(index - 1) == '\\'
        if char == '"' && !escaped then inDoubleQuote = false
      else
        char match
          case '['               => depth += 1
          case '{'               => depth += 1
          case ']'               => depth -= 1
          case '}'               => depth -= 1
          case '\''              => inSingleQuote = true
          case '"'               => inDoubleQuote = true
          case ':' if depth == 0 => found = index
          case _                 =>

      index += 1

    found

  private def splitFlowItems(input: Text): List[Text] =
    val source = input.s
    val items = scala.collection.mutable.ArrayBuffer[Text]()
    var depth = 0
    var inSingleQuote = false
    var inDoubleQuote = false
    var start = 0
    var index = 0

    while index < source.length do
      val char = source.charAt(index)

      if inSingleQuote then
        if char == '\'' then inSingleQuote = false
      else if inDoubleQuote then
        val escaped = index > 0 && source.charAt(index - 1) == '\\'
        if char == '"' && !escaped then inDoubleQuote = false
      else
        char match
          case '['  => depth += 1
          case '{'  => depth += 1
          case ']'  => depth -= 1
          case '}'  => depth -= 1
          case '\'' => inSingleQuote = true
          case '"'  => inDoubleQuote = true

          case ',' if depth == 0 =>
            items.append(input.segment(start.z till index.z))
            start = index + 1

          case _ =>

      index += 1

    items.append(input.skip(start))
    items.toList

  private def stripLineComment(line: Text): Text =
    val source = line.s
    var index = 0
    var inSingleQuote = false
    var inDoubleQuote = false
    var hashIndex = -1

    while index < source.length && hashIndex < 0 do
      val char = source.charAt(index)
      val precededByWhitespace = index == 0 || source.charAt(index - 1).isWhitespace

      if char == '\'' && !inDoubleQuote then inSingleQuote = !inSingleQuote
      else if char == '"' && !inSingleQuote then inDoubleQuote = !inDoubleQuote
      else if char == '#' && !inSingleQuote && !inDoubleQuote && precededByWhitespace
      then hashIndex = index

      index += 1

    if hashIndex < 0 then line else line.before(hashIndex.z)

  private def resolveScalar(input: Text): YamlAst =
    import YamlParser.*

    if input.nil || nullLiterals.contains(input) then YamlAst.Null
    else if trueLiterals.contains(input) then YamlAst.Bool(true)
    else if falseLiterals.contains(input) then YamlAst.Bool(false)
    else if positiveInfinityLiterals.contains(input) then YamlAst.Decimal(Double.PositiveInfinity)
    else if negativeInfinityLiterals.contains(input) then YamlAst.Decimal(Double.NegativeInfinity)
    else if nanLiterals.contains(input) then YamlAst.Decimal(Double.NaN)
    else parseInteger(input).orElse(parseDecimal(input)).getOrElse(YamlAst.Str(input))

  private def parseInteger(input: Text): Option[YamlAst.Integer] =
    if input.starts(t"0x") || input.starts(t"0X") then
      try Some(YamlAst.Integer(java.lang.Long.parseLong(input.skip(2).s, 16)))
      catch case _: NumberFormatException => None
    else if input.starts(t"0o") || input.starts(t"0O") then
      try Some(YamlAst.Integer(java.lang.Long.parseLong(input.skip(2).s, 8)))
      catch case _: NumberFormatException => None
    else
      try Some(YamlAst.Integer(java.lang.Long.parseLong(input.s)))
      catch case _: NumberFormatException => None

  private def parseDecimal(input: Text): Option[YamlAst.Decimal] =
    try Some(YamlAst.Decimal(java.lang.Double.parseDouble(input.s)))
    catch case _: NumberFormatException => None

  private def unescapeSingleQuoted(input: Text): Text =
    val source = input.s
    val builder = new StringBuilder
    var index = 0

    while index < source.length do
      val current = source.charAt(index)
      val nextIsQuote = index + 1 < source.length && source.charAt(index + 1) == '\''

      if current == '\'' && nextIsQuote then
        builder.append('\'')
        index += 2
      else if current == '\n' then
        index = foldLineBreaks(builder, source, index)
      else
        builder.append(current)
        index += 1

    builder.toString.tt

  private def unescapeDoubleQuoted(input: Text): Text =
    val source = input.s
    val builder = new StringBuilder
    var index = 0

    while index < source.length do
      val char = source.charAt(index)
      if char == '\\' && index + 1 < source.length then
        index += appendEscape(builder, source, index)
      else if char == '\n' then
        index = foldLineBreaks(builder, source, index)
      else
        builder.append(char)
        index += 1

    builder.toString.tt

  private def foldLineBreaks(builder: StringBuilder, source: String, start: Int): Int =
    var index = start
    var newlineCount = 0

    var done = false
    while !done && index < source.length do
      val char = source.charAt(index)
      if char == '\n' then
        newlineCount += 1
        index += 1
      else if char == ' ' || char == '\t' then
        index += 1
      else
        done = true

    if newlineCount == 1 then builder.append(' ')
    else
      var k = 1
      while k < newlineCount do
        builder.append('\n')
        k += 1

    index

  private def appendEscape(builder: StringBuilder, source: String, index: Int): Int =
    source.charAt(index + 1) match
      case '\\' => builder.append('\\')        ; 2
      case '"'  => builder.append('"')         ; 2
      case '\'' => builder.append('\'')        ; 2
      case '/'  => builder.append('/')         ; 2
      case ' '  => builder.append(' ')         ; 2
      case '0'  => builder.append(0x00.toChar) ; 2
      case 'a'  => builder.append(0x07.toChar) ; 2
      case 'b'  => builder.append('\b')        ; 2
      case 'e'  => builder.append(0x1b.toChar) ; 2
      case 'f'  => builder.append('\f')        ; 2
      case 'n'  => builder.append('\n')        ; 2
      case 'r'  => builder.append('\r')        ; 2
      case 't'  => builder.append('\t')        ; 2
      case 'v'  => builder.append(0x0b.toChar) ; 2
      case 'N'  => builder.append(0x85.toChar) ; 2
      case '_'  => builder.append(0xa0.toChar) ; 2
      case 'L'  => builder.append(0x2028.toChar)   ; 2
      case 'P'  => builder.append(0x2029.toChar)   ; 2

      case 'x' if index + 4 <= source.length =>
        val hex = source.substring(index + 2, index + 4).nn
        builder.append(java.lang.Integer.parseInt(hex, 16).toChar)
        4

      case 'u' if index + 6 <= source.length =>
        val hex = source.substring(index + 2, index + 6).nn
        builder.append(java.lang.Integer.parseInt(hex, 16).toChar)
        6

      case 'U' if index + 10 <= source.length =>
        val hex = source.substring(index + 2, index + 10).nn
        builder.append(Character.toChars(java.lang.Integer.parseInt(hex, 16)).nn)
        10

      case other =>
        builder.append('\\').append(other)
        2
