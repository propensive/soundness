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
import rudiments.*

import YamlError.Reason

object YamlParser:
  private case class Line(indent: Int, content: String)

  def parse(input: Text)(using Tactic[YamlError]): YamlAst = new YamlParser().parse(input)

class YamlParser(using Tactic[YamlError]):
  import YamlParser.Line

  private val anchors = scala.collection.mutable.Map.empty[String, YamlAst]

  def parse(input: Text): YamlAst =
    val lines = preprocess(input.s)
    if lines.isEmpty then YamlAst.Null else parseBlock(lines)

  private def preprocess(input: String): List[Line] =
    val results = scala.collection.mutable.ArrayBuffer[Line]()

    for rawLine <- input.split("\n", -1).nn do
      val line = stripLineComment(rawLine.nn)
      var end = line.length
      while end > 0 && (line.charAt(end - 1) == ' ' || line.charAt(end - 1) == '\t') do
        end -= 1
      val rightTrimmed = line.substring(0, end).nn

      var indent = 0
      while indent < rightTrimmed.length && rightTrimmed.charAt(indent) == ' ' do indent += 1

      if indent < rightTrimmed.length
      then results.append(Line(indent, rightTrimmed.substring(indent).nn))

    results.toList

  private def parseBlock(lines: List[Line]): YamlAst =
    if lines.isEmpty then YamlAst.Null
    else
      val head = lines.head

      if isBareAnchor(head.content) then
        val name = head.content.substring(1).nn
        val value = if lines.tail.isEmpty then YamlAst.Null else parseBlock(lines.tail)
        anchors.update(name, value)
        value
      else if isBlockSequenceItem(head.content) then
        parseBlockSequence(lines, head.indent)
      else if isBlockMappingHead(head.content) then
        parseBlockMapping(lines, head.indent)
      else if isBlockScalarIndicator(head.content) && lines.tail.nonEmpty then
        parseBlockScalar(head.content, lines.tail)
      else if lines.lengthCompare(1) == 0 then
        parseTrimmed(head.content)
      else
        parseTrimmed(lines.map(_.content).mkString(" ").trim.nn)

  private def isBareAnchor(content: String): Boolean =
    content.length >= 2 && content.charAt(0) == '&' && !content.contains(" ")

  private def isBlockSequenceItem(content: String): Boolean =
    content == "-" || content.startsWith("- ")

  private def isBlockMappingHead(content: String): Boolean =
    val colon = findTopLevelColon(content)
    colon >= 0 && (colon == content.length - 1 || content.charAt(colon + 1) == ' ')

  private def isBlockScalarIndicator(content: String): Boolean = content match
    case "|" | ">" | "|-" | "|+" | ">-" | ">+" => true
    case _                                     => false

  private def parseBlockScalar(indicator: String, lines: List[Line]): YamlAst =
    if lines.isEmpty then YamlAst.Str(Text(""))
    else
      val style = indicator.charAt(0)

      val chomp =
        if indicator.length > 1 then indicator.charAt(1) else 'c'

      val baseIndent = lines.head.indent
      val builder = new StringBuilder

      lines.zipWithIndex.foreach: (line, position) =>
        val padding = (line.indent - baseIndent).max(0)
        if position > 0 then builder.append(if style == '|' then '\n' else ' ')
        var space = 0
        while space < padding do
          builder.append(' ')
          space += 1
        builder.append(line.content)

      chomp match
        case '-' => ()
        case '+' => builder.append('\n')
        case _   => builder.append('\n')

      YamlAst.Str(Text(builder.toString))

  private def parseBlockSequence(lines: List[Line], baseIndent: Int): YamlAst =
    val items = scala.collection.mutable.ArrayBuffer[YamlAst]()
    val rest = scala.collection.mutable.ArrayBuffer[Line]().addAll(lines)

    def headIsItem: Boolean =
      rest.nonEmpty && rest.head.indent == baseIndent && isBlockSequenceItem(rest.head.content)

    while headIsItem do
      val first = rest.remove(0)
      val itemLines = scala.collection.mutable.ArrayBuffer[Line]()
      val firstContent = if first.content == "-" then "" else first.content.substring(2).nn

      if firstContent.nonEmpty then itemLines.append(Line(0, firstContent))

      while rest.nonEmpty && rest.head.indent > baseIndent do
        val continuation = rest.remove(0)
        itemLines.append(Line(continuation.indent - baseIndent - 2, continuation.content))

      items.append(parseBlock(itemLines.toList))

    YamlAst.Sequence(IArray.from(items))

  private def parseBlockMapping(lines: List[Line], baseIndent: Int): YamlAst =
    val entries = scala.collection.mutable.ArrayBuffer[(YamlAst, YamlAst)]()
    val rest = scala.collection.mutable.ArrayBuffer[Line]().addAll(lines)

    def headIsEntry: Boolean =
      rest.nonEmpty && rest.head.indent == baseIndent && isBlockMappingHead(rest.head.content)

    while headIsEntry do
      val line = rest.remove(0)
      val colon = findTopLevelColon(line.content)
      val keyText = line.content.substring(0, colon).nn.trim.nn

      val inline =
        if colon + 1 < line.content.length then line.content.substring(colon + 1).nn.trim.nn
        else ""

      val key = parseTrimmed(keyText)

      val bareAnchorWithBlock =
        isBareAnchor(inline) && rest.nonEmpty && rest.head.indent > baseIndent

      if bareAnchorWithBlock then
        val name = inline.substring(1).nn
        val valueLines = scala.collection.mutable.ArrayBuffer[Line]()
        while rest.nonEmpty && rest.head.indent > baseIndent do
          valueLines.append(rest.remove(0))
        val value = parseBlock(valueLines.toList)
        anchors.update(name, value)
        entries.append((key, value))
      else if inline.nonEmpty && !isBlockScalarIndicator(inline) then
        entries.append((key, parseTrimmed(inline)))
      else
        val valueLines = scala.collection.mutable.ArrayBuffer[Line]()
        while rest.nonEmpty && rest.head.indent > baseIndent do
          valueLines.append(rest.remove(0))

        val value =
          if inline.isEmpty then parseBlock(valueLines.toList)
          else parseBlockScalar(inline, valueLines.toList)

        entries.append((key, value))

    YamlAst.Mapping(IArray.from(entries))

  private def parseTrimmed(input: String): YamlAst =
    if input.isEmpty then YamlAst.Null
    else if input.charAt(0) == '*' then resolveAlias(input.substring(1).nn.trim.nn)
    else if input.charAt(0) == '&' then
      val spaceIndex = input.indexOf(' ')

      if spaceIndex < 0 then
        val name = input.substring(1).nn
        anchors.update(name, YamlAst.Null)
        YamlAst.Null
      else
        val name = input.substring(1, spaceIndex).nn
        val value = parseTrimmedCore(input.substring(spaceIndex + 1).nn.trim.nn)
        anchors.update(name, value)
        value
    else
      parseTrimmedCore(input)

  private def resolveAlias(name: String): YamlAst =
    anchors.get(name) match
      case Some(value) => value

      case None =>
        raise(YamlError(Reason.UnknownAlias(Text(name)))) yet YamlAst.Null

  private def parseTrimmedCore(input: String): YamlAst =
    val length = input.length

    if length == 0 then YamlAst.Null
    else
      val first = input.charAt(0)
      val last = input.charAt(length - 1)

      if length >= 2 && first == '"' && last == '"'
      then YamlAst.Str(unescapeDoubleQuoted(input.substring(1, length - 1).nn))
      else if length >= 2 && first == '\'' && last == '\''
      then YamlAst.Str(unescapeSingleQuoted(input.substring(1, length - 1).nn))
      else if length >= 2 && first == '[' && last == ']'
      then parseFlowSequence(input.substring(1, length - 1).nn)
      else if length >= 2 && first == '{' && last == '}'
      then parseFlowMapping(input.substring(1, length - 1).nn)
      else resolveScalar(input)

  private def parseFlowSequence(body: String): YamlAst =
    if body.trim.nn.isEmpty then YamlAst.Sequence(IArray.empty)
    else YamlAst.Sequence(IArray.from(splitFlowItems(body).map(item => parseTrimmed(item.trim.nn))))

  private def parseFlowMapping(body: String): YamlAst =
    if body.trim.nn.isEmpty then YamlAst.Mapping(IArray.empty)
    else
      val entries = splitFlowItems(body).map(item => parseFlowMappingEntry(item.trim.nn))
      YamlAst.Mapping(IArray.from(entries))

  private def parseFlowMappingEntry(entry: String): (YamlAst, YamlAst) =
    val colonIndex = findTopLevelColon(entry)

    if colonIndex < 0 then (parseTrimmed(entry), YamlAst.Null)
    else
      val key = parseTrimmed(entry.substring(0, colonIndex).nn.trim.nn)
      val value = parseTrimmed(entry.substring(colonIndex + 1).nn.trim.nn)
      (key, value)

  private def findTopLevelColon(input: String): Int =
    var depth = 0
    var inSingleQuote = false
    var inDoubleQuote = false
    var index = 0
    var found = -1

    while index < input.length && found < 0 do
      val char = input.charAt(index)

      if inSingleQuote then
        if char == '\'' then inSingleQuote = false
      else if inDoubleQuote then
        val escaped = index > 0 && input.charAt(index - 1) == '\\'
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

  private def splitFlowItems(input: String): List[String] =
    val items = scala.collection.mutable.ArrayBuffer[String]()
    var depth = 0
    var inSingleQuote = false
    var inDoubleQuote = false
    var start = 0
    var index = 0

    while index < input.length do
      val char = input.charAt(index)

      if inSingleQuote then
        if char == '\'' then inSingleQuote = false
      else if inDoubleQuote then
        val escaped = index > 0 && input.charAt(index - 1) == '\\'
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
            items.append(input.substring(start, index).nn)
            start = index + 1

          case _ =>

      index += 1

    items.append(input.substring(start).nn)
    items.toList

  private def stripLineComment(line: String): String =
    var index = 0
    var inSingleQuote = false
    var inDoubleQuote = false
    var hashIndex = -1
    while index < line.length && hashIndex < 0 do
      val char = line.charAt(index)
      val precededByWhitespace = index == 0 || line.charAt(index - 1).isWhitespace
      if char == '\'' && !inDoubleQuote then inSingleQuote = !inSingleQuote
      else if char == '"' && !inSingleQuote then inDoubleQuote = !inDoubleQuote
      else if char == '#' && !inSingleQuote && !inDoubleQuote && precededByWhitespace
      then hashIndex = index
      index += 1
    if hashIndex < 0 then line else line.substring(0, hashIndex).nn

  private def resolveScalar(input: String): YamlAst =
    if input.isEmpty then YamlAst.Null
    else input match
      case "null" | "Null" | "NULL" | "~" => YamlAst.Null
      case "true" | "True" | "TRUE"       => YamlAst.Bool(true)
      case "false" | "False" | "FALSE"    => YamlAst.Bool(false)
      case ".inf" | ".Inf" | ".INF"       => YamlAst.Decimal(Double.PositiveInfinity)
      case "-.inf" | "-.Inf" | "-.INF"    => YamlAst.Decimal(Double.NegativeInfinity)
      case "+.inf" | "+.Inf" | "+.INF"    => YamlAst.Decimal(Double.PositiveInfinity)
      case ".nan" | ".NaN" | ".NAN"       => YamlAst.Decimal(Double.NaN)

      case _ =>
        parseInteger(input).orElse(parseDecimal(input)).getOrElse(YamlAst.Str(Text(input)))

  private def parseInteger(input: String): Option[YamlAst.Integer] =
    if input.startsWith("0x") || input.startsWith("0X") then
      try Some(YamlAst.Integer(java.lang.Long.parseLong(input.substring(2), 16)))
      catch case _: NumberFormatException => None
    else if input.startsWith("0o") || input.startsWith("0O") then
      try Some(YamlAst.Integer(java.lang.Long.parseLong(input.substring(2), 8)))
      catch case _: NumberFormatException => None
    else
      try Some(YamlAst.Integer(java.lang.Long.parseLong(input)))
      catch case _: NumberFormatException => None

  private def parseDecimal(input: String): Option[YamlAst.Decimal] =
    try Some(YamlAst.Decimal(java.lang.Double.parseDouble(input)))
    catch case _: NumberFormatException => None

  private def unescapeSingleQuoted(input: String): Text =
    val builder = new StringBuilder
    var index = 0

    while index < input.length do
      val current = input.charAt(index)
      val nextIsQuote = index + 1 < input.length && input.charAt(index + 1) == '\''

      if current == '\'' && nextIsQuote then
        builder.append('\'')
        index += 2
      else
        builder.append(current)
        index += 1

    Text(builder.toString)

  private def unescapeDoubleQuoted(input: String): Text =
    val builder = new StringBuilder
    var index = 0
    while index < input.length do
      val char = input.charAt(index)
      if char != '\\' || index + 1 >= input.length then
        builder.append(char)
        index += 1
      else
        index += appendEscape(builder, input, index)
    Text(builder.toString)

  private def appendEscape(builder: StringBuilder, input: String, index: Int): Int =
    input.charAt(index + 1) match
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

      case 'x' if index + 4 <= input.length =>
        val hex = input.substring(index + 2, index + 4)
        builder.append(java.lang.Integer.parseInt(hex, 16).toChar)
        4

      case 'u' if index + 6 <= input.length =>
        val hex = input.substring(index + 2, index + 6)
        builder.append(java.lang.Integer.parseInt(hex, 16).toChar)
        6

      case 'U' if index + 10 <= input.length =>
        val hex = input.substring(index + 2, index + 10)
        builder.append(Character.toChars(java.lang.Integer.parseInt(hex, 16)).nn)
        10

      case other =>
        builder.append('\\').append(other)
        2
