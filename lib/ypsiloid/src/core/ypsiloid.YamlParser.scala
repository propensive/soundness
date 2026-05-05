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

object YamlParser:
  def parse(input: Text): YamlAst =
    parseTrimmed(stripCommentsAndTrim(input.s))

  private def parseTrimmed(input: String): YamlAst =
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
      else resolveScalar(input)

  private def parseFlowSequence(body: String): YamlAst =
    if body.trim.nn.isEmpty then YamlAst.Sequence(IArray.empty)
    else YamlAst.Sequence(IArray.from(splitFlowItems(body).map(item => parseTrimmed(item.trim.nn))))

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

  private def stripCommentsAndTrim(input: String): String =
    val lines = input.split("\n", -1).nn
    val stripped = lines.iterator.map(line => stripLineComment(line.nn))
    stripped.filter(_.trim.nn.nonEmpty).mkString("\n").trim.nn

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
