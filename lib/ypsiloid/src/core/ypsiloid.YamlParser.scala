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
    val cleaned = stripCommentsAndTrim(input.s)
    resolveScalar(cleaned)

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
