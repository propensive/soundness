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
package decorum

import scala.collection.mutable

object Tokenizer:
  def tokenize(text: String): IndexedSeq[IndexedSeq[Token]] =
    val lines  = mutable.ArrayBuffer[IndexedSeq[Token]]()
    var line   = mutable.ArrayBuffer[Token]()
    var i      = 0
    var inBlock = false

    inline def emit(tok: Token): Unit = line += tok
    inline def endLine(): Unit =
      lines += line.toIndexedSeq
      line = mutable.ArrayBuffer[Token]()

    while i < text.length do
      val c = text.charAt(i)

      if c == '\n' then
        endLine()
        i += 1
      else if inBlock then
        val start = i
        var done = false
        while i < text.length && text.charAt(i) != '\n' && !done do
          if i + 1 < text.length && text.charAt(i) == '*' && text.charAt(i + 1) == '/' then
            i += 2
            inBlock = false
            done = true
          else i += 1
        emit(Token(text.substring(start, i).nn, Kind.Comment))
      else if c == '/' && i + 1 < text.length && text.charAt(i + 1) == '/' then
        val start = i
        while i < text.length && text.charAt(i) != '\n' do i += 1
        emit(Token(text.substring(start, i).nn, Kind.Comment))
      else if c == '/' && i + 1 < text.length && text.charAt(i + 1) == '*' then
        val start = i
        i += 2
        inBlock = true
        var done = false
        while i < text.length && text.charAt(i) != '\n' && !done do
          if i + 1 < text.length && text.charAt(i) == '*' && text.charAt(i + 1) == '/' then
            i += 2
            inBlock = false
            done = true
          else i += 1
        emit(Token(text.substring(start, i).nn, Kind.Comment))
      else if c == '"' then
        scanQuotedString(text, i, '"') match
          case (newPos, content) =>
            emit(Token(content, Kind.Strs))
            i = newPos
      else if c == '\'' then
        scanQuotedString(text, i, '\'') match
          case (newPos, content) =>
            emit(Token(content, Kind.Strs))
            i = newPos
      else if c == '`' then
        val start = i
        i += 1
        while i < text.length && text.charAt(i) != '`' && text.charAt(i) != '\n' do i += 1
        if i < text.length && text.charAt(i) == '`' then i += 1
        emit(Token(text.substring(start, i).nn, Kind.Code))
      else if c == ' ' || c == '\t' then
        val start = i
        while i < text.length && (text.charAt(i) == ' ' || text.charAt(i) == '\t') do i += 1
        emit(Token(text.substring(start, i).nn, Kind.Space))
      else if isIdentStart(c) then
        val start = i
        while i < text.length && isIdentPart(text.charAt(i)) do i += 1
        val ident: String = text.substring(start, i).nn
        if i < text.length && (text.charAt(i) == '"' || text.charAt(i) == '\'') then
          val quote = text.charAt(i)
          val (newPos, content) = scanQuotedString(text, i, quote)
          emit(Token(ident + content, Kind.Strs))
          i = newPos
        else emit(Token(ident, Kind.Code))
      else if c.isDigit then
        val start = i
        while i < text.length && (text.charAt(i).isLetterOrDigit || text.charAt(i) == '.') do i += 1
        emit(Token(text.substring(start, i).nn, Kind.Code))
      else if isOpChar(c) then
        val start = i
        while i < text.length && isOpChar(text.charAt(i)) do i += 1
        emit(Token(text.substring(start, i).nn, Kind.Code))
      else
        emit(Token(c.toString, Kind.Code))
        i += 1

    lines += line.toIndexedSeq
    lines.toIndexedSeq

  private def isIdentStart(c: Char): Boolean = c.isLetter || c == '_'
  private def isIdentPart(c: Char): Boolean  = c.isLetterOrDigit || c == '_'

  private def isOpChar(c: Char): Boolean = c match
    case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
    case '<' | '>' | '=' | '!' | '?' | ':'                   => true
    case _                                                   => false

  // Scans a quoted string starting at position `start` (which is the opening quote).
  // Returns (positionAfterString, fullText including quotes).
  // Handles escape sequences. If the string spans newlines, scanning stops at the
  // newline and the caller resumes on the next line as Code (a small inaccuracy
  // for triple-quoted multi-line strings, but sufficient for our checks).
  private def scanQuotedString(text: String, start: Int, quote: Char): (Int, String) =
    val triple = quote == '"' && start + 2 < text.length
                   && text.charAt(start + 1) == '"' && text.charAt(start + 2) == '"'
    var i = start + (if triple then 3 else 1)
    var done = false
    while i < text.length && text.charAt(i) != '\n' && !done do
      if triple
        && i + 2 < text.length && text.charAt(i) == '"'
        && text.charAt(i + 1) == '"' && text.charAt(i + 2) == '"'
      then
        i += 3
        done = true
      else if !triple && text.charAt(i) == quote then
        i += 1
        done = true
      else if text.charAt(i) == '\\' && i + 1 < text.length && text.charAt(i + 1) != '\n' then
        i += 2
      else i += 1
    (i, text.substring(start, i).nn)
