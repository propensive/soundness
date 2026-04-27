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
    val lines          = mutable.ArrayBuffer[IndexedSeq[Token]]()
    var line           = mutable.ArrayBuffer[Token]()
    var i              = 0
    var inBlock        = false
    var inTripleString = false

    inline def emit(tok: Token): Unit = line += tok

    inline def endLine(): Unit =
      lines += line.toIndexedSeq
      line = mutable.ArrayBuffer[Token]()

    while i < text.length do
      val c = text.charAt(i)

      if c == '\n' then
        endLine()
        i += 1
      else if inTripleString then
        val start = i
        var done = false
        while i < text.length && text.charAt(i) != '\n' && !done do
          if i + 2 < text.length && text.charAt(i) == '"'
            && text.charAt(i + 1) == '"' && text.charAt(i + 2) == '"'
          then
            i += 3
            done = true
            inTripleString = false
          else i += 1
        emit(Token(text.substring(start, i).nn, Kind.Strs))
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
        val (newPos, content, closed) = scanQuotedString(text, i, '"')
        emit(Token(content, Kind.Strs))
        i = newPos
        if !closed && content.startsWith("\"\"\"") then inTripleString = true
      else if c == '\'' then
        // A `'` is a char literal only in the precise forms `'X'`, `'\X'`,
        // `'\uXXXX'`. Otherwise it is Scala 3 macro syntax: a quote
        // (`'{ … }` / `' { … }` / `'[ … ]`) or a quoted reference
        // (`'ident`). In every non-char-literal case we just emit `'` as a
        // Code token.
        val isCharLiteral =
          (i + 2 < text.length && text.charAt(i + 2) == '\'')
            || (i + 3 < text.length && text.charAt(i + 1) == '\\'
                  && text.charAt(i + 3) == '\'')
            || (i + 7 < text.length && text.charAt(i + 1) == '\\'
                  && text.charAt(i + 2) == 'u' && text.charAt(i + 7) == '\'')
        if isCharLiteral then
          val (newPos, content, _) = scanQuotedString(text, i, '\'')
          emit(Token(content, Kind.Strs))
          i = newPos
        else
          emit(Token("'", Kind.Code))
          i += 1
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
          val (newPos, content, closed) = scanQuotedString(text, i, quote)
          emit(Token(ident + content, Kind.Strs))
          i = newPos
          if !closed && content.startsWith("\"\"\"") then inTripleString = true
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
  // Returns (positionAfterString, fullText including quotes, closedOnThisLine).
  // Handles escape sequences and string-interpolation blocks `${...}`. If the
  // string spans newlines, scanning stops at the newline; the caller is
  // responsible for setting tokenizer state accordingly.
  private def scanQuotedString(text: String, start: Int, quote: Char): (Int, String, Boolean) =
    val triple = quote == '"' && start + 2 < text.length
      && text.charAt(start + 1) == '"' && text.charAt(start + 2) == '"'
    var i = start + (if triple then 3 else 1)
    var done = false
    while i < text.length && text.charAt(i) != '\n' && !done do
      if triple && text.charAt(i) == '"' then
        // Per the Scala spec, a triple-quoted string is closed by the LAST
        // run of three or more consecutive `"`. Count the run, and if it has
        // length >= 3 treat the final three as the closer (any preceding `"`s
        // become part of the string content).
        var qcount = 0
        while i + qcount < text.length && text.charAt(i + qcount) == '"' do qcount += 1
        if qcount >= 3 then
          i += qcount
          done = true
        else i += qcount
      else if !triple && text.charAt(i) == quote then
        i += 1
        done = true
      else if text.charAt(i) == '$' && i + 1 < text.length && text.charAt(i + 1) == '{' then
        // Skip past the interpolation block, balancing nested `{`/`}`. Quoted
        // strings inside the block (which themselves can contain `{`/`}`) are
        // skipped via a recursive scan so they don't unbalance the count.
        i += 2
        var depth = 1
        while i < text.length && text.charAt(i) != '\n' && depth > 0 do
          val c = text.charAt(i)
          if c == '{' then { depth += 1; i += 1 }
          else if c == '}' then { depth -= 1; i += 1 }
          else if c == '"' || c == '\'' then
            val (newPos, _, _) = scanQuotedString(text, i, c)
            i = newPos
          else if c == '\\' && i + 1 < text.length && text.charAt(i + 1) != '\n' then
            i += 2
          else i += 1
      else if text.charAt(i) == '\\' && i + 1 < text.length && text.charAt(i + 1) != '\n' then
        i += 2
      else i += 1
    (i, text.substring(start, i).nn, done)
