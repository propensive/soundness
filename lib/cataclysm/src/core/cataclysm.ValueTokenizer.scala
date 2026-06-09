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
package cataclysm

import anticipation.*
import contingency.*
import gossamer.*
import zephyrine.*

// Breaks a CSS property value into `ValueToken`s, following the CSS Syntax
// Module Level 3 tokenizer for the subset that appears in values. The output
// feeds the value matcher; it is deliberately lenient, raising a `CssError`
// only for an unterminated string.
private[cataclysm] object ValueTokenizer:
  def tokens(text: Text)(using Tactic[CssError]): List[ValueToken] =
    import zephyrine.lineation.linefeedChars

    Tokenizer(Cursor[Text](text)).all()

  private class Tokenizer(cursor: Cursor[Text])(using Tactic[CssError]):
    def all(): List[ValueToken] =
      val acc = scala.collection.mutable.ListBuffer[ValueToken]()
      while !cursor.peek.isEnd do acc.append(next())
      acc.toList

    // ── primitives ──────────────────────────────────────────────────────────

    private def fail(reason: CssError.Reason): Nothing =
      abort(CssError(reason, cursor.line, cursor.column))

    private def whitespaceChar(char: Char): Boolean =
      char == ' ' || char == '\t' || char == '\n' || char == '\r'

    private def identStart(char: Char): Boolean = char.isLetter || char == '_' || char.toInt >= 0x80
    private def identChar(char: Char): Boolean = identStart(char) || char.isDigit || char == '-'
    private def digit(datum: Datum): Boolean = !datum.isEnd && datum.asInt.toChar.isDigit
    private def peekChar: Char = cursor.peek.asInt.toChar
    private def peek2: Datum = cursor.lookahead { cursor.next(); cursor.peek }
    private def peek3: Datum = cursor.lookahead { cursor.next(); cursor.next(); cursor.peek }

    // ── dispatch ──────────────────────────────────────────────────────────────

    private def next(): ValueToken =
      val char = peekChar

      if whitespaceChar(char) then whitespace()
      else if char == '"' || char == '\'' then string(char)
      else if char == '#' then hash()
      else if char == ',' then single(ValueToken.Comma)
      else if char == '(' then single(ValueToken.Open)
      else if char == ')' then single(ValueToken.Close)
      else if numberStart(char) then number()
      else if identStart(char) then identOrFunction()
      else if char == '-' && identAhead() then identOrFunction()
      else delim(char)

    private def numberStart(char: Char): Boolean =
      if char.isDigit then true
      else if char == '.' then digit(peek2)
      else if char == '+' || char == '-' then digit(peek2) || (peek2 == '.' && digit(peek3))
      else false

    private def identAhead(): Boolean =
      val datum = peek2
      !datum.isEnd && (identStart(datum.asInt.toChar) || datum == '-')

    private def single(token: ValueToken): ValueToken =
      cursor.advance()
      token

    private def delim(char: Char): ValueToken =
      cursor.advance()
      ValueToken.Delim(char)

    // ── whitespace, strings and hashes ────────────────────────────────────────

    private def whitespace(): ValueToken =
      while !cursor.peek.isEnd && whitespaceChar(peekChar) do cursor.advance()
      ValueToken.Whitespace

    private def string(quote: Char): ValueToken =
      cursor.advance()
      val buf = java.lang.StringBuilder()
      var continue = true

      while continue do
        val datum = cursor.peek

        if datum.isEnd then
          fail(CssError.Reason.UnterminatedString)
        else
          val char = datum.asInt.toChar

          if char == '\\' then
            cursor.advance()
            val escaped = cursor.peek

            if escaped.isEnd then fail(CssError.Reason.UnterminatedString)
            else
              buf.append(escaped.asInt.toChar)
              cursor.advance()
          else if char == quote then
            cursor.advance()
            continue = false
          else
            buf.append(char)
            cursor.advance()

      ValueToken.Quoted(buf.toString.nn.tt)

    private def hash(): ValueToken =
      cursor.advance()
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && identChar(peekChar) do
        buf.append(peekChar)
        cursor.advance()

      ValueToken.Hash(buf.toString.nn.tt)

    // ── numbers, percentages and dimensions ───────────────────────────────────

    private def number(): ValueToken =
      val buf = java.lang.StringBuilder()

      if cursor.peek == '+' || cursor.peek == '-' then
        buf.append(peekChar)
        cursor.advance()

      while digit(cursor.peek) do
        buf.append(peekChar)
        cursor.advance()

      if cursor.peek == '.' && digit(peek2) then
        buf.append('.')
        cursor.advance()

        while digit(cursor.peek) do
          buf.append(peekChar)
          cursor.advance()

      if (cursor.peek == 'e' || cursor.peek == 'E') && exponentAhead() then
        buf.append(peekChar)
        cursor.advance()

        if cursor.peek == '+' || cursor.peek == '-' then
          buf.append(peekChar)
          cursor.advance()

        while digit(cursor.peek) do
          buf.append(peekChar)
          cursor.advance()

      classify(buf.toString.nn)

    private def exponentAhead(): Boolean =
      digit(peek2) || ((peek2 == '+' || peek2 == '-') && digit(peek3))

    private def classify(raw: String): ValueToken =
      if cursor.peek == '%' then
        cursor.advance()
        ValueToken.Percentage(raw.toDouble, (raw+"%").tt)
      else if !cursor.peek.isEnd && identStart(peekChar) then
        val unit = readName()
        ValueToken.Dimension(raw.toDouble, unit, (raw+unit.s).tt)
      else
        val integer = !raw.contains(".") && !raw.contains("e") && !raw.contains("E")
        ValueToken.Number(raw.toDouble, integer, raw.tt)

    // ── identifiers, functions and urls ───────────────────────────────────────

    private def readName(): Text =
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && identChar(peekChar) do
        buf.append(peekChar)
        cursor.advance()

      buf.toString.nn.tt

    private def identOrFunction(): ValueToken =
      val name = readName()

      if cursor.peek == '(' then
        if name.s.toLowerCase.nn == "url" then url()
        else
          cursor.advance()
          ValueToken.Function(name)
      else
        ValueToken.Ident(name)

    private def url(): ValueToken =
      cursor.advance()
      skipSpaces()

      if cursor.peek == '"' || cursor.peek == '\'' then ValueToken.Function(t"url")
      else
        val buf = java.lang.StringBuilder()

        while !cursor.peek.isEnd && cursor.peek != ')' && !whitespaceChar(peekChar) do
          buf.append(peekChar)
          cursor.advance()

        skipSpaces()
        if cursor.peek == ')' then cursor.advance()
        ValueToken.Url(buf.toString.nn.tt)

    private def skipSpaces(): Unit =
      while !cursor.peek.isEnd && whitespaceChar(peekChar) do cursor.advance()
