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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import vacuous.*
import zephyrine.*

// Parses a CSS Value Definition Syntax (VDS) string — a property's value grammar
// from the MDN dataset — into a `Syntax` tree. Combinator precedence, from
// loosest to tightest, is: `|`, `||`, `&&`, juxtaposition, then the multipliers
// (`?`, `*`, `+`, `{m,n}`, `#`, `!`) which bind to the preceding term.
private[cataclysm] object SyntaxParser:
  def parse(text: Text)(using Tactic[CssError]): Syntax =
    import zephyrine.lineation.linefeedChars

    Parser(Cursor[Text](text)).document()

  private class Parser(cursor: Cursor[Text, ?])(using Tactic[CssError]):
    def document(): Syntax =
      ws()
      val result = oneOf()
      ws()
      if !cursor.peek.isEnd then unexpected()
      result

    // ── primitives ──────────────────────────────────────────────────────────

    private def fail(reason: CssError.Reason): Nothing =
      abort(CssError(reason, cursor.line, cursor.column))

    private def unexpected(): Nothing =
      val datum = cursor.peek

      if datum.isEnd then fail(CssError.Reason.UnexpectedEnd)
      else fail(CssError.Reason.UnexpectedChar(datum.asInt.toChar))

    private def whitespaceChar(datum: Datum): Boolean =
      datum == ' ' || datum == '\t' || datum == '\n' || datum == '\r'

    private def ws(): Unit = while whitespaceChar(cursor.peek) do cursor.advance()

    private def eat(char: Char): Unit =
      if cursor.peek == char then cursor.advance() else unexpected()

    private def nameChar(char: Char): Boolean = char.isLetter || char.isDigit || char == '-'

    private def digit(datum: Datum): Boolean = !datum.isEnd && datum.asInt.toChar.isDigit

    // ── combinators, loosest to tightest ────────────────────────────────────

    private def oneOf(): Syntax =
      val acc = scala.collection.mutable.ListBuffer[Syntax](anyOf())
      ws()

      while cursor.peek == '|' do
        cursor.advance()
        ws()
        acc.append(anyOf())
        ws()

      if acc.size == 1 then acc.head else Syntax.OneOf(acc.toList)

    private def anyOf(): Syntax =
      val acc = scala.collection.mutable.ListBuffer[Syntax](allOf())
      ws()

      while pipePipe() do
        cursor.advance()
        eat('|')
        ws()
        acc.append(allOf())
        ws()

      if acc.size == 1 then acc.head else Syntax.AnyOf(acc.toList)

    private def allOf(): Syntax =
      val acc = scala.collection.mutable.ListBuffer[Syntax](sequence())
      ws()

      while cursor.peek == '&' do
        cursor.advance()
        eat('&')
        ws()
        acc.append(sequence())
        ws()

      if acc.size == 1 then acc.head else Syntax.AllOf(acc.toList)

    private def sequence(): Syntax =
      val acc = scala.collection.mutable.ListBuffer[Syntax]()
      ws()

      while termStart() do
        acc.append(term())
        ws()

      if acc.isEmpty then unexpected()
      if acc.size == 1 then acc.head else Syntax.Sequence(acc.toList)

    // A `|` that is not the start of a `||`.
    private def pipePipe(): Boolean =
      cursor.peek == '|' && cursor.lookahead { cursor.next(); cursor.peek == '|' }

    private def termStart(): Boolean =
      val datum = cursor.peek
      !(datum.isEnd || datum == '|' || datum == '&' || datum == ']' || datum == ')')

    // ── a term: a primary followed by zero or more multipliers ───────────────

    private def term(): Syntax =
      var result = primary()
      var continue = true

      while continue do
        val datum = cursor.peek

        if datum == '?' then result = once(result)
        else if datum == '*' then result = star(result)
        else if datum == '+' then result = plus(result)
        else if datum == '!' then result = mandatory(result)
        else if datum == '#' then result = hash(result)
        else if datum == '{' then result = braces(result)
        else continue = false

      result

    private def once(term: Syntax): Syntax =
      cursor.advance()
      Syntax.Repeated(term, 0, 1, false)

    private def star(term: Syntax): Syntax =
      cursor.advance()
      Syntax.Repeated(term, 0, Unset, false)

    private def plus(term: Syntax): Syntax =
      cursor.advance()
      Syntax.Repeated(term, 1, Unset, false)

    private def mandatory(term: Syntax): Syntax =
      cursor.advance()
      Syntax.Mandatory(term)

    private def hash(term: Syntax): Syntax =
      cursor.advance()

      if cursor.peek == '{' then
        val (min, max) = range()
        Syntax.Repeated(term, min, max, true)
      else
        Syntax.Repeated(term, 1, Unset, true)

    private def braces(term: Syntax): Syntax =
      val (min, max) = range()
      Syntax.Repeated(term, min, max, false)

    private def range(): (Int, Optional[Int]) =
      eat('{')
      val min = number()

      val max =
        if cursor.peek == ',' then
          cursor.advance()
          if digit(cursor.peek) then number() else Unset
        else
          min

      eat('}')
      (min, max)

    private def number(): Int =
      val buf = java.lang.StringBuilder()

      while digit(cursor.peek) do
        buf.append(cursor.peek.asInt.toChar)
        cursor.advance()

      if buf.length == 0 then unexpected()
      buf.toString.nn.toInt

    // ── primaries ────────────────────────────────────────────────────────────

    private def primary(): Syntax =
      val datum = cursor.peek

      if datum == '<' then angle()
      else if datum == '[' then group()
      else if datum == '\'' then quoted()
      else if datum == '/' then literal('/')
      else if datum == ',' then literal(',')
      else identOrFunction()

    private def literal(char: Char): Syntax =
      cursor.advance()
      Syntax.Literal(char.toString.nn.tt)

    private def group(): Syntax =
      cursor.advance()
      ws()
      val inner = oneOf()
      ws()
      eat(']')
      inner

    private def quoted(): Syntax =
      cursor.advance()
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && cursor.peek != '\'' do
        buf.append(cursor.peek.asInt.toChar)
        cursor.advance()

      eat('\'')
      Syntax.Literal(buf.toString.nn.tt)

    private def identOrFunction(): Syntax =
      val name = readName()

      if cursor.peek == '(' then
        cursor.advance()
        ws()
        val body = oneOf()
        ws()
        eat(')')
        Syntax.Function(name, body)
      else
        Syntax.Keyword(name)

    private def readName(): Text =
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && nameChar(cursor.peek.asInt.toChar) do
        buf.append(cursor.peek.asInt.toChar)
        cursor.advance()

      if buf.length == 0 then unexpected()
      buf.toString.nn.tt

    // ── `<type>`, `<type [bounds]>` and `<'property'>` ────────────────────────

    private def angle(): Syntax =
      cursor.advance()
      ws()
      if cursor.peek == '\'' then propertyRef() else typeRef()

    private def propertyRef(): Syntax =
      cursor.advance()
      val name = readUntil('\'')
      eat('\'')
      ws()
      eat('>')
      Syntax.Property(name)

    private def typeRef(): Syntax =
      val name = typeName()
      ws()
      val bounds = if cursor.peek == '[' then bracketBounds() else Unset
      ws()
      eat('>')
      Syntax.Type(name, bounds)

    private def typeBoundary(datum: Datum): Boolean =
      datum.isEnd || whitespaceChar(datum) || datum == '[' || datum == '>'

    private def typeName(): Text =
      val buf = java.lang.StringBuilder()
      var continue = true

      while continue do
        val datum = cursor.peek

        if typeBoundary(datum) then continue = false
        else
          buf.append(datum.asInt.toChar)
          cursor.advance()

      if buf.length == 0 then unexpected()
      buf.toString.nn.tt

    private def bracketBounds(): Text =
      cursor.advance()
      val text = readUntil(']')
      eat(']')
      text.trim

    private def readUntil(stop: Char): Text =
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && cursor.peek != stop do
        buf.append(cursor.peek.asInt.toChar)
        cursor.advance()

      buf.toString.nn.tt
