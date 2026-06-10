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
import nomenclature.*
import vacuous.*
import zephyrine.*

// Parses a (comment-stripped, whitespace-normalized) selector prelude into a
// `SelectorList`, following Selectors Level 4. Strict: malformed input raises a
// `CssError`. Functional pseudo-classes `:is`/`:where`/`:not`/`:has` take nested
// selector lists, `:nth-*` take an `An+B` (with optional `of` selector list),
// and any other function keeps its argument as raw text.
private[cataclysm] object SelectorParser:
  def parse(text: Text)(using Tactic[CssError]): SelectorList =
    import zephyrine.lineation.linefeedChars

    Parser(Cursor[Text](text)).document()

  private class Parser(cursor: Cursor[Text])(using Tactic[CssError]):
    def document(): SelectorList =
      val list = selectorList(relative = false)
      ws()
      if !cursor.peek.isEnd then unexpected()
      list

    // ── primitives ──────────────────────────────────────────────────────────

    private def unexpected(): Nothing =
      val datum = cursor.peek

      if datum.isEnd then fail(CssError.Reason.UnexpectedEnd)
      else fail(CssError.Reason.UnexpectedChar(datum.asInt.toChar))

    private def fail(reason: CssError.Reason): Nothing =
      abort(CssError(reason, cursor.line, cursor.column))

    private def whitespaceChar(datum: Datum): Boolean =
      datum == ' ' || datum == '\t' || datum == '\n' || datum == '\r'

    private def ws(): Boolean =
      var any = false

      while whitespaceChar(cursor.peek) do
        cursor.advance()
        any = true

      any

    private def eat(char: Char): Unit =
      if cursor.peek == char then cursor.advance() else unexpected()

    private def identStart(char: Char): Boolean =
      char.isLetter || char == '_' || char == '-' || char.toInt >= 0x80

    private def identChar(char: Char): Boolean = identStart(char) || char.isDigit

    private def readIdent(): Text =
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && identChar(cursor.peek.asInt.toChar) do
        buf.append(cursor.peek.asInt.toChar)
        cursor.advance()

      if buf.length == 0 then unexpected()
      buf.toString.nn.tt

    // ── selector list / complex / compound ──────────────────────────────────

    private def selectorList(relative: Boolean): SelectorList =
      val acc = scala.collection.mutable.ListBuffer[Selector]()
      acc.append(complex(relative))
      var continue = true

      while continue do
        ws()

        if cursor.peek == ',' then
          cursor.advance()
          ws()
          acc.append(complex(relative))
        else
          continue = false

      SelectorList(acc.toList)

    private def complex(relative: Boolean): Selector =
      ws()
      val lead = if relative then leadingCombinator() else Unset
      ws()
      val head = compound()
      val rest = scala.collection.mutable.ListBuffer[(Combinator, Compound)]()
      var continue = true

      while continue do
        combinatorBetween() match
          case Unset =>
            continue = false

          case comb: Combinator =>
            rest.append((comb, compound()))

      Selector(lead, head, rest.toList)

    private def combinator(kind: Combinator): Combinator =
      cursor.advance()
      ws()
      kind

    private def column(): Combinator =
      cursor.advance()
      eat('|')
      ws()
      Combinator.Column

    private def leadingCombinator(): Optional[Combinator] =
      val datum = cursor.peek

      if datum == '>' then combinator(Combinator.Child)
      else if datum == '+' then combinator(Combinator.NextSibling)
      else if datum == '~' then combinator(Combinator.SubsequentSibling)
      else if datum == '|' then column()
      else Unset

    private def combinatorBetween(): Optional[Combinator] =
      val hadWs = ws()
      val datum = cursor.peek

      if datum.isEnd || datum == ',' || datum == ')' then Unset
      else if datum == '>' then combinator(Combinator.Child)
      else if datum == '+' then combinator(Combinator.NextSibling)
      else if datum == '~' then combinator(Combinator.SubsequentSibling)
      else if datum == '|' then column()
      else if hadWs then Combinator.Descendant
      else unexpected()

    private def compound(): Compound =
      val parts = scala.collection.mutable.ListBuffer[Simple]()

      typeOrUniversal() match
        case Unset          => ()
        case simple: Simple => parts.append(simple)

      var continue = true

      while continue do
        val datum = cursor.peek

        if datum == '#' then parts.append(id())
        else if datum == '.' then parts.append(cls())
        else if datum == '[' then parts.append(attribute())
        else if datum == '&' then nesting(parts)
        else if datum == ':' then parts.append(pseudo())
        else continue = false

      if parts.isEmpty then fail(CssError.Reason.EmptySelector)
      Compound(parts.toList)

    private def nesting(parts: scala.collection.mutable.ListBuffer[Simple]): Unit =
      cursor.advance()
      parts.append(Simple.Nesting)

    // ── type, universal and namespaces ──────────────────────────────────────

    private def typeOrUniversal(): Optional[Simple] =
      val datum = cursor.peek

      if datum == '|' then
        cursor.advance()
        finishType(Namespace.Default)
      else if datum == '*' then
        cursor.advance()

        if singlePipe() then
          cursor.advance()
          finishType(Namespace.Any)
        else
          Simple.Universal(Unset)
      else if !datum.isEnd && identStart(datum.asInt.toChar) then
        val name = readIdent()

        if singlePipe() then
          cursor.advance()
          finishType(Namespace.Named(name))
        else
          Simple.Type(Unset, name)
      else
        Unset

    private def finishType(namespace: Namespace): Simple =
      val datum = cursor.peek

      if datum == '*' then
        cursor.advance()
        Simple.Universal(namespace)
      else if !datum.isEnd && identStart(datum.asInt.toChar) then
        Simple.Type(namespace, readIdent())
      else
        unexpected()

    // True if the current char is a lone `|` (a namespace separator), not the
    // start of a `||` combinator or a `|=` attribute matcher.
    private def singlePipe(): Boolean =
      cursor.peek == '|' && cursor.lookahead { cursor.next(); pipeTail() }

    private def pipeTail(): Boolean =
      val datum = cursor.peek
      datum != '|' && datum != '='

    // ── simple selectors ────────────────────────────────────────────────────

    private def id(): Simple =
      cursor.advance()
      val text = readIdent()

      Simple.Id:
        safely(Name[DomId](text)).or:
          invalid(text)
          text.asInstanceOf[Name[DomId]]

    private def cls(): Simple =
      cursor.advance()
      val text = readIdent()

      Simple.Class:
        safely(Name[CssClass](text)).or:
          invalid(text)
          text.asInstanceOf[Name[CssClass]]

    // The parser is lenient: an invalid identifier (e.g. a class starting with a
    // digit) raises a `CssError` but parsing continues with the name verbatim.
    private def invalid(text: Text): Unit =
      raise(CssError(CssError.Reason.InvalidName(text), cursor.line, cursor.column))

    private def attribute(): Simple =
      eat('[')
      ws()
      val (namespace, name) = attrName()
      ws()

      if cursor.peek == ']' then
        cursor.advance()
        Simple.Attribute(namespace, name, Unset)
      else
        val test = AttributeTest(matcher(), attrAfterMatcher(), attrModifier())
        ws()
        eat(']')
        Simple.Attribute(namespace, name, test)

    private def attrAfterMatcher(): Text =
      ws()
      val value = attrValue()
      ws()
      value

    private def attrName(): (Optional[Namespace], Text) =
      val datum = cursor.peek

      if datum == '|' then
        cursor.advance()
        (Namespace.Default, readIdent())
      else if datum == '*' then
        cursor.advance()
        eat('|')
        (Namespace.Any, readIdent())
      else
        val name = readIdent()

        if singlePipe() then
          cursor.advance()
          (Namespace.Named(name), readIdent())
        else
          (Unset, name)

    private def simpleMatcher(kind: AttributeMatcher): AttributeMatcher =
      cursor.advance()
      kind

    private def pairedMatcher(kind: AttributeMatcher): AttributeMatcher =
      cursor.advance()
      eat('=')
      kind

    private def matcher(): AttributeMatcher =
      val datum = cursor.peek

      if datum == '=' then simpleMatcher(AttributeMatcher.Exact)
      else if datum == '~' then pairedMatcher(AttributeMatcher.Includes)
      else if datum == '|' then pairedMatcher(AttributeMatcher.DashMatch)
      else if datum == '^' then pairedMatcher(AttributeMatcher.Prefix)
      else if datum == '$' then pairedMatcher(AttributeMatcher.Suffix)
      else if datum == '*' then pairedMatcher(AttributeMatcher.Substring)
      else unexpected()

    private def attrValue(): Text =
      val datum = cursor.peek

      if datum == '"' || datum == '\'' then
        val quote = datum.asInt.toChar
        val buf = java.lang.StringBuilder()
        buf.append(quote)
        cursor.advance()
        readString(quote, buf)
        buf.toString.nn.tt
      else
        readIdent()

    private def attrModifier(): Optional[Char] =
      val datum = cursor.peek

      if datum == 'i' || datum == 'I' || datum == 's' || datum == 'S' then
        val char = datum.asInt.toChar
        cursor.advance()
        char
      else
        Unset

    // ── pseudo-classes and pseudo-elements ──────────────────────────────────

    private def pseudo(): Simple =
      cursor.advance()
      val element = cursor.peek == ':'
      if element then cursor.advance()
      val name = readIdent()

      if cursor.peek == '(' then functionalPseudo(name, element)
      else if element then Simple.PseudoElement(name, Unset)
      else Simple.PseudoClass(name, Unset)

    private def functionalPseudo(name: Text, element: Boolean): Simple =
      cursor.advance()
      ws()
      val argument = pseudoArgument(name)
      ws()
      eat(')')

      if element then Simple.PseudoElement(name, argument)
      else Simple.PseudoClass(name, argument)

    private def pseudoArgument(name: Text): PseudoArgument =
      val key: String = name.s.toLowerCase.nn

      key match
        case "is" | "where" | "not" | "matches" =>
          PseudoArgument.Selectors(selectorList(relative = false))

        case "has" =>
          PseudoArgument.Selectors(selectorList(relative = true))

        case "nth-child" | "nth-last-child" | "nth-of-type" =>
          nth()

        case "nth-last-of-type" | "nth-col" | "nth-last-col" =>
          nth()

        case _ =>
          PseudoArgument.Raw(rawArgument())

    private def nth(): PseudoArgument =
      ws()
      val (a, b) = anPlusB()
      ws()
      val of = if keyword(t"of") then nthOf() else Unset
      PseudoArgument.Nth(a, b, of)

    private def nthOf(): SelectorList =
      ws()
      selectorList(relative = false)

    private def anPlusB(): (Int, Int) =
      if keyword(t"odd") then
        (2, 1)
      else if keyword(t"even") then
        (2, 0)
      else
        val sign = signOpt()
        val digits = digitsOpt()
        val datum = cursor.peek

        if datum == 'n' || datum == 'N' then
          nTerm(sign, digits)
        else
          digits.lay(unexpected()): value =>
            (0, sign*value)

    private def nTerm(sign: Int, digits: Optional[Int]): (Int, Int) =
      cursor.advance()
      val a = sign*digits.or(1)
      ws()
      val datum = cursor.peek

      if datum == '+' || datum == '-' then
        cursor.advance()
        ws()
        (a, (if datum == '-' then -1 else 1)*digitsRequired())
      else
        (a, 0)

    private def signOpt(): Int =
      val datum = cursor.peek

      if datum == '+' then advanceValue(1)
      else if datum == '-' then advanceValue(-1)
      else 1

    private def advanceValue(value: Int): Int =
      cursor.advance()
      value

    private def digitsOpt(): Optional[Int] =
      val buf = java.lang.StringBuilder()

      while !cursor.peek.isEnd && cursor.peek.asInt.toChar.isDigit do
        buf.append(cursor.peek.asInt.toChar)
        cursor.advance()

      if buf.length == 0 then Unset else buf.toString.nn.toInt

    private def digitsRequired(): Int = digitsOpt().lay(unexpected())(identity)

    // ── keyword matching ─────────────────────────────────────────────────────

    // Match `word` case-insensitively at the cursor (with an identifier
    // boundary after it) and consume it on success.
    private def keyword(word: Text): Boolean =
      val text = word.s
      val matched = cursor.lookahead(matchesKeyword(text))

      if matched then consume(text.length)
      matched

    private def matchesKeyword(text: String): Boolean =
      var ok = true
      var index = 0

      while ok && index < text.length do
        val datum = cursor.peek

        if datum.isEnd then
          ok = false
        else if datum.asInt.toChar.toLower != text.charAt(index).toLower then
          ok = false
        else
          cursor.next()
          index += 1

      ok && boundary()

    private def boundary(): Boolean =
      val datum = cursor.peek
      datum.isEnd || !identChar(datum.asInt.toChar)

    private def consume(count: Int): Unit =
      var index = 0

      while index < count do
        cursor.advance()
        index += 1

    // ── raw functional argument and strings ──────────────────────────────────

    private def rawArgument(): Text =
      val buf = java.lang.StringBuilder()
      var depth = 0
      var continue = true

      while continue do
        val datum = cursor.peek

        if datum.isEnd then
          unexpected()
        else if datum == ')' && depth == 0 then
          continue = false
        else
          val char = datum.asInt.toChar

          if char == '(' || char == '[' then depth += 1
          else if char == ')' || char == ']' then depth -= 1

          if char == '"' || char == '\'' then
            buf.append(char)
            cursor.advance()
            readString(char, buf)
          else
            buf.append(char)
            cursor.advance()

      buf.toString.nn.tt.trim

    private def readString(quote: Char, buf: java.lang.StringBuilder): Unit =
      var continue = true

      while continue do
        val datum = cursor.peek

        if datum.isEnd then
          fail(CssError.Reason.UnterminatedString)
        else
          val char = datum.asInt.toChar

          if char == '\\' then
            escapeChar(buf)
          else if char == quote then
            buf.append(quote)
            cursor.advance()
            continue = false
          else
            buf.append(char)
            cursor.advance()

    private def escapeChar(buf: java.lang.StringBuilder): Unit =
      buf.append('\\')
      cursor.advance()
      val datum = cursor.peek

      if datum.isEnd then
        fail(CssError.Reason.UnterminatedString)
      else
        buf.append(datum.asInt.toChar)
        cursor.advance()
 
