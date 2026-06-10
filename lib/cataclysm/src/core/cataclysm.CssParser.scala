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
import vacuous.*
import zephyrine.*

import Css.Node

// A small, lenient recursive-descent CSS reader built on a `zephyrine.Cursor`.
// Selectors, at-rule preludes and property values are kept as raw (normalized)
// `Text` — comments are stripped and runs of whitespace are collapsed to a
// single space, then the result is trimmed. Nested rules are supported: a rule
// body is itself a list of nodes.
private[cataclysm] object CssParser:
  def parse(input: Iterator[Text], validating: Boolean = true)(using Tactic[CssError]): Css =
    import zephyrine.lineation.linefeedChars

    Parser(Cursor[Text](input), validating).document()

  private class Parser(cursor: Cursor[Text], validating: Boolean)(using Tactic[CssError]):
    def document(): Css = Css(nodes())

    private def fail(reason: CssError.Reason): Nothing =
      abort(CssError(reason, cursor.line, cursor.column))

    // Parse a list of nodes until end-of-input or an unconsumed `}`.
    private def nodes(): List[Node] =
      val acc = scala.collection.mutable.ListBuffer[Node]()
      var continue = true

      while continue do
        skipTrivia()
        val datum = cursor.peek

        if datum.isEnd || datum == '}' then continue = false
        else if datum == ';' then cursor.advance() // a stray, empty declaration
        else acc.append(node())

      acc.to(List)

    // Parse a single node: scan a "prelude" up to a terminating `{`, `;`, `}`
    // or end-of-input, respecting comments, strings and `()`/`[]` nesting.
    private def node(): Node =
      val buf = java.lang.StringBuilder()
      var depth = 0
      var colonAt = -1
      var block = false
      var scanning = true

      def whitespace(): Unit =
        if buf.length > 0 && buf.charAt(buf.length - 1) != ' ' then buf.append(' ')

      while scanning do
        val datum = cursor.peek

        if datum.isEnd then scanning = false
        else
          val char = datum.asInt.toChar

          char match
            case '/' =>
              cursor.advance()

              if cursor.peek == '*' then
                cursor.advance()
                comment()
                whitespace()
              else
                buf.append('/')

            case '"' | '\'' =>
              buf.append(char)
              cursor.advance()
              string(char, buf)

            case '(' | '[' =>
              depth += 1
              buf.append(char)
              cursor.advance()

            case ')' | ']' =>
              if depth > 0 then depth -= 1
              buf.append(char)
              cursor.advance()

            case '{' if depth == 0 =>
              block = true
              cursor.advance()
              scanning = false

            case ';' if depth == 0 =>
              cursor.advance()
              scanning = false

            case '}' if depth == 0 =>
              scanning = false

            case ':' if depth == 0 =>
              if colonAt < 0 then colonAt = buf.length
              buf.append(':')
              cursor.advance()

            case ' ' | '\t' | '\n' | '\r' =>
              whitespace()
              cursor.advance()

            case other =>
              buf.append(other)
              cursor.advance()

      val text = buf.toString.nn.tt.trim

      if block then
        val body = nodes()
        if cursor.peek == '}' then cursor.advance() else fail(CssError.Reason.UnexpectedEnd)

        if text.starts(t"@") then
          val (name, prelude) = atRule(text)
          Node.At(name, prelude, body)
        else
          Node.Rule(SelectorParser.parse(text), body)
      else if text.starts(t"@") then
        val (name, prelude) = atRule(text)
        Node.At(name, prelude, Unset)
      else if colonAt >= 0 then
        val property = buf.substring(0, colonAt).nn.tt.trim
        val value = buf.substring(colonAt + 1).nn.tt.trim
        if validating then validate(property, value)
        Node.Declaration(property, value)
      else
        if validating then validate(text, t"")
        Node.Declaration(text, t"")

    // Check a declaration's property name and value, accumulating (via `raise`)
    // any error rather than aborting, so the rest of the stylesheet is still
    // read. Custom properties (`--…`) accept any value.
    private def validate(property: Text, value: Text): Unit =
      if property.starts(t"--") then ()
      else PropertyDef.of(property) match
        case definition: PropertyDef =>
          SyntaxMatcher.check(definition, value) match
            case Outcome.Valid =>
              ()

            case Outcome.Invalid =>
              raise(CssError(CssError.Reason.BadValue(property, value), cursor.line, cursor.column))

            case Outcome.Unsupported(types) =>
              val reason = CssError.Reason.UnsupportedValue(property, types)
              raise(CssError(reason, cursor.line, cursor.column))

        case _ =>
          raise(CssError(CssError.Reason.UnknownProperty(property), cursor.line, cursor.column))

    // Split an `@…` prelude into its identifier and the remaining prelude text.
    private def atRule(text: Text): (Text, Text) =
      val body = text.s.substring(1).nn
      val space = body.indexOf(' ')

      if space < 0 then (body.tt, t"")
      else (body.substring(0, space).nn.tt, body.substring(space + 1).nn.tt.trim)

    // Consume the body of a `/* … */` comment; the opening `/*` is already read.
    private def comment(): Unit =
      var scanning = true

      while scanning do
        val datum = cursor.peek

        if datum.isEnd then fail(CssError.Reason.UnterminatedComment)
        else if datum == '*' then
          cursor.advance()
          val next = cursor.peek

          if next.isEnd then fail(CssError.Reason.UnterminatedComment)
          else if next == '/' then
            cursor.advance()
            scanning = false
        else
          cursor.advance()

    // Consume the body of a quoted string verbatim into `buf`; the opening
    // quote is already appended and read.
    private def string(quote: Char, buf: java.lang.StringBuilder): Unit =
      var scanning = true

      while scanning do
        val datum = cursor.peek

        if datum.isEnd then fail(CssError.Reason.UnterminatedString)
        else
          val char = datum.asInt.toChar

          if char == '\\' then
            buf.append('\\')
            cursor.advance()
            val escaped = cursor.peek

            if escaped.isEnd then fail(CssError.Reason.UnterminatedString)
            else
              buf.append(escaped.asInt.toChar)
              cursor.advance()
          else if char == quote then
            buf.append(quote)
            cursor.advance()
            scanning = false
          else
            buf.append(char)
            cursor.advance()

    // Consume whitespace and `/* … */` comments.
    private def skipTrivia(): Unit =
      var continue = true

      while continue do
        val datum = cursor.peek

        if datum == ' ' || datum == '\t' || datum == '\n' || datum == '\r' then cursor.advance()
        else if datum == '/' && cursor.lookahead(cursor.next() && cursor.peek == '*') then
          cursor.advance()
          cursor.advance()
          comment()
        else
          continue = false
