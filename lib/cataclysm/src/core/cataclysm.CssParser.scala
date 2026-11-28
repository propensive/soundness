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

import murmuration.Iterator
import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.{after, before, skip}
import vacuous.*
import zephyrine.*

import Css.Node
import cataclysm.Css.Outcome

// A small, lenient recursive-descent CSS reader built on a `zephyrine.Cursor`.
// Selectors, at-rule preludes and property values are kept as raw (normalized)
// `Text` — comments are stripped and runs of whitespace are collapsed to a
// single space, then the result is trimmed. Nested rules are supported: a rule
// body is itself a list of nodes.
private[cataclysm] object CssParser:
  // Native `Chain` sibling of the `Iterator` overload: same cursor
  // construction, no stdlib hop. No default for `validating`: only one
  // overloaded alternative may define default arguments.
  def parse(input: Chain[Text], validating: Boolean)(using Tactic[Css.Error]): Css =
    import zephyrine.lineation.linefeedChar

    Parser(Cursor[Text](input), validating).document()

  // The interoperation shape: an `Iterator` of chunks.
  // A declaration containing `placeholder` (the `css"…"` interpolator's stand-in for a
  // substitution) is not validated here: the interpolator checks it against the type of
  // the value substituted.
  def parse(input: Iterator[Text], validating: Boolean = true, placeholder: Optional[Text] = Unset)
    ( using Tactic[Css.Error] )
  :   Css =

    import zephyrine.lineation.linefeedChar

    Parser(Cursor[Text](input), validating, placeholder).document()

  // The at-rules whose blocks hold style rules or declarations of properties. The block of any
  // other at-rule (`@font-face`, `@page`, `@counter-style`, `@property`, …) holds *descriptors*,
  // which have grammars of their own, and are validated against those, not as properties.
  private def grouping(name: Text): Boolean = name match
    case t"media" | t"supports" | t"layer" | t"container" | t"scope" | t"starting-style"
    | t"document" | t"position-try" =>
      true

    case _ =>
      false

  private class Parser
    ( cursor: Cursor[Text, ?], validating: Boolean, placeholder: Optional[Text] = Unset )
    ( using Tactic[Css.Error] ):

    // The name of the at-rule whose descriptors the declarations being read are, or `Unset`
    // if they are declarations of properties.
    private var descriptive: Optional[Text] = Unset

    def document(): Css = Css(nodes())

    private def fail(reason: Css.Error.Reason): Nothing =
      abort(Css.Error(reason, cursor.line, cursor.column))

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
      val line = cursor.line
      val column = cursor.column
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
        val enclosing = descriptive

        descriptive =
          if !text.starts(t"@") then Unset else
            val name = atRule(text)(0).lower
            if grouping(name) then Unset else name

        val body = nodes()
        descriptive = enclosing
        if cursor.peek == '}' then cursor.advance() else fail(Css.Error.Reason.UnexpectedEnd)

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
        if validating then validate(property, value, line, column)
        Node.Declaration(property, value)
      else
        if validating then validate(text, t"", line, column)
        Node.Declaration(text, t"")

    // Check a declaration's property name and value, accumulating (via `raise`)
    // any error rather than aborting, so the rest of the stylesheet is still
    // read. Custom properties (`--…`) accept any value, and a declaration with a
    // substitution is the interpolator's to check. An error is reported at the start
    // of its declaration.
    private def validate(property: Text, value: Text, line: Ordinal, column: Ordinal): Unit =
      val substituted = placeholder.lay(false): placeholder =>
        property.contains(placeholder) || value.contains(placeholder)

      if property.starts(t"--") || substituted then ()
      else descriptive.lay(check(property, PropertyDef.of(property), value, line, column)):
        rule => describe(rule, property, value, line, column)

    // Check a declaration in the block of the at-rule `rule` against that rule's
    // descriptors. An at-rule the dataset gives no descriptors (`@keyframes`, whose block
    // holds rules, or one which is not known) is not checked. `@page` takes properties
    // as well as its descriptors. A descriptor whose grammar uses a type the matcher
    // does not implement (`<unicode-range-token>`) cannot be verified, and is accepted.
    private def describe(rule: Text, name: Text, value: Text, line: Ordinal, column: Ordinal)
    :   Unit =

      if PropertyDef.descriptive(rule) then PropertyDef.descriptor(rule, name.lower) match
        case definition: PropertyDef =>
          if Css.Syntax.Matcher.check(definition, value) == Outcome.Invalid
          then raise(Css.Error(Css.Error.Reason.BadValue(name, value), line, column))

        case _ =>
          if rule == t"page" then check(name, PropertyDef.of(name), value, line, column)
          else raise(Css.Error(Css.Error.Reason.UnknownDescriptor(rule, name), line, column))

    private def check
      ( property:   Text,
        definition: Optional[PropertyDef],
        value:      Text,
        line:       Ordinal,
        column:     Ordinal )
    :   Unit =

      definition match
        case definition: PropertyDef =>
          Css.Syntax.Matcher.check(definition, value) match
            case Outcome.Valid =>
              ()

            case Outcome.Invalid =>
              raise:
                Css.Error(Css.Error.Reason.BadValue(property, value), line, column)

            case Outcome.Unsupported(types) =>
              val reason = Css.Error.Reason.UnsupportedValue(property, types)
              raise(Css.Error(reason, line, column))

        case _ =>
          raise(Css.Error(Css.Error.Reason.UnknownProperty(property), line, column))

    // Split an `@…` prelude into its identifier and the remaining prelude text.
    private def atRule(text: Text): (Text, Text) =
      val body: Text = text.skip(1)

      body.offsetOf(t" ").lay((body, t"")): space => (body.before(space), body.after(space).trim)

    // Consume the body of a `/* … */` comment; the opening `/*` is already read.
    private def comment(): Unit =
      var scanning = true

      while scanning do
        val datum = cursor.peek

        if datum.isEnd then fail(Css.Error.Reason.UnterminatedComment)
        else if datum == '*' then
          cursor.advance()
          val next = cursor.peek

          if next.isEnd then fail(Css.Error.Reason.UnterminatedComment)
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

        if datum.isEnd then fail(Css.Error.Reason.UnterminatedString)
        else
          val char = datum.asInt.toChar

          if char == '\\' then
            buf.append('\\')
            cursor.advance()
            val escaped = cursor.peek

            if escaped.isEnd then fail(Css.Error.Reason.UnterminatedString)
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
