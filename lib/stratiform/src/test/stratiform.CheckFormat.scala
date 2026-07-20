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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import vacuous.*

// Reads upstream `.check` fixtures (Rust's #[derive(Debug)] output) into a
// cross-language CheckTree. Tests compare CheckTrees produced from the
// reference fixtures with CheckTrees built from the Scala parser's AST,
// avoiding any dependence on byte-equal Rust↔Scala string formatting.

object CheckFormat:

  enum CheckTree:
    case Struct(name: Text, fields: List[(Text, CheckTree)])
    case Sequence(items: List[CheckTree])
    case Variant(name: Text, payload: Optional[CheckTree])
    case Tuple(items: List[CheckTree])
    case Str(value: Text)
    case Num(value: Int)

  case class ExpectedError(code: Int, startOffset: Int, endOffset: Int, message: Text)

  case class CheckFile(tree: CheckTree, errors: List[ExpectedError])

  def parse(source: Text): CheckFile =
    val cursor = Cursor(source.s, 0)
    val tree = parseValue(cursor)
    cursor.skipWhitespace()
    val errors =
      if cursor.matchesLiteral("errors:") then
        cursor.advance("errors:".length)
        parseErrors(cursor)
      else Nil

    CheckFile(tree, errors)

  // Parse a multi-document stream `.check` fixture (§6.1): a sequence of
  // `=== document N ===` header lines, each followed by a `Document { … }`
  // value and an optional per-document `errors:` block. Each section is parsed
  // independently with `parse`.
  def parseStream(source: Text): List[CheckFile] =
    val sections = scala.collection.mutable.ListBuffer.empty[StringBuilder]
    source.s.split('\n').foreach: line =>
      if line.startsWith("=== document ") then sections += new StringBuilder()
      else if sections.nonEmpty then sections.last.append(line).append('\n')

    sections.toList.map(section => parse(Text(section.toString)))

  private def parseValue(cursor: Cursor): CheckTree =
    cursor.skipWhitespace()
    val c = cursor.peek()

    if c == '['  then parseSeq(cursor)
    else if c == '('  then parseTuple(cursor)
    else if c == '"'  then CheckTree.Str(parseString(cursor))
    else if c == '\'' then CheckTree.Str(parseChar(cursor))
    else if isDigit(c) || c == '-' then CheckTree.Num(parseInt(cursor))
    else parseIdentifierValue(cursor)

  private def parseIdentifierValue(cursor: Cursor): CheckTree =
    val name = cursor.readIdentifier()
    cursor.skipInlineWhitespace()

    cursor.peek() match
      case '{' => CheckTree.Struct(name, parseStructBody(cursor))
      case '(' => CheckTree.Variant(name, Optional(parseSomePayload(cursor)))
      case _   => CheckTree.Variant(name, Unset)

  private def parseStructBody(cursor: Cursor): List[(Text, CheckTree)] =
    cursor.advance(1) // consume '{'
    val builder = scala.collection.mutable.ListBuffer.empty[(Text, CheckTree)]
    cursor.skipWhitespace()

    while cursor.peek() != '}' do
      val field = cursor.readIdentifier()
      cursor.skipWhitespace()
      cursor.expect(':')
      cursor.skipWhitespace()
      val value = parseValue(cursor)
      builder += ((field, value))
      cursor.skipWhitespace()
      if cursor.peek() == ',' then
        cursor.advance(1)
        cursor.skipWhitespace()

    cursor.advance(1) // consume '}'

    builder.toList

  private def parseSomePayload(cursor: Cursor): CheckTree =
    cursor.advance(1) // consume '('
    cursor.skipWhitespace()
    val value = parseValue(cursor)
    cursor.skipWhitespace()
    if cursor.peek() == ',' then
      cursor.advance(1)
      cursor.skipWhitespace()
    cursor.expect(')')

    value

  private def parseSeq(cursor: Cursor): CheckTree =
    cursor.advance(1) // consume '['
    val builder = scala.collection.mutable.ListBuffer.empty[CheckTree]
    cursor.skipWhitespace()

    while cursor.peek() != ']' do
      builder += parseValue(cursor)
      cursor.skipWhitespace()
      if cursor.peek() == ',' then
        cursor.advance(1)
        cursor.skipWhitespace()

    cursor.advance(1)

    CheckTree.Sequence(builder.toList)

  private def parseTuple(cursor: Cursor): CheckTree =
    cursor.advance(1) // consume '('
    val builder = scala.collection.mutable.ListBuffer.empty[CheckTree]
    cursor.skipWhitespace()

    while cursor.peek() != ')' do
      builder += parseValue(cursor)
      cursor.skipWhitespace()
      if cursor.peek() == ',' then
        cursor.advance(1)
        cursor.skipWhitespace()

    cursor.advance(1)

    CheckTree.Tuple(builder.toList)

  private def parseString(cursor: Cursor): Text =
    cursor.advance(1) // opening "
    val builder = StringBuilder()
    while cursor.peek() != '"' do
      val ch = cursor.peek()
      if ch == '\\' then
        cursor.advance(1)
        cursor.peek() match
          case 'n'  => builder.append('\n'); cursor.advance(1)
          case 't'  => builder.append('\t'); cursor.advance(1)
          case 'r'  => builder.append('\r'); cursor.advance(1)
          case '\\' => builder.append('\\'); cursor.advance(1)
          case '"'  => builder.append('"');  cursor.advance(1)
          case '0'  => builder.append(' '); cursor.advance(1)
          case 'x'  =>
            cursor.advance(1)
            val hex = cursor.takeWhile(c => isHexDigit(c))
            builder.append(Integer.parseInt(hex, 16).toChar)

          case 'u'  =>
            cursor.advance(1)
            if cursor.peek() == '{' then cursor.advance(1)
            val hex = cursor.takeWhile(c => isHexDigit(c))
            if cursor.peek() == '}' then cursor.advance(1)
            builder.append(Character.toChars(Integer.parseInt(hex, 16)))

          case other =>
            builder.append(other)
            cursor.advance(1)
      else
        builder.append(ch)
        cursor.advance(1)

    cursor.advance(1) // closing "

    Text(builder.toString)

  private def parseChar(cursor: Cursor): Text =
    cursor.advance(1) // opening '
    val ch =
      if cursor.peek() == '\\' then
        cursor.advance(1)
        val esc = cursor.peek()
        cursor.advance(1)
        esc match
          case 'n'  => '\n'
          case 't'  => '\t'
          case 'r'  => '\r'
          case '\\' => '\\'
          case '\'' => '\''
          case other => other
      else
        val c = cursor.peek()
        cursor.advance(1)
        c

    cursor.expect('\'')

    Text(ch.toString)

  private def parseInt(cursor: Cursor): Int =
    val start = cursor.position
    if cursor.peek() == '-' then cursor.advance(1)
    while isDigit(cursor.peek()) do cursor.advance(1)

    cursor.input.substring(start, cursor.position).toInt

  private def parseErrors(cursor: Cursor): List[ExpectedError] =
    val builder = scala.collection.mutable.ListBuffer.empty[ExpectedError]
    cursor.skipLineWhitespace()

    while cursor.position < cursor.input.length do
      cursor.skipLineWhitespace()
      if cursor.peek() == 'E' then
        cursor.advance(1)
        val code = parseInt(cursor)
        cursor.skipInlineWhitespace()
        cursor.expect('[')
        val start = parseInt(cursor)
        cursor.expect(',')
        val end = parseInt(cursor)
        cursor.expect(')')
        cursor.skipInlineWhitespace()
        if cursor.peek() == ':' then cursor.advance(1)
        cursor.skipInlineWhitespace()
        val message = cursor.takeUntil(_ == '\n')
        builder += ExpectedError(code, start, end, Text(message))

      cursor.skipLineWhitespace()

    builder.toList

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  private def isHexDigit(c: Char): Boolean = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  private final class Cursor(val input: String, var position: Int):
    def peek(): Char = if position < input.length then input.charAt(position) else ' '

    def advance(n: Int): Unit = position += n

    def matchesLiteral(literal: String): Boolean =
      position + literal.length <= input.length && input.regionMatches(position, literal, 0, literal.length)

    def expect(c: Char): Unit =
      if peek() != c then sys.error
       (s"CheckFormat parse error at $position: expected '$c' but found '${peek()}'")
      advance(1)

    def readIdentifier(): Text =
      val start = position
      while position < input.length && isIdentifierChar(input.charAt(position)) do position += 1

      Text(input.substring(start, position))

    def takeWhile(predicate: Char => Boolean): String =
      val start = position
      while position < input.length && predicate(input.charAt(position)) do position += 1

      input.substring(start, position)

    def takeUntil(predicate: Char => Boolean): String =
      val start = position
      while position < input.length && !predicate(input.charAt(position)) do position += 1

      input.substring(start, position)

    def skipInlineWhitespace(): Unit =
      while position < input.length
        && { val c = input.charAt(position); c == ' ' || c == '\t' }
      do position += 1

    def skipLineWhitespace(): Unit =
      while position < input.length
        && { val c = input.charAt(position); c == ' ' || c == '\t' || c == '\n' || c == '\r' }
      do position += 1

    def skipWhitespace(): Unit = skipLineWhitespace()

  private def isIdentifierChar(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
