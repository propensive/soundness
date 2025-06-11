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
┃    Soundness, version 0.34.0.                                                                    ┃
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
package xylophone

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import errorDiagnostics.empty

object XmlInterpolation:
  enum XmlInput:
    case Flat(text: Text)
    case Structured(xml: XmlAst.Element)

  enum XmlContext:
    case AttributeValue, InTagName, SelfClosingTagName, TagClose, ClosingTag, InAttributeName,
        InTagBody, AttributeEquals, Body, InBodyEntity, InAttributeEntity

  case class CharExtractor(chars: Set[Char]):
    val charSet = chars.to(Set)
    def unapply(char: Char): Boolean = charSet.contains(char)

  case class ParseStateNode(name: Text, namespaces: Set[Text])

  case class ParseState
              (offset:  Int,
               context: XmlContext,
               stack:   List[ParseStateNode],
               current: Text,
               source:  Text,
               ns:      Boolean):

    def apply(newContext: XmlContext, char: Char) =
      copy(context = newContext, current = t"$current$char", offset = offset + 1)

    def apply(newContext: XmlContext): ParseState =
      copy(context = newContext, offset = offset + 1, ns = false)

    def apply(char: Char): ParseState = copy(offset = offset + 1, current = t"$current$char")
    def apply(): ParseState = copy(offset = offset + 1)
    def reset: ParseState = copy(current = t"")
    def namespace: ParseState = copy(ns = true)
    def push: ParseState = copy(stack = ParseStateNode(current, Set()) :: stack, current = t"")

    def addNamespace(ns: Text): ParseState =
      copy(stack = stack.head.copy(namespaces = stack.head.namespaces + ns) :: stack.tail)

    def checkNs: Boolean =
      !stack.head.name.contains(":")
      || stack.flatMap(_.namespaces).contains(stack.head.name.cut(t":")(0))

    def rollback(difference: Int): ParseState = copy(offset = offset - difference)

    def pop: ParseState throws InterpolationError =
      val tag = stack.prim.or:
        throw InterpolationError
               (m"spurious closing tag: $current", offset - current.length, current.length)

      if tag.name == current then copy(stack = stack.tail) else
        throw
          InterpolationError
           (m"closing tag '$current' does not match expected tag '${tag.name}'",
            offset - current.length,
            current.length)

  given xmlInput: Substitution[XmlInput, Text, "t"]:
    def embed(value: Text) = XmlInput.Flat(value)

  given genInsert: [value] => (writer: XmlEncoder[value])
        =>  Insertion[XmlInput, value] =
    value => XmlInput.Structured(writer.write(value))

  object XmlInterpolator extends Interpolator[XmlInput, ParseState, XmlDoc]:
    import XmlContext.*

    val Letters = ('a' to 'z').to(Set) ++ ('A' to 'Z').to(Set)
    val Digits = ('0' to '9').to(Set)
    val TagChar = CharExtractor(Letters ++ Digits + '.' + '-' + '_')
    def initial: ParseState = ParseState(0, XmlContext.Body, Nil, t"", t"", false)

    private def escape(str: Text): Text =
      str
      . sub(t"\"", t"&quot;")
      . sub(t"'", t"&apos;")
      . sub(t"<", t"&lt;")
      . sub(t">", t"&gt;")
      . sub(t"&", t"&amp;")

    def skip(state: ParseState): ParseState = state.context match
      case AttributeValue | Body => parse(state, t"")
      case AttributeEquals       => parse(state, t"\"\"")

      case _ =>
        throw InterpolationError(m"a substitution cannot be made in this position")

    def insert(state: ParseState, value: XmlInput): ParseState =
      state.context match
        case AttributeValue | Body => value match
          case XmlInput.Flat(str)       => parse(state, escape(str))
          case XmlInput.Structured(xml) => parse(state, xml.show)

        case AttributeEquals       => value match
          case XmlInput.Flat(str)       => parse(state, t"\"${escape(str)}\"")
          case XmlInput.Structured(xml) => parse(state, t"\"${escape(xml.show)}\"")

        case _ =>
          throw InterpolationError(m"a substitution cannot be made in this position")

    def complete(state: ParseState): XmlDoc =
      if state.stack.nonEmpty then throw InterpolationError(m"""
          expected closing tag: ${state.stack.head.name}
      """)

      safely(Xml.parse(state.source)).or:
        throw InterpolationError(m"the XML could not be parsed")

    def parse(state: ParseState, string: Text): ParseState =
      string.chars.foldLeft(state.copy(offset = 0)):
        case (state@ParseState(_, _, _, _, _, _), char) => state.context match

        case InTagName => char match
          case TagChar()                => state(char)
          case ' ' | '\n' | '\r' | '\t' => state.push(InTagBody)

          case '/' =>
            if state.current.empty then state(ClosingTag) else state(SelfClosingTagName)

          case ':' =>
            if state.ns then throw InterpolationError(m"""
              the tag name can contain at most one ':' character to indicate a namespace
            """, state.offset, 1)
            else state(char).namespace

          case '>' =>
            if state.push.checkNs then state.push(Body)
            else throw InterpolationError(
                m"""the tag uses a namespace that has not been declared with an xmlns attribute""")

          case _ =>
            throw InterpolationError(m"""not a valid tag name character""", state.offset, 1)

        case SelfClosingTagName => char match
          case TagChar() => state(char)

          case ':' =>
            if state.ns then throw
              InterpolationError
               (m"the tag name can contain at most one ':' character to indicate a namespace",
                state.offset,
                1)
            else state(char).namespace

          case '>' =>
            if state.checkNs then state(Body)
            else throw
              InterpolationError
               (m"the tag uses a namespace that has not been declared with an xmlns attribute")

          case _ => throw InterpolationError(m"expected '>'", state.offset, 1)

        case ClosingTag => char match
          case TagChar()                => state(char)
          case '>'                      => state.pop(Body)
          case ' ' | '\n' | '\t' | '\r' => state()

          case ':' =>
            if state.ns then throw
              InterpolationError
               (m"the tag name can contain at most one ':' character to indicate a namespace",
                state.offset,
                1)
            else state(char).namespace

          case _ =>
            throw InterpolationError(m"expected '>' or whitespace", state.offset, 1)

        case InAttributeName => char match
          case TagChar()                => state(char)
          case ' ' | '\n' | '\r' | '\t' => state(InAttributeName)

          case '>' =>
            throw InterpolationError(m"attribute value has not been specified", state.offset, 1)

          case '=' =>
            if state.current.starts(t"xmlns:")
            then state.addNamespace(state.current.skip(6))(AttributeEquals)
            else state(AttributeEquals)

          case ':' =>
            if state.ns then throw
              InterpolationError
               (m"the attribute name can contain at most one ':' character indicating a namespace",
                state.offset,
                1)
            else state(char).namespace

          case char =>
            throw InterpolationError
                   (m"character $char is not valid in an attribute name", state.offset, 1)

        case AttributeEquals => char match
          case ' ' | '\n' | '\r' | '\t' => state()
          case '"'                      => state(AttributeValue)

          case _ =>
            throw InterpolationError(m"expected '\"'", state.offset, 1)

        case AttributeValue => char match
          case '"'  => state(InTagBody)
          case '&'  => state(InAttributeEntity)
          case char => state(char)

        case InTagBody => char match
          case ' ' | '\n' | '\r' | '\t' => state(InTagBody)
          case TagChar()                => state(InAttributeName, char)
          case '/'                      => state(TagClose)

          case '>' =>
            if state.checkNs then state(Body)
            else throw
              InterpolationError
               (m"the tag uses a namespace that has not been declared with an xmlns attribute")

          case char =>
            throw InterpolationError
                   (m"character $char is not permitted in a tag name", state.offset)

        case TagClose => char match
          case '>' => state.pop(Body)
          case _   => throw InterpolationError(m"expected >", state.offset, 1)

        case Body => char match
          case '<' => state(InTagName)
          case '&' => state(InBodyEntity)
          case _   => state()

        case InBodyEntity => char match
          case ';' => state()

          case char =>
            throw InterpolationError
                   (m"character $char is not valid in an entity name", state.offset, 1)

        case InAttributeEntity  => char match
          case ';'       => state()
          case TagChar() => state()

          case char =>
            throw InterpolationError
                   (m"character $char is not valid in an entity name", state.offset, 1)

    . copy(source = t"${state.source}$string")
