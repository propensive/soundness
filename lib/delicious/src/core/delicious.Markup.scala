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
package delicious

import scala.collection.mutable

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*

object Markup:
  private final val Start    = '\uE000'
  private final val End      = '\uE001'
  private final val AttrsSep = '\uE002'
  private final val TextSep  = '\uE003'
  private final val AttrSep  = '\u001F'

  def marked(text: Text): Boolean = text.s.exists { char => char >= Start && char <= TextSep }

  private[delicious] def decode(text: Text): Text =
    if text.s.indexOf('%') < 0 then text else
      val builder = new StringBuilder
      var index = 0

      while index < text.s.length do
        val char = text.s.charAt(index)

        if char == '%' && index + 4 < text.s.length then
          try
            builder.append(Integer.parseInt(text.s.substring(index + 1, index + 5), 16).toChar)
            index += 5
          catch case _: NumberFormatException =>
            builder.append(char)
            index += 1
        else
          builder.append(char)
          index += 1

      builder.toString.tt

  private def typed(kind: Text, attrs: List[(Text, Text)], children: List[Markup]): Markup =
    def attr(name: Text): Optional[Text] =
      attrs.seek(_(0) == name).let(_(1))

    val rendition = Rendition(attr(t"style"))

    kind.s match
      case "type" =>
        val marked: List[(Text, Text)] = attrs.filter(_(0) == t"p")
        val decoded: List[Optional[Placeholder]] = marked.map { (_, value) => Placeholder.decode(value) }

        val placeholders: List[Placeholder] =
          decoded.sweep { case placeholder: Placeholder => placeholder }

        Typed(attr(t"tasty"), placeholders, rendition, children)

      case "sym"  => Symbolic(attr(t"name").or(t""), attr(t"full").or(t""), rendition, children)
      case "name" => Named(attr(t"isType").let(_ == t"true").or(false), rendition, children)
      case "code" => Code(rendition, children)
      case _      => Spanned(kind, rendition, children)

  /** Parse a marked-up string into a tree, following the compiler's
   *  `DiagnosticMarkup.parse` semantics exactly: anything that does not form a
   *  complete marker is dropped and the surrounding content treated as plain
   *  text; the children of an unterminated marker are spliced into its parent. */
  def parse(text: Text): List[Markup] =
    val input: String = text.s

    final class Frame(val kind: Text, val attrs: List[(Text, Text)]):
      val children = mutable.ListBuffer[Markup]()
      val text = new StringBuilder

      def flush(): Unit =
        if text.nonEmpty then
          children += Textual(text.toString.tt)
          text.clear()

    val root = Frame(t"", Nil)
    var stack: List[Frame^] = List(root)
    var index = 0

    while index < input.length do
      val char = input.charAt(index)

      if char == Start then
        val kindEnd = input.indexOf(AttrsSep, index + 1)
        val attrsEnd = if kindEnd < 0 then -1 else input.indexOf(TextSep, kindEnd + 1)

        val headerOk = kindEnd > 0 && attrsEnd > 0
          && !input.substring(index + 1, attrsEnd).nn.exists { char => char == Start || char == End }

        if !headerOk then index += 1 else
          stack.stdlib.head.flush()

          val attrs = input.substring(kindEnd + 1, attrsEnd).nn match
            case ""         => Nil
            case attributes =>
              attributes.tt.cut(AttrSep).map: keyValue =>
                keyValue.cut(t"=", 2) match
                  case List(key, value) => (key, decode(value))
                  case _                => (keyValue, t"")

          stack = Frame(input.substring(index + 1, kindEnd).nn.tt, attrs) :: stack
          index = attrsEnd + 1

      else if char == End then
        stack match
          // Do not destructure the parent here (`case top :: (parent :: _)`):
          // that binds two `Frame^`s drawn from the same stack, and the
          // separation checker cannot prove them distinct. Finish with `top`,
          // then reach the parent through `rest.stdlib.head`, so only one frame is
          // ever live. Were the two to alias, the old shape mutated a frame
          // while reading it. The `.stdlib` bridges here (and the `stack.stdlib.head`
          // reads above and below) are forced: the stack's element type is a capability
          // (`Frame^`), which the generic `Countable`/`Terminable` instances cannot
          // accept — "capability cannot flow into capture set {}".
          case top :: rest if !rest.stdlib.isEmpty =>
            top.flush()
            val node = typed(top.kind, top.attrs, top.children.to(List))
            stack = rest
            rest.stdlib.head.children += node

          case _ => // stray End at top level

        index += 1

      else if char == AttrsSep || char == TextSep then index += 1

      else
        stack.stdlib.head.text.append(char)
        index += 1

    // splice the children of any unterminated markers into their parents
    while !stack.stdlib.tail.isEmpty do
      stack match
        // As above: one frame live at a time, parent reached via `rest.stdlib.head`,
        // and `.stdlib` throughout because `Frame^` is a capability element type.
        case top :: rest if !rest.stdlib.isEmpty =>
          top.flush()
          // `toList` copies into a fresh immutable list sharing nothing with the
          // buffer, so the inferred `^{top.children}` is spurious; laundering it
          // lets the orphans outlive `top`. Splicing them into the parent while
          // they still captured `top.children` is what the checker refused.
          val orphans = scala.caps.unsafe.unsafeAssumePure(top.children.toList)
          stack = rest
          rest.stdlib.head.children ++= orphans

        case _ => ()

    root.flush()
    root.children.to(List)

  /** The string with all markers removed, keeping only the visible text. */
  def plain(text: Text): Text =
    if !marked(text) then text else parse(text).map(_.plain).join

enum Markup:
  case Textual(text: Text)

  case Typed
        (tasty:        Optional[Text],
         placeholders: List[Placeholder],
         rendition:    Rendition,
         children:     List[Markup])

  case Symbolic(name: Text, full: Text, rendition: Rendition, children: List[Markup])
  case Named(isType: Boolean, rendition: Rendition, children: List[Markup])
  case Code(rendition: Rendition, children: List[Markup])
  case Spanned(kind: Text, rendition: Rendition, children: List[Markup])

  def plain: Text = this match
    case Textual(text)               => text
    case Typed(_, _, _, children)    => children.map(_.plain).join
    case Symbolic(_, _, _, children) => children.map(_.plain).join
    case Named(_, _, children)       => children.map(_.plain).join
    case Code(_, children)           => children.map(_.plain).join
    case Spanned(_, _, children)     => children.map(_.plain).join
