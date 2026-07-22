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

import anticipation.*
import fulminate.*
import gossamer.*
import probably.*
import vacuous.*

object Tests extends Suite(m"Delicious Tests"):
  val Start: Char    = '\uE000'
  val End: Char      = '\uE001'
  val AttrsSep: Char = '\uE002'
  val TextSep: Char  = '\uE003'
  val AttrSep: Char  = '\u001F'

  def mark(kind: Text, attrs: List[(Text, Text)], text: Text): Text =
    val attributes = attrs.map { (key, value) => t"$key=$value" }.join(t"${AttrSep}")
    t"${Start}$kind${AttrsSep}$attributes${TextSep}$text${End}"

  def run(): Unit =
    test(m"Unmarked text parses as a single text node"):
      Markup.parse(t"type mismatch")
    . assert(_ == List(Markup.Textual(t"type mismatch")))

    test(m"A sym marker parses with its attributes"):
      Markup.parse(t"method ${mark(t"sym", List(t"name" -> t"foo", t"full" -> t"example.foo"), t"foo")} here")
    . assert(_ == List
        ( Markup.Textual(t"method "),
          Markup.Symbolic(t"foo", t"example.foo", Style.Default, List(Markup.Textual(t"foo"))),
          Markup.Textual(t" here") ))

    test(m"A name marker records whether it is a type name"):
      Markup.parse(mark(t"name", List(t"isType" -> t"true"), t"Elem"))
    . assert(_ == List(Markup.Named(true, Style.Default, List(Markup.Textual(t"Elem")))))

    test(m"An unknown marker kind falls back to a spanned node"):
      Markup.parse(mark(t"mystery", Nil, t"???"))
    . assert(_ == List(Markup.Spanned(t"mystery", Style.Default, List(Markup.Textual(t"???")))))

    test(m"Styles decode from their wire names"):
      Markup.parse(mark(t"sym", List(t"name" -> t"foo", t"style" -> t"dcl"), t"def foo: Int"))
    . assert(_ == List
        ( Markup.Symbolic(t"foo", t"", Style.Declaration, List(Markup.Textual(t"def foo: Int"))) ))

    test(m"Markers nest, and children keep their order"):
      val inner = mark(t"sym", List(t"name" -> t"foo"), t"foo")
      Markup.parse(mark(t"name", Nil, t"method $inner"))
    . assert(_ == List
        ( Markup.Named
            ( false,
              Style.Default,
              List
                ( Markup.Textual(t"method "),
                  Markup.Symbolic(t"foo", t"", Style.Default, List(Markup.Textual(t"foo"))) )) ))

    test(m"Percent-encoded attribute values decode"):
      Markup.parse(mark(t"sym", List(t"name" -> t"%003ainit%003a"), t"constructor"))
    . assert(_ == List
        ( Markup.Symbolic(t":init:", t"", Style.Default, List(Markup.Textual(t"constructor"))) ))

    test(m"A stray start marker is dropped"):
      Markup.parse(t"broken ${Start} text")
    . assert(_ == List(Markup.Textual(t"broken  text")))

    test(m"A stray end marker is dropped"):
      Markup.parse(t"broken ${End} text")
    . assert(_ == List(Markup.Textual(t"broken  text")))

    test(m"An unterminated marker splices its children into the parent"):
      Markup.parse(t"a ${Start}sym${AttrsSep}name=foo${TextSep}bar")
    . assert(_ == List(Markup.Textual(t"a "), Markup.Textual(t"bar")))

    test(m"A truncated header is treated as text"):
      Markup.parse(t"a ${Start}sym no header end")
    . assert(_ == List(Markup.Textual(t"a sym no header end")))

    test(m"Plain text strips all markers"):
      val inner = mark(t"sym", List(t"name" -> t"foo"), t"foo")
      Markup.plain(t"method ${mark(t"name", Nil, t"call of $inner")} failed")
    . assert(_ == t"method call of foo failed")

    test(m"A type marker carries its TASTy payload and placeholders"):
      val placeholder = t"0|local-type|Foo|1||Foo[Int]"
      Markup.parse(mark(t"type", List(t"tasty" -> t"QUJD", t"p" -> placeholder), t"Foo[Int]"))
    . assert(_ == List
        ( Markup.Typed
            ( t"QUJD",
              List(Placeholder(0, PlaceholderKind.LocalType, t"Foo", 1, Unset, t"Foo[Int]")),
              Style.Default,
              List(Markup.Textual(t"Foo[Int]")) ) ))

    test(m"A placeholder decodes its six fields"):
      Placeholder.decode(t"3|skolem|x|0|Test.scala:5|x.type")
    . assert(_ == Placeholder(3, PlaceholderKind.Skolem, t"x", 0, t"Test.scala:5", t"x.type"))

    test(m"A placeholder with an escaped pipe decodes"):
      Placeholder.decode(t"1|error|a%007cb|0||a|b")
    . assert(_ == Unset)

    test(m"An escaped pipe within a field decodes to a literal pipe"):
      Placeholder.decode(t"1|error|a%007cb|0||printed")
    . assert(_ == Placeholder(1, PlaceholderKind.Error, t"a|b", 0, Unset, t"printed"))

    test(m"A non-numeric placeholder id is rejected"):
      Placeholder.decode(t"x|error|a|0||printed")
    . assert(_ == Unset)

    test(m"A placeholder with too few fields is rejected"):
      Placeholder.decode(t"1|error|a|0|")
    . assert(_ == Unset)

    test(m"An unknown placeholder kind is preserved"):
      Placeholder.decode(t"1|quantum|a|0||printed")
    . assert(_ == Placeholder(1, PlaceholderKind.Other(t"quantum"), t"a", 0, Unset, t"printed"))

    test(m"A placeholder reference is recognized"):
      Placeholder.reference(t"⟨scala-diag:42⟩")
    . assert(_ == 42)

    test(m"A non-placeholder literal is not a reference"):
      Placeholder.reference(t"⟨scala-diag:esc:7⟩")
    . assert(_ == Unset)

    test(m"An escaped literal unescapes"):
      Placeholder.escaped(t"⟨scala-diag:esc:real text⟩")
    . assert(_ == t"real text")

    test(m"A semantic message finds nested type markers"):
      val typed = mark(t"type", List(t"tasty" -> t"QUJD"), t"List[Int]")
      SemanticMessage.parse(t"Found: ${mark(t"name", Nil, t"value of $typed")}").types.length
    . assert(_ == 1)

    test(m"A message without markers is not marked"):
      SemanticMessage.marked(t"ordinary message")
    . assert(_ == false)

    test(m"A message with markers is marked"):
      SemanticMessage.marked(mark(t"sym", Nil, t"foo"))
    . assert(_ == true)
