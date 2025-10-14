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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package punctuation

import soundness.*

import strategies.throwUnsafely
import autopsies.contrastExpectations

case class Example(str: Text, int: Int)

object Tests extends Suite(m"Punctuation tests"):

  import Markdown.Ast.Block.*, Markdown.Ast.Inline.*, Markdown.Ast.ListItem

  def run(): Unit =
    test(m"get a heading"):
      t"# Heading 1".read[Md] match
        case Markdown(Heading(1, Prose(str))) => str
        case _                                  => t""
    .check(_ == t"Heading 1")

    test(m"get a level 2 heading"):
      t"## Heading 2".read[Md]
    .assert(_ == Markdown(Heading(2, Prose(t"Heading 2"))))

    test(m"get a bullet list"):
      t" - Item 1\n - Item 2".read[Md]
    .assert(_ == Markdown(BulletList(Unset, false, ListItem(Paragraph(Prose(t"Item 1"))),
        ListItem(Paragraph(Prose(t"Item 2"))))))

    test(m"get an ordered list"):
      t" 1. Item 1\n 2. Item 2".read[Md]
    .assert(_ == Markdown(BulletList(1, false, ListItem(Paragraph(Prose(t"Item 1"))),
        ListItem(Paragraph(Prose(t"Item 2"))))))

    test(m"plain paragraph"):
      t"Here is some content in\na paragraph.".read[InlineMd]
    .assert(_ == Markdown(Prose(t"Here is some content in\na paragraph.")))

    test(m"directional apostrophe"):
      t"It's great.".read[InlineMd]
    .assert(_ == Markdown(Prose(t"It’s great.")))

    test(m"directional double-quotes"):
      t"""Some "quoted" text.""".read[InlineMd]
    .assert(_ == Markdown(Prose(t"Some “quoted” text.")))

    test(m"directional single-quotes"):
      t"""Some 'quoted' text.""".read[InlineMd]
    .assert(_ == Markdown(Prose(t"Some ‘quoted’ text.")))

    test(m"conversion of emdashes"):
      t"""An em-dash--so elegant!""".read[Md]
    .assert(_ == Markdown(Paragraph(Prose(t"An em-dash—so elegant!"))))

    test(m"strongly emphasised text"):
      t"Here is some __strongly emphasised text__.".read[InlineMd]
    .assert(_ == Markdown(Prose(t"Here is some "),
        Strong(Prose(t"strongly emphasised text")), Prose(t".")))

    test(m"emphasised text"):
      t"Here is some *emphasised text*.".read[InlineMd]
    .assert(_ == Markdown(Prose(t"Here is some "), Emphasis(Prose(t"emphasised text")),
        Prose(t".")))

    test(m"some code"):
      t"Here is some `source code`.".read[InlineMd]
    .assert(_ == Markdown(Prose(t"Here is some "), SourceCode(t"source code"), Prose(t".")))

    test(m"a code block"):
      t"""```
         |echo Hello World
         |```""".s.stripMargin.show.read[Md]
    .assert(_ == Markdown(FencedCode(Unset, Unset, t"echo Hello World\n")))

    test(m"a syntax-aware code block"):
      t"""```scala
         |echo Hello World
         |```""".s.stripMargin.show.read[Md]
    .assert(_ == Markdown(FencedCode(t"scala", Unset, t"echo Hello World\n")))

    test(m"a link"):
      t"Take a look [here](http://example.com/)".read[Md]
    .assert(_ == Markdown(Paragraph(Prose(t"Take a look "), Weblink(t"http://example.com/",
        Prose(t"here")))))

    test(m"an image"):
      t"Take a look ![alt text](http://example.com/image.jpg)".read[Md]
    .assert(_ == Markdown(Paragraph(Prose(t"Take a look "), Image(t"alt text",
        t"http://example.com/image.jpg"))))

    test(m"a block quote"):
      t"> This paragraph is\n> indented.".read[Md]
    .assert(_ == Markdown(Blockquote(Paragraph(Prose(t"This paragraph is\nindented.")))))

    test(m"indented content"):
      t"    This paragraph is\n    indented.\n".read[Md]
    .assert(_ == Markdown(FencedCode(Unset, Unset, t"This paragraph is\nindented.\n")))

    test(m"hard linebreak"):
      t"Line 1  \nLine 2\n".read[Md]
    .assert(_ == Markdown(Paragraph(Prose(t"Line 1"), LineBreak, Prose(t"Line 2"))))

    test(m"referenced images"):
      t"""![images reference][ref]
         |
         |[ref]: http://example.com/image.jpg
         |""".s.stripMargin.show.read[Md]
    .assert(_ == Markdown(Paragraph(Image(t"images reference", t"http://example.com/image.jpg")),
        Reference(t"ref", t"http://example.com/image.jpg")))

    test(m"referenced link"):
      t"""[link reference][ref]
         |
         |[ref]: http://example.com/
         |""".s.stripMargin.show.read[Md]
    .assert(_ == Markdown(Paragraph(Weblink(t"http://example.com/", Prose(t"link reference"))),
        Reference(t"ref", t"http://example.com/")))

    test(m"thematic break"):
      t"""Paragraph 1
         |***
         |Paragraph 2""".s.stripMargin.show.read[Md]
    .assert(_ == Markdown(Paragraph(Prose(t"Paragraph 1")), ThematicBreak(),
        Paragraph(Prose(t"Paragraph 2"))))

    test(m"email link"):
      t"Email me <nobody@example.com>!".read[Md]
    .assert(_ == Markdown(Paragraph(Prose(t"Email me "), Weblink(t"nobody@example.com",
        Prose(t"mailto:nobody@example.com")), Prose(t"!"))))

    test(m"interpolator"):
      md"Hello *World*"
    .assert(_ == Markdown(Prose(t"Hello "), Emphasis(Prose(t"World"))))

    // test(m"interpolator produces inline markdown"):
    //   md"Hello *world*!".hasType[InlineMd]
    // .assert(identity)
