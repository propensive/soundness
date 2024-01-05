/*
    Punctuation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package punctuation

import probably.*
import gossamer.*
import spectacular.*
import perforate.*
import symbolism.*
import rudiments.*
import anticipation.*
import vacuous.*

import errorHandlers.throwUnsafely


case class Example(str: Text, int: Int)

object Tests extends Suite(t"Punctuation tests"):

  import Markdown.Ast.Block.*, Markdown.Ast.Inline.*, Markdown.Ast.ListItem

  def run(): Unit =
    test(t"get a heading"):
      Markdown.parse(t"# Heading 1") match
        case Markdown(Heading(1, Copy(str))) => str
        case _                                  => t""
    .check(_ == t"Heading 1")

    test(t"get a level 2 heading"):
      Markdown.parse(t"## Heading 2")
    .assert(_ == Markdown(Heading(2, Copy(t"Heading 2"))))
    
    test(t"get a bullet list"):
      Markdown.parse(t" - Item 1\n - Item 2")
    .assert(_ == Markdown(BulletList(Unset, false, ListItem(Paragraph(Copy(t"Item 1"))),
        ListItem(Paragraph(Copy(t"Item 2"))))))
    
    test(t"get an ordered list"):
      Markdown.parse(t" 1. Item 1\n 2. Item 2")
    .assert(_ == Markdown(BulletList(1, false, ListItem(Paragraph(Copy(t"Item 1"))),
        ListItem(Paragraph(Copy(t"Item 2"))))))

    test(t"plain paragraph"):
      Markdown.parseInline(t"Here is some content in\na paragraph.")
    .assert(_ == Markdown(Copy(t"Here is some content in\na paragraph.")))
    
    test(t"directional apostrophe"):
      Markdown.parseInline(t"It's great.")
    .assert(_ == Markdown(Copy(t"It’s great.")))
    
    test(t"directional double-quotes"):
      Markdown.parseInline(t"""Some "quoted" text.""")
    .assert(_ == Markdown(Copy(t"Some “quoted” text.")))
    
    test(t"directional single-quotes"):
      Markdown.parseInline(t"""Some 'quoted' text.""")
    .assert(_ == Markdown(Copy(t"Some ‘quoted’ text.")))
    
    test(t"conversion of emdashes"):
      Markdown.parse(t"""An em-dash--so elegant!""")
    .assert(_ == Markdown(Paragraph(Copy(t"An em-dash—so elegant!"))))
    
    test(t"strongly emphasised text"):
      Markdown.parseInline(t"Here is some __strongly emphasised text__.")
    .assert(_ == Markdown(Copy(t"Here is some "),
        Strong(Copy(t"strongly emphasised text")), Copy(t".")))
    
    test(t"emphasised text"):
      Markdown.parseInline(t"Here is some *emphasised text*.")
    .assert(_ == Markdown(Copy(t"Here is some "), Emphasis(Copy(t"emphasised text")),
        Copy(t".")))
    
    test(t"some code"):
      Markdown.parseInline(t"Here is some `source code`.")
    .assert(_ == Markdown(Copy(t"Here is some "), SourceCode(t"source code"), Copy(t".")))
    
    test(t"a code block"):
      Markdown.parse(t"""```
                          |echo Hello World
                          |```""".s.stripMargin.show)
    .assert(_ == Markdown(FencedCode(Unset, Unset, t"echo Hello World\n")))
    
    test(t"a syntax-aware code block"):
      Markdown.parse(t"""```scala
                          |echo Hello World
                          |```""".s.stripMargin.show)
    .assert(_ == Markdown(FencedCode(t"scala", Unset, t"echo Hello World\n")))
    
    test(t"a link"):
      Markdown.parse(t"Take a look [here](http://example.com/)")
    .assert(_ == Markdown(Paragraph(Copy(t"Take a look "), Weblink(t"http://example.com/",
        Copy(t"here")))))
    
    test(t"an image"):
      Markdown.parse(t"Take a look ![alt text](http://example.com/image.jpg)")
    .assert(_ == Markdown(Paragraph(Copy(t"Take a look "), Image(t"alt text",
        t"http://example.com/image.jpg"))))
    
    test(t"a block quote"):
      Markdown.parse(t"> This paragraph is\n> indented.")
    .assert(_ == Markdown(Blockquote(Paragraph(Copy(t"This paragraph is\nindented.")))))
    
    test(t"indented content"):
      Markdown.parse(t"    This paragraph is\n    indented.\n")
    .assert(_ == Markdown(FencedCode(Unset, Unset, t"This paragraph is\nindented.\n")))
    
    test(t"hard linebreak"):
      Markdown.parse(t"Line 1  \nLine 2\n")
    .assert(_ == Markdown(Paragraph(Copy(t"Line 1"), Break(), Copy(t"Line 2"))))
    
    test(t"referenced images"):
      Markdown.parse(t"""![images reference][ref]
                       |
                       |[ref]: http://example.com/image.jpg
                       |""".s.stripMargin.show)
    .assert(_ == Markdown(Paragraph(Image(t"images reference", t"http://example.com/image.jpg")),
        Reference(t"ref", t"http://example.com/image.jpg")))
    
    test(t"referenced link"):
      Markdown.parse(t"""[link reference][ref]
                       |
                       |[ref]: http://example.com/
                       |""".s.stripMargin.show)
    .assert(_ == Markdown(Paragraph(Weblink(t"http://example.com/", Copy(t"link reference"))),
        Reference(t"ref", t"http://example.com/")))
    
    test(t"thematic break"):
      Markdown.parse(t"""Paragraph 1
                       |***
                       |Paragraph 2""".s.stripMargin.show)
    .assert(_ == Markdown(Paragraph(Copy(t"Paragraph 1")), ThematicBreak(),
        Paragraph(Copy(t"Paragraph 2"))))
    
    test(t"email link"):
      Markdown.parse(t"Email me <nobody@example.com>!")
    .assert(_ == Markdown(Paragraph(Copy(t"Email me "), Weblink(t"nobody@example.com",
        Copy(t"mailto:nobody@example.com")), Copy(t"!"))))
    
    test(t"interpolator"):
      md"Hello *World*"
    .assert(_ == Markdown(Copy(t"Hello "), Emphasis(Copy(t"World"))))
    
    // test(t"interpolator produces inline markdown"):
    //   md"Hello *world*!".hasType[InlineMd]
    // .assert(identity)

