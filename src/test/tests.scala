/*
    Punctuation, version 0.12.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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
import rudiments.*
import eucalyptus.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

case class Example(str: Txt, int: Int)

object Tests extends Suite(str"Punctuation tests"):

  import Markdown.Ast.Block.*, Markdown.Ast.Inline.*, Markdown.Ast.ListItem

  def run(using Runner): Unit =
    test(str"get a heading") {
      Markdown.parse(str"# Heading 1") match
        case Markdown(Heading(1, Textual(str))) => str
    }.check(_ == str"Heading 1")

    test(str"get a level 2 heading") {
      Markdown.parse(str"## Heading 2")
    }.assert(_ == Markdown(Heading(2, Textual(str"Heading 2"))))
    
    test(str"get a bullet list") {
      Markdown.parse(str" - Item 1\n - Item 2")
    }.assert(_ == Markdown(BulletList(None, false, ListItem(Paragraph(Textual(str"Item 1"))),
        ListItem(Paragraph(Textual(str"Item 2"))))))
    
    test(str"get an ordered list") {
      Markdown.parse(str" 1. Item 1\n 2. Item 2")
    }.assert(_ == Markdown(BulletList(Some(1), false, ListItem(Paragraph(Textual(str"Item 1"))),
        ListItem(Paragraph(Textual(str"Item 2"))))))

    test(str"plain paragraph") {
      Markdown.parseInline(str"Here is some content in\na paragraph.")
    }.assert(_ == Markdown(Textual(str"Here is some content in a paragraph.")))
    
    test(str"directional apostrophe") {
      Markdown.parseInline(str"It's great.")
    }.assert(_ == Markdown(Textual(str"It’s great.")))
    
    test(str"directional double-quotes") {
      Markdown.parseInline(str"""Some "quoted" text.""")
    }.assert(_ == Markdown(Textual(str"Some “quoted” text.")))
    
    test(str"directional single-quotes") {
      Markdown.parseInline(str"""Some 'quoted' text.""")
    }.assert(_ == Markdown(Textual(str"Some ‘quoted’ text.")))
    
    test(str"conversion of emdashes") {
      Markdown.parse(str"""An em-dash--so elegant!""")
    }.assert(_ == Markdown(Paragraph(Textual(str"An em-dash—so elegant!"))))
    
    test(str"strongly emphasised text") {
      Markdown.parseInline(str"Here is some __strongly emphasised text__.")
    }.assert(_ == Markdown(Textual(str"Here is some "),
        Strong(Textual(str"strongly emphasised text")), Textual(str".")))
    
    test(str"emphasised text") {
      Markdown.parseInline(str"Here is some *emphasised text*.")
    }.assert(_ == Markdown(Textual(str"Here is some "), Emphasis(Textual(str"emphasised text")),
        Textual(str".")))
    
    test(str"some code") {
      Markdown.parseInline(str"Here is some `source code`.")
    }.assert(_ == Markdown(Textual(str"Here is some "), Code(str"source code"), Textual(str".")))
    
    test(str"a code block") {
      Markdown.parse(str"""```
                          |echo Hello World
                          |```""".s.stripMargin.show)
    }.assert(_ == Markdown(FencedCode(None, None, str"echo Hello World\n")))
    
    test(str"a syntax-aware code block") {
      Markdown.parse(str"""```scala
                          |echo Hello World
                          |```""".s.stripMargin.show)
    }.assert(_ == Markdown(FencedCode(Some(str"scala"), None, str"echo Hello World\n")))
    
    test(str"a link") {
      Markdown.parse(str"Take a look [here](http://example.com/)")
    }.assert(_ == Markdown(Paragraph(Textual(str"Take a look "), Link(str"http://example.com/",
        Textual(str"here")))))
    
    test(str"an image") {
      Markdown.parse(str"Take a look ![alt text](http://example.com/image.jpg)")
    }.assert(_ == Markdown(Paragraph(Textual(str"Take a look "), Image(str"alt text",
        str"http://example.com/image.jpg"))))
    
    test(str"a block quote") {
      Markdown.parse(str"> This paragraph is\n> indented.")
    }.assert(_ == Markdown(Blockquote(Paragraph(Textual(str"This paragraph is indented.")))))
    
    test(str"indented content") {
      Markdown.parse(str"    This paragraph is\n    indented.\n")
    }.assert(_ == Markdown(FencedCode(None, None, str"This paragraph is\nindented.\n")))
    
    test(str"hard linebreak") {
      Markdown.parse(str"Line 1  \nLine 2\n")
    }.assert(_ == Markdown(Paragraph(Textual(str"Line 1"), Break(), Textual(str"Line 2"))))
    
    test(str"referenced images") {
      Markdown.parse(str"""![images reference][ref]
                       |
                       |[ref]: http://example.com/image.jpg
                       |""".s.stripMargin.show)
    }.assert(_ == Markdown(Paragraph(Image(str"images reference", str"http://example.com/image.jpg")),
        Reference(str"ref", str"http://example.com/image.jpg")))
    
    test(str"referenced link") {
      Markdown.parse(str"""[link reference][ref]
                       |
                       |[ref]: http://example.com/
                       |""".s.stripMargin.show)
    }.assert(_ == Markdown(Paragraph(Link(str"http://example.com/", Textual(str"link reference"))),
        Reference(str"ref", str"http://example.com/")))
    
    test(str"thematic break") {
      Markdown.parse(str"""Paragraph 1
                       |***
                       |Paragraph 2""".s.stripMargin.show)
    }.assert(_ == Markdown(Paragraph(Textual(str"Paragraph 1")), ThematicBreak(),
        Paragraph(Textual(str"Paragraph 2"))))
    
    test(str"email link") {
      Markdown.parse(str"Email me <nobody@example.com>!")
    }.assert(_ == Markdown(Paragraph(Textual(str"Email me "), Link(str"nobody@example.com",
        Textual(str"mailto:nobody@example.com")), Textual(str"!"))))
    
    test(str"interpolator") {
      md"Hello *World*"
    }.assert(_ == Markdown(Textual(str"Hello "), Emphasis(Textual(str"World"))))
