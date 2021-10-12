/*
    Punctuation, version 0.12.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import unsafeExceptions.canThrowAny

case class Example(str: String, int: Int)

object Tests extends Suite("Punctuation tests"):

  import Markdown.Ast.Block.*, Markdown.Ast.Inline.*, Markdown.Ast.ListItem

  def run(using Runner): Unit =
    test("get a heading") {
      Markdown.parse("# Heading 1") match
        case Markdown(Heading(1, Text(str))) => str
    }.assert(_ == "Heading 1")

    test("get a level 2 heading") {
      Markdown.parse("## Heading 2")
    }.assert(_ == Markdown(Heading(2, Text("Heading 2"))))
    
    test("get a bullet list") {
      Markdown.parse(" - Item 1\n - Item 2")
    }.assert(_ == Markdown(BulletList(None, false, ListItem(Paragraph(Text("Item 1"))),
        ListItem(Paragraph(Text("Item 2"))))))
    
    test("get an ordered list") {
      Markdown.parse(" 1. Item 1\n 2. Item 2")
    }.assert(_ == Markdown(BulletList(Some(1), false, ListItem(Paragraph(Text("Item 1"))),
        ListItem(Paragraph(Text("Item 2"))))))

    test("plain paragraph") {
      Markdown.parseInline("Here is some content in\na paragraph.")
    }.assert(_ == Markdown(Text("Here is some content in a paragraph.")))
    
    test("directional apostrophe") {
      Markdown.parseInline("It's great.")
    }.assert(_ == Markdown(Text("It’s great.")))
    
    test("directional double-quotes") {
      Markdown.parseInline("""Some "quoted" text.""")
    }.assert(_ == Markdown(Text("Some “quoted” text.")))
    
    test("directional single-quotes") {
      Markdown.parseInline("""Some 'quoted' text.""")
    }.assert(_ == Markdown(Text("Some ‘quoted’ text.")))
    
    test("conversion of emdashes") {
      Markdown.parse("""An em-dash--so elegant!""")
    }.assert(_ == Markdown(Paragraph(Text("An em-dash—so elegant!"))))
    
    test("strongly emphasised text") {
      Markdown.parseInline("Here is some __strongly emphasised text__.")
    }.assert(_ == Markdown(Text("Here is some "),
        Strong(Text("strongly emphasised text")), Text(".")))
    
    test("emphasised text") {
      Markdown.parseInline("Here is some *emphasised text*.")
    }.assert(_ == Markdown(Text("Here is some "), Emphasis(Text("emphasised text")),
        Text(".")))
    
    test("some code") {
      Markdown.parseInline("Here is some `source code`.")
    }.assert(_ == Markdown(Text("Here is some "), Code("source code"), Text(".")))
    
    test("a code block") {
      Markdown.parse("""```
        |echo Hello World
        |```""".stripMargin)
    }.assert(_ == Markdown(FencedCode(None, None, "echo Hello World\n")))
    
    test("a syntax-aware code block") {
      Markdown.parse("""```scala
        |echo Hello World
        |```""".stripMargin)
    }.assert(_ == Markdown(FencedCode(Some("scala"), None, "echo Hello World\n")))
    
    test("a link") {
      Markdown.parse("Take a look [here](http://example.com/)")
    }.assert(_ == Markdown(Paragraph(Text("Take a look "), Link("http://example.com/", Text("here")))))
    
    test("an image") {
      Markdown.parse("Take a look ![alt text](http://example.com/image.jpg)")
    }.assert(_ == Markdown(Paragraph(Text("Take a look "), Image("alt text",
        "http://example.com/image.jpg"))))
    
    test("a block quote") {
      Markdown.parse("> This paragraph is\n> indented.")
    }.assert(_ == Markdown(Blockquote(Paragraph(Text("This paragraph is indented.")))))
    
    test("indented content") {
      Markdown.parse("    This paragraph is\n    indented.\n")
    }.assert(_ == Markdown(FencedCode(None, None, "This paragraph is\nindented.\n")))
    
    test("hard linebreak") {
      Markdown.parse("Line 1  \nLine 2\n")
    }.assert(_ == Markdown(Paragraph(Text("Line 1"), Break(), Text("Line 2"))))
    
    test("referenced images") {
      Markdown.parse("""![images reference][ref]
                       |
                       |[ref]: http://example.com/image.jpg
                       |""".stripMargin)
    }.assert(_ == Markdown(Paragraph(Image("images reference", "http://example.com/image.jpg")),
        Reference("ref", "http://example.com/image.jpg")))
    
    test("referenced link") {
      Markdown.parse("""[link reference][ref]
                       |
                       |[ref]: http://example.com/
                       |""".stripMargin)
    }.assert(_ == Markdown(Paragraph(Link("http://example.com/", Text("link reference"))),
        Reference("ref", "http://example.com/")))
    
    test("thematic break") {
      Markdown.parse("""Paragraph 1
                       |***
                       |Paragraph 2""".stripMargin)
    }.assert(_ == Markdown(Paragraph(Text("Paragraph 1")), ThematicBreak(),
        Paragraph(Text("Paragraph 2"))))
    
    test("email link") {
      Markdown.parse("Email me <nobody@example.com>!")
    }.assert(_ == Markdown(Paragraph(Text("Email me "), Link("nobody@example.com",
        Text("mailto:nobody@example.com")), Text("!"))))
    
    test("interpolator") {
      md"Hello *World*"
    }.assert(_ == Markdown(Text("Hello "), Emphasis(Text("World"))))
