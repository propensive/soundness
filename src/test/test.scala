/*

    Litterateur, version 0.4.0. Copyright 2019-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package litterateur

import probably._

object Tests extends Suite("Litterateur tests") {

  def run(test: Runner): Unit = {
    test("get a heading") {
      Markdown.parse("# Heading 1")
    }.assert(_ == Document(Heading(1, Text("Heading 1"))))
    
    test("get a level 2 heading") {
      Markdown.parse("## Heading 2")
    }.assert(_ == Document(Heading(2, Text("Heading 2"))))
    
    test("get a bullet list") {
      Markdown.parse(" - Item 1\n - Item 2")
    }.assert(_ == Document(BulletList(ListItem(Paragraph(Text("Item 1"))), ListItem(Paragraph(Text("Item 2"))))))
    
    test("get an ordered list") {
      Markdown.parse(" 1. Item 1\n 2. Item 2")
    }.assert(_ == Document(OrderedList(ListItem(Paragraph(Text("Item 1"))), ListItem(Paragraph(Text("Item 2"))))))

    test("plain paragraph") {
      Markdown.parse("Here is some content in\na paragraph.")
    }.assert(_ == Document(Paragraph(Text("Here is some content in\na paragraph."))))
    
    test("directional apostrophe") {
      Markdown.parse("It's great.")
    }.assert(_ == Document(Paragraph(Text("It’s great."))))
    
    test("directional double-quotes") {
      Markdown.parse("""Some "quoted" text.""")
    }.assert(_ == Document(Paragraph(Text("Some “quoted” text."))))
    
    test("directional single-quotes") {
      Markdown.parse("""Some 'quoted' text.""")
    }.assert(_ == Document(Paragraph(Text("Some ‘quoted’ text."))))
    
    test("conversion of emdashes") {
      Markdown.parse("""An em-dash--so elegant!""")
    }.assert(_ == Document(Paragraph(Text("An em-dash—so elegant!"))))
    
    test("strongly emphasised text") {
      Markdown.parse("Here is some __strongly emphasised text__.")
    }.assert(_ == Document(Paragraph(Text("Here is some "), StrongEmphasis(Text("strongly emphasised text")), Text("."))))
    
    test("emphasised text") {
      Markdown.parse("Here is some *emphasised text*.")
    }.assert(_ == Document(Paragraph(Text("Here is some "), Emphasis(Text("emphasised text")), Text("."))))
    
    test("some code") {
      Markdown.parse("Here is some `source code`.")
    }.assert(_ == Document(Paragraph(Text("Here is some "), Code(Text("source code")), Text("."))))
    
    test("a code block") {
      Markdown.parse("""```
        |echo Hello World
        |```""".stripMargin)
    }.assert(_ == Document(CodeBlock(None, Text("echo Hello World\n"))))
    
    test("a syntax-aware code block") {
      Markdown.parse("""```scala
        |echo Hello World
        |```""".stripMargin)
    }.assert(_ == Document(CodeBlock(Some("scala"), Text("echo Hello World\n"))))
    
    test("a link") {
      Markdown.parse("Take a look [here](http://example.com/)")
    }.assert(_ == Document(Paragraph(Text("Take a look "), Link("here", "http://example.com/"))))
    
    test("an image") {
      Markdown.parse("Take a look ![alt text](http://example.com/image.jpg)")
    }.assert(_ == Document(Paragraph(Text("Take a look "), Image("alt text", "http://example.com/image.jpg"))))
    
    test("a block quote") {
      Markdown.parse("> This paragraph is\n> indented.")
    }.assert(_ == Document(Blockquote(Paragraph(Text("This paragraph is\nindented.")))))
    
    test("indented content") {
      Markdown.parse("    This paragraph is\n    indented.\n")
    }.assert(_ == Document(Indented("This paragraph is\nindented.\n")))
    
    test("hard linebreak") {
      Markdown.parse("Line 1  \nLine 2\n")
    }.assert(_ == Document(Paragraph(Text("Line 1"), LineBreak, Text("Line 2"))))
    
    test("referenced images") {
      Markdown.parse("""![images reference][ref]
                       |
                       |[ref]: http://example.com/image.jpg
                       |""".stripMargin)
    }.assert(_ == Document(Paragraph(Image("images reference", "http://example.com/image.jpg")), Reference("ref", "http://example.com/image.jpg")))
    
    test("referenced link") {
      Markdown.parse("""[link reference][ref]
                       |
                       |[ref]: http://example.com/
                       |""".stripMargin)
    }.assert(_ == Document(Paragraph(Link("link reference", "http://example.com/")), Reference("ref", "http://example.com/")))
    
    test("thematic break") {
      Markdown.parse("""Paragraph 1
                       |***
                       |Paragraph 2""".stripMargin)
    }.assert(_ == Document(Paragraph(Text("Paragraph 1")), ThematicBreak, Paragraph(Text("Paragraph 2"))))
    
    test("email link") {
      Markdown.parse("Email me <nobody@example.com>!")
    }.assert(_ == Document(Paragraph(Text("Email me "), Link("nobody@example.com", "mailto:nobody@example.com"), Text("!"))))

    ()
  }
}

