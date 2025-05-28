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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package cellulose

import anticipation.*
import contingency.*
import eucalyptus.*, logging.stdout
import fulminate.*
import gossamer.*
import hieroglyph.*
import parasite.*, threadModels.virtual
import probably.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*, stdioSources.virtualMachine
import vacuous.*

import errorDiagnostics.stackTraces

import java.io as ji

import strategies.throwUnsafely

case class User(id: Int, email: Text, privilege: List[Privilege])
case class Privilege(name: Text, grant: Boolean)

object Tests extends Suite(m"CoDL tests"):

  given Realm = realm"tests"

  def run(): Unit = supervise:
    import CodlToken.*
    import Arity.*

    suite(m"Reader tests"):
      def interpret(text: Text): PositionReader = PositionReader(Stream(text))

      test(m"Character can store line"):
        Character('©', 123, 456).line
      .assert(_ == 123)

      test(m"Character can store column"):
        Character('©', 123, 456).column
      .assert(_ == 456)

      test(m"Character can store Char"):
        Character('©', 123, 456).char
      .assert(_ == '©')

      test(m"Initial position is line 0"):
        val reader = interpret(t"abc")
        reader.next().line
      .assert(_ == 0)

      test(m"Initial position is column 0"):
        val reader = interpret(t"abc")
        reader.next().column
      .assert(_ == 0)

      test(m"Initial character is correct"):
        val reader = interpret(t"abc")
        reader.next().char
      .assert(_ == 'a')

      test(m"Initial linefeed character is correct"):
        val reader = interpret(t"\nabc")
        reader.next().char
      .assert(_ == '\n')

      test(m"Initial carriage return and linefeed gives linefeed"):
        val reader = interpret(t"\r\nabc")
        reader.next().char
      .assert(_ == '\n')

      test(m"Character following CR/LF is correct"):
        val reader = interpret(t"\r\nabc")
        reader.next()
        reader.next().char
      .assert(_ == 'a')

      test(m"Read a character gives column 1"):
        val reader = interpret(t"abc")
        reader.next()
        reader.next().column
      .assert(_ == 1)

      test(m"Read a character gives correct line"):
        val reader = interpret(t"abc")
        reader.next()
        reader.next().line
      .assert(_ == 0)

      test(m"Character after newline gives correct line"):
        val reader = interpret(t"\nabc")
        reader.next()
        reader.next().line
      .assert(_ == 1)

      test(m"Character after newline gives correct column"):
        val reader = interpret(t"\nabc")
        reader.next()
        reader.next().column
      .assert(_ == 0)

      test(m"Read a CR/LF character gives correct line"):
        val reader = interpret(t"\r\nabc")
        reader.next()
        reader.next().line
      .assert(_ == 1)

      test(m"character after CR/LF gives correct column"):
        val reader = interpret(t"\r\nabc")
        reader.next()
        reader.next().column
      .assert(_ == 0)

      test(m"after LF next newline does not fail"):
        val reader = interpret(t"a\nbc\n")
        for i <- 0 until 4 do reader.next()
        reader.next().char
      .assert(_ == '\n')

      test(m"after LF next newline must not include CR"):
        val reader = interpret(t"a\nbc\r\n")
        for i <- 0 until 4 do reader.next()
        capture[CodlError](reader.next().char)
      .matches:
        case CodlError(_, _, _, CodlError.Reason.CarriageReturnMismatch(false)) =>

      test(m"after CR/LF next CR/LF does not fail"):
        val reader = interpret(t"a\r\nbc\r\n")
        for i <- 0 until 4 do reader.next()
        reader.next().char
      .assert(_ == '\n')

      test(m"after CR/LF next newline must include CR"):
        val reader = interpret(t"a\r\nbc\n")
        for i <- 0 until 4 do reader.next()
        capture[CodlError](reader.next().char)
      .matches:
        case CodlError(_, _, _, CodlError.Reason.CarriageReturnMismatch(true)) =>

      test(m"can capture start of text"):
        val reader = interpret(t"abcdef")
        reader.put(reader.next())
        reader.put(reader.next())
        reader.put(reader.next())
        reader.get()
      .assert(_ == t"abc")

      test(m"capture is empty after get"):
        val reader = interpret(t"abcdef")
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
      .assert(_ == t"def")

      test(m"capture position is correct"):
        val reader = interpret(t"abcdef")
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
        for i <- 0 until 3 do reader.put(reader.next())
        reader.get()
      .assert(_ == t"def")

      test(m"read end character"):
        val reader = interpret(t"")
        reader.next()
      .matches:
        case Character.End =>

      // test(m"cannot read end twice"):
      //   val reader = interpret(t"")
      //   reader.next()
      //   capture(reader.next())
      // .matches:
      //   case _: IllegalStateException =>

    suite(m"Tokenizer tests"):
      def parseText(text: Text): (Int, Stream[CodlToken]) =
        val result = Codl.tokenize(Stream(text))
        result(1).length
        result

      test(m"Parse two words with single space"):
        parseText(t"alpha beta")
      .assert(_ == (0, Stream(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))

      test(m"Parse a completely empty document"):
        parseText(t"")
      .assert(_ == (0, Stream()))

      test(m"Parse two words with trailing spaces"):
        parseText(t"alpha beta   ")
      .assert(_ == (0, Stream(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))

      test(m"Parse two words with three spaces"):
        parseText(t"alpha   beta")
      .assert(_ == (0, Stream(Item(t"alpha", 0, 0), Item(t"beta", 0, 8))))

      test(m"Parse two words with newline"):
        parseText(t"alpha beta\n")
      .assert(_ == (0, Stream(Item(t"alpha", 0, 0), Item(t"beta", 0, 6))))

      test(m"Parse two words with two lines"):
        parseText(t"alpha\nbeta")
      .assert(_ == (0, Stream(Item(t"alpha", 0, 0), Peer, Item(t"beta", 1, 0))))

      test(m"Parse two words on two lines with indentation"):
        parseText(t"alpha\n  beta")
      .assert(_ == (0, Stream(Item(t"alpha", 0, 0), Indent, Item(t"beta", 1, 2))))

      test(m"Parse two words on two lines with initial indentation"):
        parseText(t" alpha\n   beta")
      .assert(_ == (1, Stream(Item(t"alpha", 0, 1), Indent, Item(t"beta", 1, 3))))

      test(m"Parse two words on two lines with initial newline"):
        parseText(t"\nalpha\n  beta")
      .assert(_ == (0, Stream(Item(t"alpha", 1, 0), Indent, Item(t"beta", 2, 2))))

      test(m"Parse text with whitespace on blank lines"):
        parseText(t"root\n  two\n\n \npeer")
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Indent, Item(t"two", 1, 2), Blank, Blank,
          Outdent(1), Item(t"peer", 4, 0))))

      test(m"Parse shebang"):
        parseText(t"""|#!/bin/bash
                      |root
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Comment(t"!/bin/bash", 0, 0), Peer, Item(t"root", 1, 0))))

      test(m"Parse initial comment"):
        parseText(t"""|# Initial comment
                      |root""".s.stripMargin.show)
      .assert(_ == (0, Stream(Comment(t" Initial comment", 0, 0), Peer, Item(t"root", 1, 0))))

      test(m"Parse two-line comment"):
        parseText(t"""|# Line 1
                      |# Line 2
                      |root""".s.stripMargin.show)
      .assert(_ == (0, Stream(Comment(t" Line 1", 0, 0), Peer, Comment(t" Line 2", 1, 0),
          Peer, Item(t"root", 2, 0))))

      test(m"Parse remark"):
        parseText(t"""|root # remark""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Comment(t"remark", 0, 6))))

      test(m"Parse non-remark"):
        parseText(t"""|root #not-a-remark""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"#not-a-remark", 0, 5))))

      test(m"Parse multi-word remark"):
        parseText(t"""|root # remark words""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Comment(t"remark words", 0, 6))))

      test(m"Parse double indentation"):
        parseText(t"""|root
                      |    child content
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"child content", 1, 4, true))))

      test(m"Unindented line does not terminate long content"):
        parseText(t"""|root
                      |    one
                      |    two
                      |
                      |    three""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"one\ntwo\n\nthree", 1, 4, true))))


      test(m"Parse double indentation then peer"):
        parseText(t"""|root
                      |    child content
                      |next
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"child content", 1, 4, true), Peer,
          Item(t"next", 2, 0))))

      test(m"Parse double indentation then peer as children"):
        parseText(t"""|root
                      |  child
                      |      content
                      |  next
                      |""".s.stripMargin.show)(1).to(List)
      .assert(_ == List(Item(t"root", 0, 0), Indent, Item(t"child", 1, 2, false),
          Item(t"content", 2, 6, true), Peer, Item(t"next", 3, 2, false)))

      test(m"Parse triple indentation then peer as children"):
        parseText(t"""|root
                      |  child
                      |    grandchild
                      |        content
                      |    next
                      |""".s.stripMargin.show)(1).to(List)
      .assert(_ == Stream(Item(t"root", 0, 0), Indent, Item(t"child", 1, 2, false), Indent,
          Item(t"grandchild", 2, 4, false), Item(t"content", 3, 8, true), Peer,
          Item(t"next", 4, 4, false)).to(List))

      test(m"Parse triple indentation then outdent"):
        parseText(t"""|root
                      |  child
                      |    grandchild
                      |        content
                      |  next
                      |""".s.stripMargin.show)(1).to(List)
      .assert(_ == List(Item(t"root", 0, 0), Indent, Item(t"child", 1, 2, false), Indent,
          Item(t"grandchild", 2, 4, false), Item(t"content", 3, 8, true), Outdent(1),
          Item(t"next", 4, 2, false)))

      test(m"Parse triple indentation then double outdent"):
        parseText(t"""|root
                      |  child
                      |    grandchild
                      |        content
                      |next
                      |""".s.stripMargin.show)(1).to(List)
      .assert(_ == List(Item(t"root", 0, 0), Indent, Item(t"child", 1, 2, false), Indent,
          Item(t"grandchild", 2, 4, false), Item(t"content", 3, 8, true), Outdent(2),
          Item(t"next", 4, 0, false)))

      test(m"Parse double indentation then peer with margin"):
        parseText(t"""| root
                      |     child content
                      | next
                      |""".s.stripMargin.show)
      .assert(_ == (1, Stream(Item(t"root", 0, 1), Item(t"child content", 1, 5, true), Peer,
          Item(t"next", 2, 1))))

      test(m"Parse double indentation then peer with margin and indent"):
        parseText(t"""| root
                      |     child content
                      |   next
                      |""".s.stripMargin.show)
      .assert(_ == (1, Stream(Item(t"root", 0, 1), Item(t"child content", 1, 5, true), Indent,
          Item(t"next", 2, 3))))

      test(m"Parse multiline content"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true))))

      test(m"Parse multiline content then peer"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |next
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Peer,
          Item(t"next", 3, 0))))

      test(m"Terminated content"):
        parseText(t"""|root child
                      |##
                      |""".s.stripMargin.show)(1)
      .assert(_ == Stream(Item(t"root", 0, 0), Item(t"child", 0, 5), Peer, Body(Stream())))

      test(m"Terminated content 2"):
        parseText(t"root\n  one two\n##\n")(1)

      . assert(_ == Stream(Item(t"root", 0, 0), Indent, Item(t"one", 1, 2), Item(t"two", 1, 6), Outdent(1), Body(Stream())))

      test(m"Terminated content after child"):
        parseText(t"""|root
                      |  child
                      |##
                      |""".s.stripMargin.show)(1)
      .assert(_ == Stream(Item(t"root", 0, 0), Indent, Item(t"child", 1, 2), Outdent(1), Body(Stream())))

      test(m"Terminated content after long parameter"):
        parseText(t"""|root
                      |    child
                      |##
                      |""".s.stripMargin.show)(1).to(List)
      .assert(_ == Stream(Item(t"root", 0, 0), Item(t"child", 1, 4, true), Peer, Body(Stream())).to(List))

      test(m"Terminated content with body"):
        parseText(t"""|root
                      |##
                      | follow""".s.stripMargin.show)(1)
      .assert(_ == Stream(Item(t"root", 0, 0), Peer, Body(Stream(' ', 'f', 'o', 'l', 'l', 'o', 'w'))))

      test(m"Terminated content with body and newline"):
        parseText(t"""|root
                      |##
                      | follow
                      |""".s.stripMargin.show)(1)
      .assert(_ == Stream(Item(t"root", 0, 0), Peer, Body(Stream(' ', 'f', 'o', 'l', 'l', 'o', 'w', '\n'))))

      test(m"Parse multiline content then indent"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |  next
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Indent,
          Item(t"next", 3, 2))))

      test(m"Parse multiline content then indented comment"):
        parseText(t"""|root
                      |    child content
                      |    more
                      |  # comment
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"child content\nmore", 1, 4, true), Indent,
          Comment(t" comment", 3, 2))))

      test(m"Parse multiline content including a hash"):
        parseText(t"""|root
                      |    content
                      |    # not a comment
                      |""".s.stripMargin.show)
      .assert: value =>
        value == (0, Stream(Item(t"root", 0, 0), Item(t"content\n# not a comment", 1, 4, true)))

      test(m"Parse multiline content including a additional indentation"):
        parseText(t"""|root
                      |    content
                      |     indented
                      |""".s.stripMargin.show)
      .assert(_ == (0, Stream(Item(t"root", 0, 0), Item(t"content\n indented", 1, 4, true))))

      test(m"Surplus indentation"):
        parseText(t"""|root
                      |     surplus-indented
                      |""".s.stripMargin.show)(1)
      .assert(_.has(CodlToken.Error(CodlError(1, 5, 1, CodlError.Reason.SurplusIndent))))

      test(m"Uneven indentation"):
        parseText(t"""|root
                      | uneven indented
                      |""".s.stripMargin.show)(1)
      .assert(_.has(CodlToken.Error(CodlError(1, 1, 1, CodlError.Reason.UnevenIndent(0, 1)))))

      test(m"Uneven indentation 2"):
        parseText(t"""|root
                      |   uneven indented
                      |""".s.stripMargin.show)(1)
      .assert(_.has(CodlToken.Error(CodlError(1, 3, 1, CodlError.Reason.UnevenIndent(0, 3)))))

      test(m"Insufficient indentation"):
        parseText(t"""|     root
                      |    uneven indented
                      |""".s.stripMargin.show)(1)
      .assert(_.has(CodlToken.Error(CodlError(1, 4, 1, CodlError.Reason.InsufficientIndent))))

      test(m"Uneven de-indentation"):
        parseText(t"""|root
                      |  child
                      | deindentation
                      |""".s.stripMargin.show)(1)
      .assert(_.has(CodlToken.Error(CodlError(2, 1, 1, CodlError.Reason.UnevenIndent(0, 1)))))

    suite(m"Access tests"):
      import dynamicCodlAccess.enabled
      val doc = CodlDoc(
        CodlNode(t"term")(
          CodlNode(t"name")(CodlNode(t"alpha")(), CodlNode(t"beta")()),
          CodlNode(t"name")(CodlNode(t"gamma")()),
          CodlNode(t"kind")(
            CodlNode(t"query")(CodlNode(t"value")())
          )
        ),
        CodlNode(t"element")()
      )

      test(m"Access first element"):
        doc.term().key
      .assert(_ == t"term")

      test(m"Access second element"):
        doc.element().key
      .assert(_ == t"element")

      test(m"Access nested element"):
        doc.term().kind().key
      .assert(_ == t"kind")

      test(m"Access multiple keys"):
        doc.term().name.length
      .assert(_ == 2)

      test(m"Access deeper nested key"):
        doc.term(0).name(1)()
      .assert(_ == CodlNode(Data(t"gamma")))

      test(m"Access deeper nested param"):
        doc.term().name()(1)
      .assert(_ == CodlNode(Data(t"beta")))

    def read(text: Text): CodlDoc = Codl.parse(text)

    suite(m"Untyped parsing tests"):
      test(m"Empty document"):
        read(t"")
      .assert(_ == CodlDoc())

      test(m"Simplest non-empty document"):
        read(t"root").wiped
      .assert(_ == CodlDoc(CodlNode(Data(t"root"))))

      test(m"Root peers"):
        read(t"root\nelement\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(), CodlNode(t"element")()))

      test(m"Single child"):
        read(t"root\n  child").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")())))

      test(m"Single child and peer"):
        read(t"root\n  child\npeer").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")()), CodlNode(t"peer")()))

      test(m"Single child and grandchild"):
        read(t"root\n  child\n    grandchild").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")(CodlNode(t"grandchild")()))))

      test(m"Single child and grandchild and peer"):
        read(t"root\n  child\n    grandchild\npeer").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")(CodlNode(t"grandchild")())), CodlNode(t"peer")()))

      test(m"Peers at multiple levels"):
        read(t"root\n  child\n    grandchild\n  child\n    grandchild\npeer").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")(CodlNode(t"grandchild")()),
          CodlNode(t"child")(CodlNode(t"grandchild")())), CodlNode(t"peer")()))

      test(m"Data with parameter"):
        read(t"root param").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"param")())))

      test(m"Data with remark"):
        read(t"root # remark").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root"), Extra(0, Nil, t"remark"))))

      test(m"Data after comment"):
        read(t"# comment\nroot").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root"), Extra(0, List(t" comment"), Unset))))

      test(m"Data after two comments"):
        read(t"# comment 1\n# comment 2\nroot").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root"), Extra(0, List(t" comment 1", t" comment 2"), Unset))))

      test(m"Comment on child"):
        read(t"root\n  # comment\n  child").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(0, List(t" comment"),
          Unset)))))))

      test(m"Comment and blank line on child"):
        read(t"root\n\n  # comment\n  child").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(1, List(t" comment"),
          Unset)))))))

      test(m"Data with multiple parameters"):
        read(t"root param1 param2").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"param1")(), CodlNode(t"param2")())))

      test(m"Blank line before child"):
        read(t"root\n\n  child").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(1, Nil)))))))

      test(m"Two blank lines before child"):
        read(t"root\n\n \n  child").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child", IArray()), Extra(2, Nil, Unset)))))))

      test(m"Data with multiple parameters, remark and comment"):
        read(t"# comment\nroot param1 param2 # remark").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(t"param1")(), CodlNode(t"param2")())), Extra(0,
          List(t" comment"), t"remark"))))

      test(m"Data with multiple parameters, remark, comment and peer"):
        read(t"# comment\nroot param1 param2 # remark\npeer").untyped
      .assert(_ == CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(t"param1")(), CodlNode(t"param2")())), Extra(0,
          List(t" comment"), t"remark")), CodlNode(t"peer")()))

      test(m"Comment on blank node"):
        read(t"# comment\n\nroot").untyped
      .assert(_ == CodlDoc(CodlNode(Unset, Extra(0, List(t" comment"), Unset)), CodlNode(Data(t"root"), Extra(1,
          Nil, Unset))))

      test(m"Remark after blank line"):
        read(t"root\n\npeer # remark").untyped
      .assert(_ == CodlDoc(CodlNode(t"root")(), CodlNode(Data(t"peer"), Extra(0, Nil, t"remark"))))

      test(m"Long item"):
        read(t"root\n    one two\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one two")())))

      test(m"Unindented blank line does not terminate long content"):
        read(t"root\n    one\n    two\n\n    three").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one\ntwo\n\nthree")())))

      test(m"Multiline item"):
        read(t"root\n  child\n    this\n        one two\n        three four\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")(CodlNode(t"this")(CodlNode(t"one two\nthree four")())))))

      test(m"Terminated content"):
        read(t"ROOT\n  one two\n##\nfoobar\nbaz").wiped
      .assert(_ == CodlDoc(CodlNode(t"ROOT")(CodlNode(t"one")(CodlNode(t"two")()))))

      test(m"Terminated content without newline"):
        read(t"root\n    one two\n##").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one two")())))

      test(m"Terminated content with body"):
        read(t"root\n    one two\n##\nunparsed").body
      .assert(_ == Stream('u', 'n', 'p', 'a', 'r', 's', 'e', 'd'))

      test(m"Terminated content with newline before body"):
        read(t"root\n    one two\n##\n\nunparsed").body
      .assert(_ == Stream('\n', 'u', 'n', 'p', 'a', 'r', 's', 'e', 'd'))

      test(m"Terminated content with newline after body"):
        read(t"root\n    one two\n##\nunparsed\n").body
      .assert(_ == Stream('u', 'n', 'p', 'a', 'r', 's', 'e', 'd', '\n'))

      test(m"Terminator without newline and spurious content"):
        try read(t"root\n    one two\n##spurious") yet Unset
        catch case error: CodlError => error
      .assert(_ == CodlError(2, 2, 1, BadTermination))


    suite(m"Schema tests"):
      import dynamicCodlAccess.enabled
      val childSchema = Struct(AtMostOne,
                          t"one" -> Field(AtMostOne),
                          t"two" -> Field(AtMostOne)
                        )

      val grandchildSchema = Struct(AtMostOne, t"data" -> Field(AtMostOne))

      val childSchema2 = Struct(AtMostOne,
                           t"alpha" -> grandchildSchema,
                           t"beta" -> Field(AtMostOne)
                         )

      val rootSchema = Struct(AtMostOne,
                         t"child" -> childSchema,
                         t"second" -> childSchema2,
                         t"third" -> Field(AtMostOne),
                         t"other" -> Struct(AtMostOne,
                           t"param" -> Field(AtMostOne),
                         )
                       )

      val topSchema = Struct(AtMostOne, t"root" -> rootSchema)

      test(m"Root node has correct name"):
        topSchema.parse(t"root").untyped()
      .assert(_ == CodlNode(Data(t"root")))

      test(m"Root node has correct schema"):
        topSchema.parse(t"root").schema
      .assert(_ == topSchema)

      test(m"First child of root param is validated"):
        topSchema.parse(t"root\n  child").root()(0).schema
      .assert(_ == childSchema)

      test(m"Third child of root param is validated"):
        val doc = topSchema.parse(t"root\n  child\n  second\n  third value\n")
        doc.root()(2).schema
      .assert(_ == Field(AtMostOne))

      test(m"Second child of root param is validated"):
        val doc = topSchema.parse(t"root\n  child\n  second")
        doc.root()(1).schema
      .assert(_ == childSchema2)

      test(m"Child is validated"):
        topSchema.parse(t"root\n  child").root().child().schema
      .assert(_ == childSchema)

      test(m"Different-named child is validated"):
        topSchema.parse(t"root\n  second").root().second().schema
      .assert(_ == childSchema2)

      test(m"Grandchild nodes are validated"):
        topSchema.parse(t"root\n  second\n    alpha").root().second().alpha().schema
      .assert(_ == grandchildSchema)

      test(m"Invalid top-level node"):
        capture(topSchema.parse(t"riot"))
      .assert(_ == CodlError(0, 0, 4, InvalidKey(t"riot", t"riot")))

      test(m"Indent after comment forbidden"):
        capture(Codl.parse(t"root\n  # comment\n    child"))
      .assert(_ == CodlError(1, 2, 1, CodlError.Reason.IndentAfterComment))

      test(m"Validate second top-level node"):
        rootSchema.parse(t"child\nsecond").second().schema
      .assert(_ == childSchema2)

      test(m"Validate second top-level node out of order"):
        rootSchema.parse(t"second\nchild").second().schema
      .assert(_ == childSchema2)

      test(m"Validate second top-level node after child"):
        rootSchema.parse(t"child\n  one\nsecond").second().schema
      .assert(_ == childSchema2)

      val requiredChild = Struct(AtMostOne,
                            t"root" -> Struct(AtMostOne,
                              t"child" -> Field(One)
                            )
                          )

      test(m"Missing required node throws exception"):
        capture(requiredChild.parse(t"root"))
      .assert(_ == CodlError(0, 0, 4, MissingKey(t"root", t"child")))

      test(m"Present required node does not throw exception"):
        requiredChild.parse(t"root\n  child").untyped.root().child()
      .assert(_ == Data(t"child"))

      val repeatableChild = Struct(AtMostOne,
                              t"root" -> Struct(AtMostOne,
                                t"child" -> Field(Many)
                              )
                            )

      test(m"Duplicated unique child is forbidden"):
        capture[CodlError](requiredChild.parse(t"root\n  child\n  child"))
      .assert(_ == CodlError(2, 2, 5, DuplicateKey(t"child", t"child")))

      test(m"Duplicated repeatable child is permitted"):
        repeatableChild.parse(t"root\n  child\n  child").untyped.root().child(1)
      .assert(_ == Data(t"child"))

      val atLeastOneChild = Struct(AtMostOne,
                              t"root" -> Struct(AtMostOne,
                                t"child" -> Field(AtLeastOne)
                              )
                            )

      test(m"'At least one' may mean one"):
        atLeastOneChild.parse(t"root\n  child").untyped.root().child()
      .assert(_ == Data(t"child"))

      test(m"'At least one' may mean two"):
        atLeastOneChild.parse(t"root\n  child\n  child").untyped.root().child(1)
      .assert(_ == Data(t"child"))

      test(m"'At least one' may not mean zero"):
        capture[CodlError](requiredChild.parse(t"root"))
      .assert(_ == CodlError(0, 0, 4, MissingKey(t"root", t"child")))

      def childWithTwoParams(alpha: Arity, beta: Arity) =
        Struct(AtMostOne,
          t"root" -> Struct(AtMostOne,
            t"child" -> Struct(AtMostOne,
              t"alpha" -> Field(alpha),
              t"beta" -> Field(beta)
            )
          )
        )

      def rootWithTwoParams(alpha: Arity, beta: Arity) =
        Struct(AtMostOne,
          t"child" -> Struct(AtMostOne,
            t"alpha" -> Field(alpha),
            t"beta" -> Field(beta)
          )
        )

      test(m"Access parameters by name"):
        childWithTwoParams(One, One).parse(t"root\n  child first second").root().child().beta()
      .assert(_ == Data(t"second", IArray(), schema = Field(One), layout = Layout(0, false, 14)))

      test(m"Surplus parameters"):
        capture(childWithTwoParams(One, One).parse(t"root\n  child one two three"))
      .assert(_ == CodlError(1, 16, 5, SurplusParams(t"three", t"child")))

      test(m"Two surplus parameters"):
        capture(childWithTwoParams(One, One).parse(t"root\n  child one two three four"))
      .assert(_ == CodlError(1, 16, 5, SurplusParams(t"three", t"child")))

      test(m"Two optional parameters not specified"):
        childWithTwoParams(AtMostOne, AtMostOne).parse(t"root\n  child").root().child().layout
        . params
      .assert(_ == 0)

      test(m"Two optional parameters with one specified"):
        childWithTwoParams(AtMostOne, AtMostOne).parse(t"root\n  child one").root().child().layout.params
      .assert(_ == 1)

      test(m"Two optional parameters with both specified"):
        childWithTwoParams(AtMostOne, AtMostOne).parse(t"root\n  child one two").root().child().layout.params
      .assert(_ == 2)

      test(m"Two optional parameters with one surplus"):
        capture(childWithTwoParams(AtMostOne, AtMostOne).parse(t"root\n  child one two three").root().child())
      .assert(_ == CodlError(1, 16, 5, SurplusParams(t"three", t"child")))

      test(m"Variadic parameters are counted"):
        childWithTwoParams(One, Many).parse(t"root\n  child one two three four").root().child().layout.params
      .assert(_ == 4)

      test(m"Variadic parameter has right number of values"):
        childWithTwoParams(One, Many).parse(t"root\n  child one two three four").root().child().beta.length
      .assert(_ == 3)

      test(m"Variadic parameters are optional"):
        childWithTwoParams(One, Many).parse(t"root\n  child one").root().child().layout.params
      .assert(_ == 1)

      test(m"'at least one' parameters are not optional"):
        capture(childWithTwoParams(One, AtLeastOne).parse(t"root\n  child one"))
      .assert(_ == CodlError(1, 2, 5, MissingKey(t"child", t"beta")))

      test(m"Variadic first parameters don't count for second"):
        capture(childWithTwoParams(AtLeastOne, AtLeastOne).parse(t"root\n  child one two three"))
      .assert(_ == CodlError(1, 2, 5, MissingKey(t"child", t"beta")))

      test(m"Two optional parameters not specified on root"):
        rootWithTwoParams(AtMostOne, AtMostOne).parse(t"  child").child().layout.params
      .assert(_ == 0)

      test(m"Two optional parameters with one specified on root"):
        rootWithTwoParams(AtMostOne, AtMostOne).parse(t"  child one").child().layout.params
      .assert(_ == 1)

      test(m"Two optional parameters with both specified on root"):
        rootWithTwoParams(AtMostOne, AtMostOne).parse(t"  child one two").child().layout.params
      .assert(_ == 2)

      test(m"Two optional parameters with one surplus on root"):
        capture(rootWithTwoParams(AtMostOne, AtMostOne).parse(t"  child one two three").child())
      .assert(_ == CodlError(0, 16, 5, SurplusParams(t"three", t"child")))

      test(m"Variadic parameters are counted on root"):
        rootWithTwoParams(One, Many).parse(t"  child one two three four").child().layout.params
      .assert(_ == 4)

      test(m"Variadic parameter has right number of values on root"):
        rootWithTwoParams(One, Many).parse(t"child one two three four").child().beta.length
      .assert(_ == 3)

      test(m"Variadic parameters are optional on root"):
        rootWithTwoParams(One, Many).parse(t"  child one").child().layout.params
      .assert(_ == 1)

      test(m"'at least one' parameters are not optional on root"):
        capture(rootWithTwoParams(One, AtLeastOne).parse(t"  child one"))
      .assert(_ == CodlError(0, 2, 5, MissingKey(t"child", t"beta")))

      test(m"Variadic first parameters don't count for second on root"):
        capture(rootWithTwoParams(AtLeastOne, AtLeastOne).parse(t"  child one two three"))
      .assert(_ == CodlError(0, 2, 5, MissingKey(t"child", t"beta")))

    suite(m"Path tests"):
      import dynamicCodlAccess.enabled
      val schema = Struct(AtMostOne,
        t"root" -> Struct(AtMostOne,
          t"field" -> Struct(AtMostOne,
            t"id"   -> Field(Unique),
            t"name" -> Field(One),
            t"description" -> Field(One)
          ),
          t"another" -> Struct(Arity.Many,
            t"id"      -> Field(Unique),
            t"name"    -> Field(One),
            t"color"   -> Field(AtMostOne),
            t"size"    -> Field(AtMostOne)
          )
        )
      )


      test(m"CodlNode contains reference to an ident param"):
        schema.parse(t"""root\n  field apple Braeburn red""").root().ids
      .assert(_ == Set(t"apple"))

      test(m"CodlNode contains reference to an ident param with children"):
        schema.parse(t"""root\n  field apple Braeburn\n    description red""").root().ids
      .assert(_ == Set(t"apple"))

      test(m"CodlNode contains reference to an ident param as a child"):
        schema.parse(t"""root\n  another\n    id jack\n    name Jack\n    color blue\n    size 8""").root().ids
      .assert(_ == Set(t"jack"))

      test(m"CodlNode contains multiple ids"):
        schema.parse(t"""root\n  another jill\n    name Jill\n    color foo\n  another\n    id jack\n    name Jack\n""").root().ids
      .assert(_ == Set(t"jack", t"jill"))

      test(m"CodlNode contains multiple ids (tweaked)"):
        schema.parse(t"""root\n  another jill\n    name Jill\n  another\n    id jack\n    name Jack\n    color blue\n    size 8""").root().ids
      .assert(_ == Set(t"jack", t"jill"))

      val repetitionSchema = Struct(AtMostOne,
        t"ABC" -> Struct(Many,
          t"DEF" -> Field(Unique),
          t"GHI" -> Field(One)
        )
      )

      test(m"Cannot have duplicate IDs of the same type"):
        capture(repetitionSchema.parse(t"ABC first One\nABC second Two\nABC first Primary"))
      .assert(_ == CodlError(2, 4, 5, CodlError.Reason.DuplicateId(t"first", 0, 4)))

    suite(m"Binary tests"):

      val schema = Struct(Arity.AtMostOne,
        t"field" -> Struct(Arity.AtMostOne,
          t"child" -> Field(AtMostOne)
        ),
        t"field2" -> Field(AtMostOne),
        t"field3" -> Field(AtMostOne)
      )

      def roundtrip(doc: CodlDoc): CodlDoc = Bcodl.read(doc.schema, ji.StringReader(doc.bcodl.s).nn)

      val doc = schema.parse(t"field")

      test(m"Serialize and deserialize one node"):
        roundtrip(doc)
      .assert(_ == doc.uncommented)

      val doc2 = schema.parse(t"field\n  child\n")
      test(m"Serialize and deserialize one node and child"):
        roundtrip(doc2)
      .assert(_ == doc2.uncommented)

      val doc3 = schema.parse(t"field\n  child value")
      test(m"Serialize and deserialize one node and child/param"):
        roundtrip(doc3)
      .assert(_ == doc3.uncommented)

      val doc4 = schema.parse(t"field\n  child value value2 value3")
      test(m"Serialize and deserialize one node and child with three params"):
        roundtrip(doc4)
      .assert(_ == doc4.uncommented)

      val doc5 = schema.parse(t"field\n  child value value2\nfield2\nfield3")
      test(m"Serialize and deserialize document with indent, outdent and peer"):
        roundtrip(doc5)
      .assert(_ == doc5.uncommented)

    suite(m"Serialization tests"):

      test(m"Serialize a node"):
        CodlDoc(CodlNode(Data(t"root"))).write
      .assert(_ == t"root\n")

      test(m"Serialize a node and a child"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child")))))).write
      .assert(_ == t"root\n  child\n")

      test(m"Serialize a node and a child with params layout"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"))), Layout(1, false, 0))))
        . write
      .assert(_ == t"root child\n")

      test(m"Serialize a node and a child with block param"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child")), CodlNode(Data(t"Hello World"))), Layout(2, true, 0)))).write
      .assert(_ == t"root child\n    Hello World\n")

      test(m"Serialize a node and a child with multiline block param"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child")), CodlNode(Data(t"Hello\nWorld"))), Layout(2, true, 0)))).write
      .assert(_ == t"root child\n    Hello\n    World\n")

      test(m"Serialize a node and a child with comment"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(comments = List(t" comment"))))))).write
      .assert(_ == t"root\n  # comment\n  child\n")

      test(m"Serialize a node and a child with multiline comment"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(comments = List(t" line 1", t" line 2"))))))).write
      .assert(_ == t"root\n  # line 1\n  # line 2\n  child\n")

      test(m"Serialize a node and a child with multiline comment and blank lines"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(blank = 2, comments = List(t" line 1", t" line 2"))))))).write
      .assert(_ == t"root\n\n\n  # line 1\n  # line 2\n  child\n")

      test(m"Serialize a node and a child with blank lines"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(blank = 2)))))).write
      .assert(_ == t"root\n\n\n  child\n")

      test(m"Serialize a node and a child with a remark"):
        CodlDoc(CodlNode(Data(t"root", IArray(CodlNode(Data(t"child"), Extra(remark = t"some remark")))))).write
      .assert(_ == t"root\n  child # some remark\n")

    suite(m"Double-spacing tests"):
      test(m"Single-space-separated"):
        read(t"root one two\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one")(), CodlNode(t"two")())))

      test(m"Double-space-separated"):
        read(t"root  one  two\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one")(), CodlNode(t"two")())))

      test(m"Short/long spacing"):
        read(t"root one  two\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one")(), CodlNode(t"two")())))

      test(m"Long/short spacing"):
        read(t"root  one two\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one two")())))

      test(m"Long/short/long spacing"):
        read(t"root  one two  three\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one two")(), CodlNode(t"three")())))

      test(m"Short/short/long spacing"):
        read(t"root one two  three\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one")(), CodlNode(t"two")(), CodlNode(t"three")())))

      test(m"Short/Long/short spacing"):
        read(t"root one  two three\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one")(), CodlNode(t"two three")())))

      test(m"Short/Long/short/Long/short spacing"):
        read(t"root one  two three  four five\n").wiped
      .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"one")(), CodlNode(t"two three")(), CodlNode(t"four five")())))


    def roundtrip[T: CodlEncoder: CodlDecoder](value: T): T = value.codl.as[T]

    suite(m"Generic Derivation tests"):

      case class Person(name: Text, age: Int)
      case class Organisation(name: Text, ceo: Person)
      case class Player(name: Text, rank: Optional[Int])

      test(m"write a simple case class"):
        Person(t"John Smith", 65).codl.untyped
      .assert(_ == CodlDoc(CodlNode(t"name")(CodlNode(t"John Smith")()), CodlNode(t"age")(CodlNode(t"65")())))

      test(m"write a nested case class"):
        val person1 = Person(t"Alpha", 1)
        Organisation(t"Acme", person1).codl.untyped
      .assert(_ == CodlDoc(CodlNode(t"name")(CodlNode(t"Acme")()), CodlNode(t"ceo")(CodlNode(t"name")(CodlNode(t"Alpha")()),
          CodlNode(t"age")(CodlNode(t"1")()))))

      test(m"write a case class with optional field specified"):
        Player(t"Barry", 1).codl.untyped
      .assert(_ == CodlDoc(CodlNode(t"name")(CodlNode(t"Barry")()), CodlNode(t"rank")(CodlNode(t"1")())))

      test(m"write a case class with optional field unspecified"):
        Player(t"Barry", Unset).codl.untyped
      .assert(_ == CodlDoc(CodlNode(t"name")(CodlNode(t"Barry")())))

      test(m"serialize a List of case classes"):
        List(Person(t"John Smith", 65), Person(t"Jim Calvin", 11)).codl.untyped
      .assert(_ == CodlDoc(
        CodlNode(t"name")(CodlNode(t"John Smith")()), CodlNode(t"age")(CodlNode(t"65")()),
        CodlNode(t"name")(CodlNode(t"Jim Calvin")()), CodlNode(t"age")(CodlNode(t"11")())
      ))

      test(m"serialize a List of integers"):
        List(1, 2, 3).codl.untyped
      .assert(_ == CodlDoc(CodlNode(t"1")(), CodlNode(t"2")(), CodlNode(t"3")()))

      test(m"serialize a List of booleans"):
        List(true, false, true).codl.untyped
      .assert(_ == CodlDoc(CodlNode(t"yes")(), CodlNode(t"no")(), CodlNode(t"yes")()))

      test(m"roundtrip a true boolean"):
        roundtrip[Boolean](true)
      .assert(_ == true)

      test(m"roundtrip a false boolean"):
        roundtrip[Boolean](false)
      .assert(_ == false)

      test(m"roundtrip an integer"):
        roundtrip[Int](42)
      .assert(_ == 42)

      test(m"roundtrip some text"):
        roundtrip[Text](t"hello world")
      .assert(_ == t"hello world")

      test(m"roundtrip a list of strings"):
        roundtrip[List[Text]](List(t"hello", t"world"))
      .assert(_ == List(t"hello", t"world"))

      case class Foo(alpha: Text, beta: Optional[Text])
      test(m"roundtrip a case class with an optional parameter"):
        roundtrip(Foo(t"one", t"two"))
      .assert(_ == Foo(t"one", t"two"))

      test(m"roundtrip a case class with an optional parameter, unset"):
        roundtrip(Foo(t"one", Unset))
      .assert(_ == Foo(t"one", Unset))

      case class Bar(foo: List[Baz], quux: Quux)
      case class Quux(alpha: Text, beta: List[Byte])
      case class Baz(gamma: Text, delta: Int, eta: Optional[Char])

      val complex = Bar(List(Baz(t"a", 2, Unset), Baz(t"c", 6, 'e')), Quux(t"e", List(1, 2, 4)))

      test(m"roundtrip a complex case class"):
        summon[CodlDecoder[Baz]]
        summon[CodlDecoder[List[Baz]]]
        roundtrip(complex)
      .assert(_ == complex)

      def print[T: CodlDecoder: CodlEncoder](value: T): Text =
        val writer = new ji.StringWriter()
        Printer.print(writer, value.codl)
        writer.toString().show

      test(m"print a simple case class"):
        print(Foo(t"one", t"two"))
      .assert(_ == t"alpha  one\nbeta  two\n")

      test(m"print a complex case class"):
        print(complex)
      .assert(_ == t"foo\n  gamma  a\n  delta  2\nfoo\n  gamma  c\n  delta  6\n  eta  e\nquux\n  alpha  e\n  beta  1\n  beta  2\n  beta  4\n")

      test(m"roundtrip a complex case class "):
        read(print(complex)).as[Bar]
      .assert(_ == complex)

      // test(m"Print a case class using positional parameters"):
      //   print(User(12, t"user@example.com", List(Privilege(t"read", true), Privilege(t"write", false))))
      // .assert(_ == t"")

    // suite(m"Extra tests"):
    //   test(m"Tabs are recorded"):
    //     read(t"# some comment\nroot   param1    param2\n  child1 param3")().extra
    //   .assert(_ == Extra())

    // // suite(m"Record tests"):

    // //   val record = GreekRecords(example1)

    // //   test[Optional[Text]](t"Test optional return value"):
    // //     record.alpha
    // //   .assert(_ == t"one")

    // //   test(m"Test return value"):
    // //     record.beta
    // //   .assert(_ == t"two")

    // //   test[Optional[Text]](t"Test missing value"):
    // //     record.iota
    // //   .assert(_ == Unset)

    // //   test[Optional[Text]](t"Test unique return value"):
    // //     record.eta
    // //   .assert(_ == t"eight")

    // //   test(m"Test many return value"):
    // //     record.gamma
    // //   .assert(_ == List(t"three", t"four", t"five"))

    // //   test(m"Test multiple return value"):
    // //     record.kappa
    // //   .assert(_ == List(t"nine", t"ten", t"eleven"))

    // suite(m"Interpolation suite"):

    //   test(m"Interpolator with no substitutions"):
    //     codl"""
    //       root
    //         child value
    //       test
    //     """.untyped.copy(margin = 0)
    //   .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"child")(CodlNode(t"value")())), CodlNode(t"test")()))

    //   test(m"Interpolator with one substitutions"):
    //     val bar = t"Hello"

    //     codl"""
    //       root ${bar}
    //         child value
    //       test
    //     """.untyped.copy(margin = 0)
    //   .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"Hello")(), CodlNode(t"child")(CodlNode(t"value")())), CodlNode(t"test")()))

    //   test(m"Interpolator with substitution containing space"):
    //     val bar = t"Hello World"

    //     codl"""
    //       root ${bar}
    //         child value
    //       test
    //     """.untyped.copy(margin = 0)
    //   .assert(_ == CodlDoc(CodlNode(t"root")(CodlNode(t"Hello World")(), CodlNode(t"child")(CodlNode(t"value")())), CodlNode(t"test")()))
