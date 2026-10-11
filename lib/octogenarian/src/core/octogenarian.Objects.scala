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
package octogenarian

import java.nio.ByteBuffer

import anticipation.*
import contingency.*
import gossamer.*
import kaleidoscope.*
import rudiments.*
import vacuous.*

// The serialized forms of git's objects. Parsing is shared by both backends; serialization is
// used when a commit or tag is authored here, which is what keeps signing in-process.
private[octogenarian] object Objects:
  import Git.Error.Reason as Reason

  def bytes(text: Text): Data = text.s.getBytes("UTF-8").nn.asInstanceOf[Data]
  def text(data: Data): Text = String(data.asInstanceOf[scala.Array[Byte]], "UTF-8").nn.tt

  def parse(hash: Git.Hash, content: (Git.Object.Kind, Data))(using Tactic[Git.Error]): Git.Object =
    content match
      case (Git.Object.Kind.Blob, data)   => Git.Object.Blob(data)
      case (Git.Object.Kind.Tree, data)   => Git.Object.Tree(parseTree(data))
      case (Git.Object.Kind.Commit, data) => Git.Object.Commit(parseCommit(hash, data))
      case (Git.Object.Kind.Tag, data)    => Git.Object.Tag(parseTag(data))

  // Header lines run until the first blank line; a line starting with a space continues the
  // previous header (the `gpgsig` block). The rest is the message.
  private def split(data: Data): (List[(Text, Text)], Text) =
    val whole = text(data)
    val boundary = whole.s.indexOf("\n\n")
    val (headerText, message) =
      if boundary < 0 then (whole, t"") else (whole.keep(boundary), whole.skip(boundary + 2))

    val headers = scala.collection.mutable.ListBuffer[(Text, Text)]()

    def continue(line: Text): Unit =
      val (key, value) = headers.last
      headers(headers.length - 1) = (key, t"$value\n${line.skip(1)}")

    def start(line: Text): Unit =
      val space = line.s.indexOf(' ')
      if space > 0 then headers += ((line.keep(space), line.skip(space + 1)))

    headerText.cut(t"\n").each: line =>
      if line.starts(t" ") && headers.nonEmpty then continue(line) else start(line)

    (headers.to(List), message)

  def parseCommit(hash: Git.Hash, data: Data)(using Tactic[Git.Error]): Commit =
    val (headers, message) = split(data)
    def header(name: Text): Optional[Text] = headers.reap { case (`name`, value) => value }
    def signature(name: Text): Git.Signature =
      header(name).let(Git.Signature.parse(_)).or(abort(Git.Error(Reason.NoSuchObject(hash.text))))

    Commit
      ( hash,
        Git.Hash.unsafe(header(t"tree").or(abort(Git.Error(Reason.NoSuchObject(hash.text))))),
        headers.sweep { case (t"parent", value) => Git.Hash.unsafe(value) },
        signature(t"author"),
        signature(t"committer"),
        header(t"gpgsig"),
        trimMessage(message) )

  def parseTag(data: Data)(using Tactic[Git.Error]): Git.Tag.Annotation =
    val (headers, body) = split(data)
    def header(name: Text): Optional[Text] = headers.reap { case (`name`, value) => value }
    val (message, signature) = splitTagSignature(body)

    Git.Tag.Annotation
      ( Git.Hash.unsafe(header(t"object").or(t"")),
        header(t"type").let(Git.Object.Kind.parse(_)).or(Git.Object.Kind.Commit),
        Git.Tag.unsafe(header(t"tag").or(t"")),
        header(t"tagger").let(Git.Signature.parse(_)),
        trimMessage(message),
        signature )

  private def splitTagSignature(body: Text): (Text, Optional[Text]) =
    val marker = body.s.indexOf("-----BEGIN ")
    if marker < 0 then (body, Unset) else (body.keep(marker), body.skip(marker))

  private def trimMessage(message: Text): Text =
    if message.ends(t"\n") then message.skip(1, Rtl) else message

  // `<mode> <name>\0<20 raw bytes>`, repeated. Read positionally through a byte buffer.
  def parseTree(data: Data): List[Git.Object.Tree.Entry] =
    val array = data.asInstanceOf[scala.Array[Byte]]
    val buffer = ByteBuffer.wrap(array).nn
    val length = array.length

    def indexOf(byte: Byte, from: Int): Int =
      if from >= length || buffer.get(from) == byte then from else indexOf(byte, from + 1)

    def slice(from: Int, until: Int): Text =
      String(array, from, until - from, "UTF-8").nn.tt

    def hex(from: Int): Text =
      val builder = StringBuilder()
      def recur(index: Int): Unit = if index < from + 20 then
        val value = buffer.get(index) & 0xff
        builder.append(Character.forDigit(value >> 4, 16)).append(Character.forDigit(value & 0xf, 16))
        recur(index + 1)
      recur(from)
      builder.toString.tt

    def recur(index: Int, entries: List[Git.Object.Tree.Entry]): List[Git.Object.Tree.Entry] =
      if index >= length then entries.reverse else
        val space = indexOf(' '.toByte, index)
        val nul = indexOf(0.toByte, space)
        if nul + 21 > length then entries.reverse else
          val mode = Git.Object.Tree.Mode.parse(slice(index, space))
          val name = slice(space + 1, nul)
          val hash = Git.Hash.unsafe(hex(nul + 1))
          val next = mode.lay(entries) { mode => Git.Object.Tree.Entry(mode, name, hash) :: entries }
          recur(nul + 21, next)

    recur(0, Nil)

  private def parentLine(parent: Git.Hash): Text = t"parent ${parent.text}\n"

  def serializeCommit
    ( tree: Git.Hash, parents: List[Git.Hash], author: Git.Signature, committer: Git.Signature,
      message: Text )
  :   Data =

    val parentLines = parents.map(parentLine(_)).join
    bytes(t"tree ${tree.text}\n${parentLines}author ${author.text}\ncommitter ${committer.text}\n\n${message}\n")

  // Inserts an armoured signature as the `gpgsig` header, its continuation lines indented by
  // one space, immediately before the blank line that ends the headers.
  def signCommit(payload: Data, signature: Text): Data =
    val whole = text(payload)
    val boundary = whole.s.indexOf("\n\n")
    val lines = signature.trim.cut(t"\n")
    val block = t"gpgsig ${lines.join(t"\n ")}\n"
    bytes(t"${whole.keep(boundary + 1)}$block${whole.skip(boundary + 1)}")

  // The signed payload and the signature of a signed commit: the object with its `gpgsig`
  // header removed, and that header's content.
  def commitSignature(data: Data): Optional[(Data, Text)] =
    val whole = text(data)
    val boundary = whole.s.indexOf("\n\n")
    if boundary < 0 then Unset else
      val headerLines = whole.keep(boundary).cut(t"\n")
      val signature = scala.collection.mutable.ListBuffer[Text]()
      val kept = scala.collection.mutable.ListBuffer[Text]()
      var inSignature = false

      val gpgsig = t"gpgsig "
      val space = t" "

      def consider(line: Text): Unit =
        if line.starts(gpgsig) then
          inSignature = true
          signature += line.skip(7)
        else if inSignature && line.starts(space) then signature += line.skip(1)
        else
          inSignature = false
          kept += line

      headerLines.each(consider(_))

      if signature.isEmpty then Unset
      else (bytes(t"${kept.to(List).join(t"\n")}${whole.skip(boundary)}"), signature.to(List).join(t"\n"))

  def serializeTag
    ( target: Git.Hash, kind: Git.Object.Kind, name: Git.Tag, tagger: Git.Signature, message: Text )
  :   Data =

    bytes(t"object ${target.text}\ntype ${kind.text}\ntag ${name.text}\ntagger ${tagger.text}\n\n$message\n")

  def signTag(payload: Data, signature: Text): Data =
    bytes(t"${text(payload)}${signature.trim}\n")

  def tagSignature(data: Data): Optional[(Data, Text)] =
    val whole = text(data)
    val marker = whole.s.indexOf("-----BEGIN ")
    if marker < 0 then Unset else (bytes(whole.keep(marker)), whole.skip(marker))
