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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package obligatory

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import revolution.*
import rudiments.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces

case class rpc() extends StaticAnnotation

object Obligatory

package breakables:
  given contentLength: Tactic[FrameError] => Text is Breakable = input =>
    val cursor = Cursor(input)
    def fail(): Nothing = abort(FrameError())

    def skip(): Unit = while cursor.next() && cursor.datum(using Unsafe) == ' ' do ()
    def key(mark: Mark)(using Cursor.Held): Optional[Text] = cursor.lay(fail()):
      case Cr  => if mark != cursor.mark then fail() else
        cursor.consume(fail())("\n")
        cursor.next()
        Unset

      case ':' =>
        cursor.grab(mark, cursor.mark).also(skip())

      case chr =>
        if cursor.next() then key(mark) else abort(FrameError())

    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(fail()):
      case '\r' => cursor.grab(mark, cursor.mark).also:
        cursor.consume(fail())("\n")
        cursor.next()
      case char => if cursor.next() then value(mark) else fail()

    def frame(length: Int): Optional[Text] =
      if cursor.finished then Unset else cursor.hold(key(cursor.mark).let(_.lower)) match
        case Unset           => cursor.take(fail())(length)
        case t"content-type" => cursor.hold(value(cursor.mark)) yet frame(length)

        case t"content-length" =>
          frame(safely(cursor.hold(value(cursor.mark)).decode[Int]).or(fail()))

        case other =>
          fail()

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset
      def hasNext: Boolean =
        if ready == Unset then ready = frame(0)
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

  given lengthPrefixed: Tactic[FrameError] => Data is Breakable = input =>
    def fail() = abort(FrameError())
    val cursor = Cursor(input)

    def length: Optional[Int] =
      cursor.lay(Unset): byte0 =>
        cursor.next()
        cursor.lay(fail()): byte1 =>
          cursor.next()
          cursor.lay(fail()): byte2 =>
            cursor.next()
            cursor.lay(fail()): byte3 =>
              cursor.next()
              byte0 << 24 | byte1 << 16 | byte2 << 8 | byte3


    def frame(): Optional[Data] = length.let: length =>
      cursor.take(fail())(length)

    new Iterator[Data]:
      private var ready: Optional[Data] = Unset
      def hasNext: Boolean =
        if ready == Unset then ready = frame()
        ready != Unset

      def next(): Data = ready.asInstanceOf[Data].also:
        ready = Unset


  given crDelimited: Text is Breakable = input =>
    val cursor = Cursor(input)

    def frame(): Optional[Text] = cursor.hold:
      val start = cursor.mark
      if !cursor.finished && cursor.seek(Cr)
      then cursor.grab(start, cursor.mark).also(cursor.next())
      else if cursor.mark == start then Unset else cursor.grab(start, cursor.mark)

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset
      def hasNext: Boolean =
        if ready == Unset then ready = frame()
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

  given lfDelimited: Text is Breakable = input =>
    val cursor = Cursor(input)

    def frame(): Optional[Text] = cursor.hold:
      val start = cursor.mark
      if !cursor.finished && cursor.seek(Lf)
      then cursor.grab(start, cursor.mark).also(cursor.next())
      else if cursor.mark == start then Unset else cursor.grab(start, cursor.mark)

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset
      def hasNext: Boolean =
        if ready == Unset then ready = frame()
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

  given crLfDelimited: Tactic[FrameError] => Text is Breakable = input =>
    val cursor = Cursor(input)

    def frame(): Optional[Text] = cursor.hold:
      val start = cursor.mark
      if !cursor.finished && cursor.seek(Cr)
      then cursor.grab(start, cursor.mark).also:
        cursor.next()
        if !cursor.lay(false)(_ == Lf) then abort(FrameError()) else cursor.next()
      else if cursor.mark == start then Unset else cursor.grab(start, cursor.mark)

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset
      def hasNext: Boolean =
        if ready == Unset then ready = frame()
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

  given serverSideEvents: Text is Breakable = input =>
    val cursor = Cursor(input)

    def frame(start: Cursor.Mark)(using Cursor.Held): Optional[Text] = cursor.hold:
      if !cursor.finished && cursor.seek(Lf) then
        val end = cursor.mark
        cursor.next()
        cursor.lay(cursor.grab(start, end)): char =>
          if char == Lf then cursor.next() yet cursor.grab(start, end) else frame(start)
      else if cursor.mark == start then Unset else
        cursor.grab(start, cursor.mark)

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset
      def hasNext: Boolean =
        if ready == Unset then ready = cursor.hold(frame(cursor.mark))
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

case class FrameError()(using Diagnostics) extends Error(m"could not deframe the message")

object Breakable

trait Breakable extends Typeclass:
  def break(input: Iterator[Self]): Iterator[Self]

trait Rcp extends Formal

object Rpc:

  given lsp: Lsp is Protocolic over (Rcp in Json) = ???


trait Semantizable extends Typeclass:
  extension (value: Self) def narrate: Text = narration(value)
  def narration(value: Self): Text

object Lsp:
  case class Folder(uri: Text, name: Text)
  case class ClientInfo(name: Text, version: Semver)

extension [element: Breakable](stream: Iterator[element])
  def break(): Iterator[element] = element.break(stream)

trait Lsp:
  @rpc
  def initialize
       (processId:        Int,
        clientInfo:       Lsp.ClientInfo,
        locale:           Text,
        rootPath:         Text,
        rootUri:          Text,
        capabilities:     Json,
        workspaceFolders: List[Lsp.Folder])
  : Unit

  @rpc
  def initialized(): Unit

  @rpc
  def shutdown(): Unit

  @rpc
  def exit(): Unit

  case class TextDocument(uri: Text, languageId: Text, version: Int, text: Text)

  @rpc
  def `textDocument/didOpen`(textDocument: TextDocument): Unit
