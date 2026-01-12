package obligatory

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
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

case class rpc() extends scala.annotation.StaticAnnotation

object Obligatory

package unframables:
  given contentLength: Tactic[FrameError] => Unframable by Text = input =>
    val cursor = Cursor(input)
    def key(mark: Mark)(using Cursor.Held): Text = cursor.lay(abort(FrameError())):
      case ':'  =>
        cursor.grab(mark, cursor.mark).also:
          cursor.next()

      case char =>
        if cursor.next() then key(mark) else abort(FrameError())

    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(abort(FrameError())):
      case '\r'  =>
        cursor.grab(mark, cursor.mark).also:
          cursor.lay(???): datum =>
            println(datum)
      case char =>
        if cursor.next() then key(mark) else abort(FrameError())

    var length: Int = 0

    cursor.hold(key(cursor.mark)).lower match
      case t"content-length" =>
        length = safely(cursor.hold(value(cursor.mark)).as[Int]).or(abort(FrameError()))

      case t"content-type"   => cursor.hold(value(cursor.mark))


  given lengthPrefixed: Tactic[FrameError] => Unframable by Data = input =>
    val cursor = Cursor(input)
    ???



  given newlineDelimited: Tactic[FrameError] => Unframable by Text = input =>
    val cursor = Cursor(input)
    ???

case class FrameError()(using Diagnostics) extends Error(m"could not deframe the message")

object Unframable

trait Unframable:
  type Operand
  def break(input: Iterator[Operand]): Iterator[Operand]

trait Rcp extends Formal

object Rpc:

  given lsp: Lsp is Protocolic over (Rcp in Json) = ???


trait Semantizable extends Typeclass:
  extension (value: Self) def narrate: Text = narration(value)
  def narration(value: Self): Text

object Lsp:
  case class Folder(uri: Text, name: Text)
  case class ClientInfo(name: Text, version: Semver)

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
