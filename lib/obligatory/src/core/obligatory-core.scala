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

package framings:
  given contentLength: Tactic[FramingError] => Framing by Text = input =>
    val cursor = Cursor(input)
    def key(mark: Mark)(using Cursor.Held): Text = cursor.lay(abort(FramingError())):
      case ':'  =>
        cursor.grab(mark, cursor.mark).also:
          cursor.next()

      case char =>
        if cursor.next() then key(mark) else abort(FramingError())

    def value(mark: Mark)(using Cursor.Held): Text = cursor.lay(abort(FramingError())):
      case '\r'  =>
        cursor.grab(mark, cursor.mark).also:
          if cursor.next() then cursor.datum

      case char =>
        if cursor.next() then key(mark) else abort(FramingError())

    var length: Int = 0

    cursor.hold(key(cursor.mark)).lower match
      case t"content-length" =>
        length = safely(cursor.hold(value(cursor.mark)).as[Int]).or(abort(FramingError()))
      case t"content-type"   => cursor.hold(value(cursor.mark))


  given newlineDelimited: Framing by Text = input =>
    val cursor = Cursor(input)
    ???

case class FramingError() extends Error(m"could not unframe the message")

object Framing

trait Framing:
  type Operand
  def break(input: Iterator[Operand]): Iterator[Operand]

trait Rcp extends Formal

object Rpc:

  given lsp: Lsp is Protocolic over (Rcp in Json) = ???


trait Lsp:

  case class Folder(uri: Text, name: Text)
  case class ClientInfo(name: Text, version: Semver)

  @rpc
  def initialize
       (processId:        Int,
        clientInfo:       ClientInfo,
        locale:           Text,
        rootPath:         Text,
        rootUri:          Text,
        capabilities:     Json,
        workspaceFolders: List[Folder])
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
