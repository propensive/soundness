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
package exegesis

import anticipation.*
import fulminate.*
import vacuous.*

object LspError:
  // Each reason carries its JSON-RPC wire code (`code`), alongside the sequential `number` used
  // for the SN-147 diagnostic. A fault raised in a handler is converted by `Lsp.listen` into an
  // error response whose `error.code` is the reason's wire code.
  enum Reason(val number: Int, val code: Int) extends Clarification:
    case Parse                extends Reason(1, -32700)
    case InvalidRequest       extends Reason(2, -32600)
    case MethodNotFound       extends Reason(3, -32601)
    case InvalidParams        extends Reason(4, -32602)
    case Internal             extends Reason(5, -32603)
    case ServerNotInitialized extends Reason(6, -32002)
    case RequestCancelled     extends Reason(7, -32800)
    case ContentModified      extends Reason(8, -32801)
    case ServerCancelled      extends Reason(9, -32802)
    case RequestFailed        extends Reason(10, -32803)

  // The inverse of `code`: recovers the reason from an error response's wire code, for a client
  // reading a fault a server sent it. A server may answer with a code outside the standard set —
  // the protocol reserves a range for implementation-defined errors — which is `Unset` here, and
  // reported as `Internal` by the caller.
  def reason(code: Int): Optional[Reason] =
    var found: Optional[Reason] = Unset
    var index: Int = 0

    while index < Reason.values.length do
      val reason = Reason.values(index)
      if reason.code == code then found = reason
      index += 1

    found

  given communicable: Reason is Communicable =
    case Reason.Parse                => m"the message could not be parsed as JSON"
    case Reason.InvalidRequest       => m"the message was not a valid JSON-RPC request"
    case Reason.MethodNotFound       => m"the method name was not recognised by the server"
    case Reason.InvalidParams        => m"the parameters were not valid for the requested method"
    case Reason.Internal             => m"an internal error occurred"
    case Reason.ServerNotInitialized => m"the server has not been initialized"
    case Reason.RequestCancelled     => m"the request was cancelled"
    case Reason.ContentModified      => m"the document content was modified during the request"
    case Reason.ServerCancelled      => m"the request was cancelled by the server"
    case Reason.RequestFailed        => m"the request failed"

case class LspError(reason: LspError.Reason, details: Optional[Text] = Unset)(using Diagnostics)
extends Error(147, reason.number)(m"the LSP operation failed because $reason"):
  // The message sent to the client in the error response: the given details, or the reason's
  // standard description.
  def response: Text = details.or(reason.communicate.text)
