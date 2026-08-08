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

import scala.caps

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import rudiments.*
import vacuous.*

// The client's half of a Language Server exchange: the handle an editor — or a proxy — holds on a
// running server. It is the mirror of `LspSession`, and it is a capability, lent by `Lsp.Server`'s
// `session` for the duration of a lambda and disposed of afterwards, so it cannot outlive the
// server it speaks to.
//
// Outbound messages are put on the inherited `JsonRpc` channel, which the session's writer drains
// onto the transport; inbound messages are read by the session's reader and routed here. A request
// blocks the caller until its response arrives, but never blocks the reader, so several requests
// may be in flight at once and may be answered out of order.
class LspConnection private[exegesis] ()(using Monitor, Diagnostics)
extends JsonRpc, caps.ExclusiveCapability:
  type Origin = Lsp

  import strategies.throwUnsafely

  // One proxy module per sub-interface, all sharing this instance's outgoing channel. The split
  // mirrors `LspDispatch`: a single proxy for the whole protocol would inline a codec per method
  // into one class and overflow the JVM constant-pool limit.
  // The connection is confined by its own type and each proxy is a member of it, so sealing the
  // reference the generated modules hold is sound; the macro cannot take a capability-typed splice
  // (as in `LspSession.client0`).
  private val channel: JsonRpc = caps.unsafe.unsafeAssumePure(this)

  // The same confinement argument seals each proxy: the generated module's only capabilities
  // are this connection and the Lsp object, both already reachable through the member that
  // holds it, so the pure interface type loses nothing that is not confined here anyway.
  val lifecycle:  LspLifecycle  = caps.unsafe.unsafeAssumePure(channel.proxy[LspLifecycle])
  val language:   LspLanguage   = caps.unsafe.unsafeAssumePure(channel.proxy[LspLanguage])
  val navigation: LspNavigation = caps.unsafe.unsafeAssumePure(channel.proxy[LspNavigation])
  val editing:    LspEditing    = caps.unsafe.unsafeAssumePure(channel.proxy[LspEditing])
  val advanced:   LspAdvanced   = caps.unsafe.unsafeAssumePure(channel.proxy[LspAdvanced])
  val workspace:  LspWorkspace  = caps.unsafe.unsafeAssumePure(channel.proxy[LspWorkspace])
  val resolve:    LspResolve    = caps.unsafe.unsafeAssumePure(channel.proxy[LspResolve])

  // A fault the server reports as an error response arrives as a `JsonRpcError` carrying the wire
  // code, which is exactly the vocabulary of `LspError.Reason`; a code outside the standard set is
  // reported as `Internal`, with the server's own message as the detail.
  private def ask[result](block: => result)(using Tactic[LspError]): result =
    try block catch case error: JsonRpcError =>
      abort(LspError(error.code.let(LspError.reason(_)).or(LspError.Reason.Internal), error.detail))

  // The raw seam: sends a message exactly as given, without minting an id or awaiting an answer.
  // A proxy forwards through it, so that the editor's own request ids — and the methods this
  // library does not model — cross unchanged.
  def send(message: Json): Unit = put(message)

  // Lifecycle

  def initialize
     ( root:         Optional[Text]        = Unset,
       name:         Text                  = t"soundness",
       version:      Optional[Text]        = Unset,
       folders:      List[Lsp.Folder]      = Nil,
       capabilities: Json                  = Map[Text, Json]().in[Json] )
     ( using Tactic[LspError] )
  :   Lsp.InitializeResult =

    ask:
      lifecycle.initialize
       ( processId        = Unset,
         clientInfo       = Lsp.ClientInfo(name, version),
         locale           = Unset,
         rootUri          = root,
         capabilities     = capabilities,
         workspaceFolders = folders )

  def initialized(): Unit = lifecycle.initialized()
  def shutdown()(using Tactic[LspError]): Unit = ask(lifecycle.shutdown()) yet ()
  def exit(): Unit = lifecycle.exit()

  // Documents

  def open(uri: Text, language0: Text, text: Text, version: Int = 1): Unit =
    lifecycle.`textDocument/didOpen`(Lsp.TextDocumentItem(uri, language0, version, text))

  def edit(uri: Text, version: Int, changes: List[Lsp.TextDocumentContentChangeEvent]): Unit =
    lifecycle.`textDocument/didChange`
     ( Lsp.VersionedTextDocumentIdentifier(uri, version), changes )

  // The whole-document form of `edit`: an unranged change replaces the document's content.
  def replace(uri: Text, version: Int, text: Text): Unit =
    edit(uri, version, List(Lsp.TextDocumentContentChangeEvent(Unset, text)))

  def save(uri: Text, text: Optional[Text] = Unset): Unit =
    lifecycle.`textDocument/didSave`(Lsp.TextDocumentIdentifier(uri), text)

  def close(uri: Text): Unit =
    lifecycle.`textDocument/didClose`(Lsp.TextDocumentIdentifier(uri))

  // Language features

  def hover(uri: Text, position: Lsp.Position)(using Tactic[LspError]): Optional[Lsp.Hover] =
    ask(language.`textDocument/hover`(Lsp.TextDocumentIdentifier(uri), position))

  def complete(uri: Text, position: Lsp.Position, context: Optional[Lsp.CompletionContext] = Unset)
     ( using Tactic[LspError] )
  :   Lsp.CompletionList =

    ask(language.`textDocument/completion`(Lsp.TextDocumentIdentifier(uri), position, context))

  def definition(uri: Text, position: Lsp.Position)(using Tactic[LspError]): List[Lsp.Location] =
    ask(language.`textDocument/definition`(Lsp.TextDocumentIdentifier(uri), position))

  def references(uri: Text, position: Lsp.Position, declaration: Boolean = true)
     ( using Tactic[LspError] )
  :   List[Lsp.Location] =

    ask:
      language.`textDocument/references`
       ( Lsp.TextDocumentIdentifier(uri), position, Lsp.ReferenceContext(declaration) )

  def symbols(uri: Text)(using Tactic[LspError]): List[Lsp.DocumentSymbol] =
    ask(language.`textDocument/documentSymbol`(Lsp.TextDocumentIdentifier(uri)))

  def format(uri: Text, options: Lsp.FormattingOptions)(using Tactic[LspError])
  :   List[Lsp.TextEdit] =

    ask(language.`textDocument/formatting`(Lsp.TextDocumentIdentifier(uri), options))

  def rename(uri: Text, position: Lsp.Position, name: Text)(using Tactic[LspError])
  :   Lsp.WorkspaceEdit =

    ask(language.`textDocument/rename`(Lsp.TextDocumentIdentifier(uri), position, name))

  def codeActions(uri: Text, range: Lsp.Range, context: Lsp.CodeActionContext)
     ( using Tactic[LspError] )
  :   List[Lsp.CodeAction] =

    ask(language.`textDocument/codeAction`(Lsp.TextDocumentIdentifier(uri), range, context))

  def signatureHelp(uri: Text, position: Lsp.Position)(using Tactic[LspError])
  :   Optional[Lsp.SignatureHelp] =

    ask(language.`textDocument/signatureHelp`(Lsp.TextDocumentIdentifier(uri), position))

  // Navigation

  def declaration(uri: Text, position: Lsp.Position)(using Tactic[LspError]): List[Lsp.Location] =
    ask(navigation.`textDocument/declaration`(Lsp.TextDocumentIdentifier(uri), position))

  def typeDefinition(uri: Text, position: Lsp.Position)(using Tactic[LspError])
  :   List[Lsp.Location] =

    ask(navigation.`textDocument/typeDefinition`(Lsp.TextDocumentIdentifier(uri), position))

  def implementation(uri: Text, position: Lsp.Position)(using Tactic[LspError])
  :   List[Lsp.Location] =

    ask(navigation.`textDocument/implementation`(Lsp.TextDocumentIdentifier(uri), position))

  def highlights(uri: Text, position: Lsp.Position)(using Tactic[LspError])
  :   List[Lsp.DocumentHighlight] =

    ask(navigation.`textDocument/documentHighlight`(Lsp.TextDocumentIdentifier(uri), position))

  def foldingRanges(uri: Text)(using Tactic[LspError]): List[Lsp.FoldingRange] =
    ask(navigation.`textDocument/foldingRange`(Lsp.TextDocumentIdentifier(uri)))

  def documentLinks(uri: Text)(using Tactic[LspError]): List[Lsp.DocumentLink] =
    ask(navigation.`textDocument/documentLink`(Lsp.TextDocumentIdentifier(uri)))

  def codeLenses(uri: Text)(using Tactic[LspError]): List[Lsp.CodeLens] =
    ask(navigation.`textDocument/codeLens`(Lsp.TextDocumentIdentifier(uri)))

  // Advanced

  def semanticTokens(uri: Text)(using Tactic[LspError]): Lsp.SemanticTokens =
    ask(advanced.`textDocument/semanticTokens/full`(Lsp.TextDocumentIdentifier(uri)))

  def inlayHints(uri: Text, range: Lsp.Range)(using Tactic[LspError]): List[Lsp.InlayHint] =
    ask(advanced.`textDocument/inlayHint`(Lsp.TextDocumentIdentifier(uri), range))

  def diagnostics(uri: Text)(using Tactic[LspError]): Lsp.DocumentDiagnosticReport =
    ask(advanced.`textDocument/diagnostic`(Lsp.TextDocumentIdentifier(uri), Unset, Unset))

  // Workspace

  def search(query: Text)(using Tactic[LspError]): List[Lsp.WorkspaceSymbol] =
    ask(workspace.`workspace/symbol`(query))

  def execute(command: Text, arguments: List[Json] = Nil)(using Tactic[LspError]): Optional[Json] =
    ask(workspace.`workspace/executeCommand`(command, arguments))

  def configure(settings: Json): Unit = workspace.`workspace/didChangeConfiguration`(settings)
