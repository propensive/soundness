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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

// A base for Language Server implementations. Subclasses provide their `name`, `capabilities` and
// any of the overridable handler hooks they support; everything else (the JSON-RPC method dispatch,
// the document store, and the stdio transport) is provided here. This is deliberately simpler than
// synesthesia's `McpServer`: the set of LSP features is fixed and closed, so the handlers are plain
// overridable methods rather than annotation-derived registrations.
trait LspServer() extends Lsp:
  import Lsp.*

  private val documents: scm.HashMap[Text, TextDocumentItem] = scm.HashMap()
  private lazy val lsp: LspClient = client

  // Abstract members

  def name: Text
  def capabilities: ServerCapabilities
  def version: Optional[Text] = Unset

  // Overridable handler hooks; each defaults to a no-op or an empty result.

  def onOpen(document: TextDocumentItem)(using LspClient): Unit = ()

  def onChange
    ( document: VersionedTextDocumentIdentifier,
      changes:  List[TextDocumentContentChangeEvent] )
    ( using LspClient )
  :   Unit = ()

  def onSave(document: TextDocumentIdentifier, text: Optional[Text])(using LspClient): Unit = ()
  def onClose(document: TextDocumentIdentifier): Unit = ()

  def hover(uri: Text, position: Position): Optional[Hover] = Unset
  def complete(uri: Text, position: Position): CompletionList = CompletionList()
  def definition(uri: Text, position: Position): List[Location] = Nil
  def references(uri: Text, position: Position, includeDeclaration: Boolean): List[Location] = Nil
  def documentSymbols(uri: Text): List[DocumentSymbol] = Nil
  def format(uri: Text, options: FormattingOptions): List[TextEdit] = Nil
  def rename(uri: Text, position: Position, newName: Text): WorkspaceEdit = WorkspaceEdit()
  def codeActions(uri: Text, range: Range, context: CodeActionContext): List[CodeAction] = Nil
  def signatureHelp(uri: Text, position: Position): Optional[SignatureHelp] = Unset

  // The current contents of an open document, if any.
  def document(uri: Text): Optional[TextDocumentItem] = documents.at(uri)

  // JSON-RPC method implementations, delegating to the hooks above.

  def initialize
    ( processId:        Optional[Int],
      clientInfo:       Optional[ClientInfo],
      locale:           Optional[Text],
      rootUri:          Optional[Text],
      capabilities:     Json,
      workspaceFolders: Optional[List[Folder]] )
  :   InitializeResult =

    InitializeResult(this.capabilities, ServerInfo(name, version))

  def initialized(): Unit = ()
  def shutdown(): Json = ().json
  def exit(): Unit = ()

  def `textDocument/didOpen`(textDocument: TextDocumentItem): Unit =
    documents(textDocument.uri) = textDocument
    onOpen(textDocument)(using lsp)

  def `textDocument/didChange`
    ( textDocument:   VersionedTextDocumentIdentifier,
      contentChanges: List[TextDocumentContentChangeEvent] )
  :   Unit =

    if contentChanges.nonEmpty then documents.at(textDocument.uri).let: current =>
      documents(textDocument.uri) =
        current.copy(version = textDocument.version, text = contentChanges.last.text)

    onChange(textDocument, contentChanges)(using lsp)

  def `textDocument/didSave`(textDocument: TextDocumentIdentifier, text: Optional[Text]): Unit =
    onSave(textDocument, text)(using lsp)

  def `textDocument/didClose`(textDocument: TextDocumentIdentifier): Unit =
    documents.remove(textDocument.uri)
    onClose(textDocument)

  def `textDocument/completion`
    ( textDocument: TextDocumentIdentifier,
      position:     Position,
      context:      Optional[CompletionContext] )
  :   CompletionList =

    complete(textDocument.uri, position)

  def `textDocument/hover`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[Hover] =

    hover(textDocument.uri, position)

  def `textDocument/definition`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    definition(textDocument.uri, position)

  def `textDocument/references`
    ( textDocument: TextDocumentIdentifier, position: Position, context: ReferenceContext )
  :   List[Location] =

    references(textDocument.uri, position, context.includeDeclaration)

  def `textDocument/documentSymbol`(textDocument: TextDocumentIdentifier): List[DocumentSymbol] =
    documentSymbols(textDocument.uri)

  def `textDocument/formatting`(textDocument: TextDocumentIdentifier, options: FormattingOptions)
  :   List[TextEdit] =

    format(textDocument.uri, options)

  def `textDocument/rename`
    ( textDocument: TextDocumentIdentifier, position: Position, newName: Text )
  :   WorkspaceEdit =

    rename(textDocument.uri, position, newName)

  def `textDocument/codeAction`
    ( textDocument: TextDocumentIdentifier, range: Range, context: CodeActionContext )
  :   List[CodeAction] =

    codeActions(textDocument.uri, range, context)

  def `textDocument/signatureHelp`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[SignatureHelp] =

    signatureHelp(textDocument.uri, position)

  // The stdio transport. Reads `Content-Length`-framed JSON-RPC messages from standard input and
  // dispatches each to the methods above; a single asynchronous writer drains the outgoing channel
  // (both request responses, funnelled in via `put`, and server-initiated notifications such as
  // `publishDiagnostics`) and frames each message back onto standard output, so writes never
  // interleave.
  def serve()(using Stdio, Monitor, Probate): Unit =
    import charEncoders.utf8
    import strategies.throwUnsafely

    val dispatch: Json => Optional[Json] = JsonRpc.serve[Lsp](this)

    // The writer drains the channel and frames each message onto stdout.
    val writer: Task[Unit] = async:
      outgoing.iterator.each: json =>
        val body: Text = json.encode
        val payload: Data = body.data
        summon[Stdio].write(t"Content-Length: ${payload.length}\r\n\r\n".data)
        summon[Stdio].write(payload)
        summon[Stdio].out.flush()

    summon[Stdio].in.stream[Data].iterator.frames[ContentLength].each: frame =>
      try dispatch(frame.utf8.decode[Json]).let(put)
      catch case error: Exception => put(JsonRpc.error(-32603, t"Internal error").json)

    writer.cancel()
