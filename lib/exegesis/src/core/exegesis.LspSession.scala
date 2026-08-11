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
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import jacinta.*
import obligatory.*
import rudiments.*
import vacuous.*

import errorDiagnostics.stackTracesDiagnostics

// One live LSP session: the bridge between the macro-generated JSON-RPC dispatchers and the
// registered handlers. It owns the open-document store and the details the client reported at
// initialization, and mints the per-dispatch handles — document, workspace, error emitter — that
// handlers receive as context. One session is created per `Lsp.listen` invocation and lives only
// in its frame; it is an exclusive capability, so it cannot be stored in an application-lifetime
// object (the bug class of `rep/capability-escape`).
private[exegesis] object LspSession:
  private[exegesis] class Cell:
    @scala.caps.unsafe.untrackedCaptures
    private[exegesis] var fault: Optional[Lsp.Error] = Unset

  // Building the session consumes the registry: nothing can register once serving begins.
  private[exegesis] def apply
    ( consume registry: Lsp.Registry^, name: Text, version: Optional[Text] )
  :   LspSession^ =

    new LspSession(registry, name, version)

private[exegesis] class LspSession private
  ( handlers: Lsp.Registry^, name: Text, version: Optional[Text] )
extends Lsp, caps.ExclusiveCapability:
  import Lsp.*

  // The store: pure, mutable data, touched only by the single-threaded dispatch loop.
  private val states: scm.HashMap[Text, Document.State] = scm.HashMap()

  // The fault recorded by the current dispatch's emitter, if any; collected (and cleared) by the
  // serve loop after each dispatch, and turned into an error response or a log message. The slot
  // lives in its own pure-data cell rather than on the session, so the emitter lent to a handler
  // captures only the cell: were it to capture the session, it would overlap with the workspace
  // handle lent alongside it, which separation checking rejects.
  private val fault0: LspSession.Cell = LspSession.Cell()

  // What the client reported at initialization: pure data, exposed through `Workspace`.
  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var processId0: Optional[Int] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var clientInfo0: Optional[ClientInfo] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var locale0: Optional[Text] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var rootUri0: Optional[Text] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var clientCapabilities0: Json = Json.ast(Json.Ast(Unset))

  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var folders0: List[Folder] = Nil

  @scala.caps.unsafe.untrackedCaptures
  private[exegesis] var trace0: Optional[Text] = Unset

  // The client proxy is minted once per session, inside it, and lent to handlers through the
  // workspace handle; it is never stored in an application-lifetime object.
  // The generated proxy captures the session; the macro splices `this` at a pure `JsonRpc`
  // hole, so the receiver is sealed at the staging boundary. The proxy never leaves the
  // session's scope: it is lent to handlers through the workspace handle.
  private[exegesis] val client0: Lsp.Client = caps.unsafe.unsafeAssumePure(this).client

  private[exegesis] def fault(): Optional[Lsp.Error] =
    val result = fault0.fault
    fault0.fault = Unset
    result

  private[exegesis] def uris: List[Text] = states.keys.to(List)

  // The fault-aware conclusion of one dispatch: a fault recorded by the handler pre-empts its
  // result — for a request it becomes the error response, echoing the request's id; for a
  // notification, which may not be answered, it is reported through `window/logMessage`.
  private[exegesis] def conclude(json: Json, response: Optional[Json]): Optional[Json] =
    fault().lay(response): fault =>
      Lsp.requestId(json).lay:
        client0.logMessage(fault.response)
        Unset
      .apply: id =>
        JsonRpc.failure(fault.reason.code, fault.response, id)

  private[exegesis] def snapshot(uri: Text): Optional[TextDocumentItem] =
    states.at(uri).let: state =>
      TextDocumentItem(state.uri, state.language, state.version, state.text)

  private def space(): Workspace^{this, caps.any} = Workspace(this)

  private def emitter(): Emit[Lsp.Error]^{caps.any} =
    val cell = fault0

    Emit[Lsp.Error]: fault => cell.fault = fault

  private def missing[result](uri: Text, default: result): result =
    fault0.fault = Lsp.Error(Lsp.Error.Reason.InvalidParams, t"the document $uri is not open")
    default

  // Invocation helpers: one per handler shape. Each looks up the handler (`null` means
  // unregistered, so the wire default is returned, as before), mints the per-dispatch handles,
  // and applies the handler with its tagged payloads. A document-scoped request for a document
  // that is not open records an `InvalidParams` fault. The registry's slots are an erased rim,
  // so each helper restores the handler's type with a single, annotated cast; the annotation
  // also stops the context-function value being applied at the definition.

  private def focused0[result](uri: Text)(default: result)(handler: AnyRef | Null): result =
    if handler == null then default else
      val invoke: Focused[result] =
        handler.asInstanceOf[Lsp.Registry.Slot[Focused[result]]].value

      states.at(uri).lay(missing(uri, default)): state =>
        invoke(using Document(state), space(), emitter())

  private def focused1[payload, result](uri: Text)(default: result)(handler: AnyRef | Null)
    ( payload: payload )
  :   result =

    if handler == null then default else
      val invoke: Focused1[payload, result] =
        handler.asInstanceOf[Lsp.Registry.Slot[Focused1[payload, result]]].value

      states.at(uri).lay(missing(uri, default)): state =>
        invoke(using payload, Document(state), space(), emitter())

  private def focused2[payload1, payload2, result](uri: Text)(default: result)
    ( handler: AnyRef | Null )
    ( payload1: payload1, payload2: payload2 )
  :   result =

    if handler == null then default else
      val invoke: Focused2[payload1, payload2, result] =
        handler.asInstanceOf[Lsp.Registry.Slot[Focused2[payload1, payload2, result]]].value

      states.at(uri).lay(missing(uri, default)): state =>
        invoke(using payload1, payload2, Document(state), space(), emitter())

  private def focused3[payload1, payload2, payload3, result](uri: Text)(default: result)
    ( handler: AnyRef | Null )
    ( payload1: payload1, payload2: payload2, payload3: payload3 )
  :   result =

    if handler == null then default else
      val invoke: Focused3[payload1, payload2, payload3, result] =
        handler.asInstanceOf[Lsp.Registry.Slot[Focused3[payload1, payload2, payload3, result]]]
        . value

      states.at(uri).lay(missing(uri, default)): state =>
        invoke(using payload1, payload2, payload3, Document(state), space(), emitter())

  private def ambient0[result](default: result)(handler: AnyRef | Null): result =
    if handler == null then default else
      val invoke: Ambient[result] =
        handler.asInstanceOf[Lsp.Registry.Slot[Ambient[result]]].value

      invoke(using space(), emitter())

  private def ambient1[payload, result](default: result)(handler: AnyRef | Null)
    ( payload: payload )
  :   result =

    if handler == null then default else
      val invoke: Ambient1[payload, result] =
        handler.asInstanceOf[Lsp.Registry.Slot[Ambient1[payload, result]]].value

      invoke(using payload, space(), emitter())

  // The JSON-RPC wire methods, dispatched by `Lsp.Dispatch`.

  def initialize
    ( processId:        Optional[Int],
      clientInfo:       Optional[ClientInfo],
      locale:           Optional[Text],
      rootUri:          Optional[Text],
      capabilities:     Json,
      workspaceFolders: Optional[List[Folder]] )
  :   InitializeResult =

    processId0 = processId
    clientInfo0 = clientInfo
    locale0 = locale
    rootUri0 = rootUri
    clientCapabilities0 = capabilities
    folders0 = workspaceFolders.or(Nil)
    InitializeResult(handlers.capabilities, ServerInfo(name, version))

  def initialized(): Unit = ambient0(())(handlers.ready0)

  def shutdown(): Json =
    ambient0(())(handlers.terminating0)
    ().in[Json]

  def exit(): Unit = ()

  def `textDocument/didOpen`(textDocument: TextDocumentItem): Unit =
    states(textDocument.uri) =
      Document.state
        ( textDocument.uri, textDocument.languageId, textDocument.version, textDocument.text )

    focused0(textDocument.uri)(())(handlers.opened0)

  def `textDocument/didChange`
    ( textDocument:   VersionedTextDocumentIdentifier,
      contentChanges: List[TextDocumentContentChangeEvent] )
  :   Unit =

    val changes0: List[TextDocumentContentChangeEvent] aka "changes" = contentChanges.aka["changes"]
    states.at(textDocument.uri).let(_.edit(textDocument.version, contentChanges))
    focused1(textDocument.uri)(())(handlers.changed0)(changes0)

  def `textDocument/didSave`(textDocument: TextDocumentIdentifier, text: Optional[Text]): Unit =
    val savedText0: Optional[Text] aka "savedText" = text.aka["savedText"]

    focused1(textDocument.uri)(())(handlers.saved0)(savedText0)

  // The final state of the document is lent to the handler, then evicted.
  def `textDocument/didClose`(textDocument: TextDocumentIdentifier): Unit =
    focused0(textDocument.uri)(())(handlers.closed0)
    states.remove(textDocument.uri)

  def `textDocument/completion`
    ( textDocument: TextDocumentIdentifier,
      position:     Position,
      context:      Optional[CompletionContext] )
  :   CompletionList =

    val position0: Position aka "position" = position.aka["position"]
    val context0: Optional[CompletionContext] aka "context" = context.aka["context"]

    focused2(textDocument.uri)(CompletionList())(handlers.complete0)
      ( position0, context0 )

  def `textDocument/hover`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[Hover] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Unset)(handlers.hover0)(position0)

  def `textDocument/definition`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.definition0)(position0)

  def `textDocument/references`
    ( textDocument: TextDocumentIdentifier, position: Position, context: ReferenceContext )
  :   List[Location] =

    val position0: Position aka "position" = position.aka["position"]

    val includeDeclaration0: Boolean aka "includeDeclaration" =
      context.includeDeclaration.aka["includeDeclaration"]

    focused2(textDocument.uri)(Nil)(handlers.references0)
      ( position0, includeDeclaration0 )

  def `textDocument/documentSymbol`(textDocument: TextDocumentIdentifier): List[DocumentSymbol] =
    focused0(textDocument.uri)(Nil)(handlers.documentSymbols0)

  def `textDocument/formatting`(textDocument: TextDocumentIdentifier, options: FormattingOptions)
  :   List[TextEdit] =

    val options0: FormattingOptions aka "options" = options.aka["options"]

    focused1(textDocument.uri)(Nil)(handlers.format0)(options0)

  def `textDocument/rename`
    ( textDocument: TextDocumentIdentifier, position: Position, newName: Text )
  :   WorkspaceEdit =

    val position0: Position aka "position" = position.aka["position"]
    val newName0: Text aka "newName" = newName.aka["newName"]

    focused2(textDocument.uri)(WorkspaceEdit())(handlers.rename0)
      ( position0, newName0 )

  def `textDocument/codeAction`
    ( textDocument: TextDocumentIdentifier, range: Range, context: CodeActionContext )
  :   List[CodeAction] =

    val range0: Range aka "range" = range.aka["range"]
    val context0: CodeActionContext aka "context" = context.aka["context"]

    focused2(textDocument.uri)(Nil)(handlers.codeActions0)
      ( range0, context0 )

  def `textDocument/signatureHelp`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[SignatureHelp] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Unset)(handlers.signatureHelp0)(position0)

  def `textDocument/declaration`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.declaration0)(position0)

  def `textDocument/typeDefinition`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.typeDefinition0)(position0)

  def `textDocument/implementation`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.implementation0)(position0)

  def `textDocument/documentHighlight`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[DocumentHighlight] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.documentHighlights0)(position0)

  def `textDocument/foldingRange`(textDocument: TextDocumentIdentifier): List[FoldingRange] =
    focused0(textDocument.uri)(Nil)(handlers.foldingRanges0)

  def `textDocument/selectionRange`
    ( textDocument: TextDocumentIdentifier, positions: List[Position] )
  :   List[SelectionRange] =

    val positions0: List[Position] aka "positions" = positions.aka["positions"]

    focused1(textDocument.uri)(Nil)(handlers.selectionRanges0)(positions0)

  def `textDocument/documentLink`(textDocument: TextDocumentIdentifier): List[DocumentLink] =
    focused0(textDocument.uri)(Nil)(handlers.documentLinks0)

  def `textDocument/codeLens`(textDocument: TextDocumentIdentifier): List[CodeLens] =
    focused0(textDocument.uri)(Nil)(handlers.codeLenses0)

  def `textDocument/documentColor`(textDocument: TextDocumentIdentifier): List[ColorInformation] =
    focused0(textDocument.uri)(Nil)(handlers.documentColors0)

  def `textDocument/colorPresentation`
    ( textDocument: TextDocumentIdentifier, color: Color, range: Range )
  :   List[ColorPresentation] =

    val color0: Color aka "color" = color.aka["color"]
    val range0: Range aka "range" = range.aka["range"]

    focused2(textDocument.uri)(Nil)(handlers.colorPresentations0)
      ( color0, range0 )

  def `textDocument/rangeFormatting`
    ( textDocument: TextDocumentIdentifier, range: Range, options: FormattingOptions )
  :   List[TextEdit] =

    val range0: Range aka "range" = range.aka["range"]
    val options0: FormattingOptions aka "options" = options.aka["options"]

    focused2(textDocument.uri)(Nil)(handlers.formatRange0)
      ( range0, options0 )

  def `textDocument/onTypeFormatting`
    ( textDocument: TextDocumentIdentifier,
      position:     Position,
      ch:           Text,
      options:      FormattingOptions )
  :   List[TextEdit] =

    val position0: Position aka "position" = position.aka["position"]
    val character0: Text aka "character" = ch.aka["character"]
    val options0: FormattingOptions aka "options" = options.aka["options"]

    focused3(textDocument.uri)(Nil)(handlers.formatOnType0)
      ( position0, character0, options0 )

  def `textDocument/prepareRename`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[Range] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Unset)(handlers.prepareRename0)(position0)

  def `textDocument/willSave`
    ( textDocument: TextDocumentIdentifier, reason: TextDocumentSaveReason )
  :   Unit =

    val reason0: TextDocumentSaveReason aka "reason" = reason.aka["reason"]

    focused1(textDocument.uri)(())(handlers.saving0)(reason0)

  def `textDocument/willSaveWaitUntil`
    ( textDocument: TextDocumentIdentifier, reason: TextDocumentSaveReason )
  :   List[TextEdit] =

    val reason0: TextDocumentSaveReason aka "reason" = reason.aka["reason"]

    focused1(textDocument.uri)(Nil)(handlers.savingEdits0)(reason0)

  def `textDocument/prepareCallHierarchy`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[CallHierarchyItem] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.prepareCallHierarchy0)(position0)

  def `callHierarchy/incomingCalls`(item: CallHierarchyItem): List[CallHierarchyIncomingCall] =
    val item0: CallHierarchyItem aka "item" = item.aka["item"]

    ambient1(Nil)(handlers.incomingCalls0)(item0)

  def `callHierarchy/outgoingCalls`(item: CallHierarchyItem): List[CallHierarchyOutgoingCall] =
    val item0: CallHierarchyItem aka "item" = item.aka["item"]

    ambient1(Nil)(handlers.outgoingCalls0)(item0)

  def `textDocument/prepareTypeHierarchy`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[TypeHierarchyItem] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.prepareTypeHierarchy0)(position0)

  def `typeHierarchy/supertypes`(item: TypeHierarchyItem): List[TypeHierarchyItem] =
    val item0: TypeHierarchyItem aka "item" = item.aka["item"]

    ambient1(Nil)(handlers.supertypes0)(item0)

  def `typeHierarchy/subtypes`(item: TypeHierarchyItem): List[TypeHierarchyItem] =
    val item0: TypeHierarchyItem aka "item" = item.aka["item"]

    ambient1(Nil)(handlers.subtypes0)(item0)

  def `textDocument/semanticTokens/full`(textDocument: TextDocumentIdentifier): SemanticTokens =
    focused0(textDocument.uri)(SemanticTokens())(handlers.semanticTokens0)

  def `textDocument/semanticTokens/full/delta`
    ( textDocument: TextDocumentIdentifier, previousResultId: Text )
  :   SemanticTokensDelta =

    val previousResultId0: Text aka "previousResultId" = previousResultId.aka["previousResultId"]

    focused1(textDocument.uri)(SemanticTokensDelta())(handlers.semanticTokensDelta0)
      ( previousResultId0 )

  def `textDocument/semanticTokens/range`(textDocument: TextDocumentIdentifier, range: Range)
  :   SemanticTokens =

    val range0: Range aka "range" = range.aka["range"]

    focused1(textDocument.uri)(SemanticTokens())(handlers.semanticTokensRange0)
      ( range0 )

  def `textDocument/inlayHint`(textDocument: TextDocumentIdentifier, range: Range)
  :   List[InlayHint] =

    val range0: Range aka "range" = range.aka["range"]

    focused1(textDocument.uri)(Nil)(handlers.inlayHints0)(range0)

  def `textDocument/inlineValue`
    ( textDocument: TextDocumentIdentifier, range: Range, context: InlineValueContext )
  :   List[InlineValueText] =

    val range0: Range aka "range" = range.aka["range"]
    val context0: InlineValueContext aka "context" = context.aka["context"]

    focused2(textDocument.uri)(Nil)(handlers.inlineValues0)
      ( range0, context0 )

  def `textDocument/linkedEditingRange`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[LinkedEditingRanges] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Unset)(handlers.linkedEditingRange0)(position0)

  def `textDocument/moniker`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Moniker] =

    val position0: Position aka "position" = position.aka["position"]

    focused1(textDocument.uri)(Nil)(handlers.monikers0)(position0)

  def `textDocument/diagnostic`
    ( textDocument:     TextDocumentIdentifier,
      identifier:       Optional[Text],
      previousResultId: Optional[Text] )
  :   DocumentDiagnosticReport =

    val identifier0: Optional[Text] aka "identifier" = identifier.aka["identifier"]

    val previousResultId0: Optional[Text] aka "previousResultId" =
      previousResultId.aka["previousResultId"]

    focused2(textDocument.uri)(DocumentDiagnosticReport())(handlers.diagnostics0)
      ( identifier0, previousResultId0 )

  def `workspace/symbol`(query: Text): List[WorkspaceSymbol] =
    val query0: Text aka "query" = query.aka["query"]

    ambient1(Nil)(handlers.workspaceSymbols0)(query0)

  private def seek(commands: List[(Text, AnyRef)], command: Text): AnyRef | Null =
    commands.stdlib.find(_._1 == command) match
      case Some((_, slot)) => slot
      case _               => null

  def `workspace/executeCommand`(command: Text, arguments: Optional[List[Json]]): Optional[Json] =
    val arguments0: Optional[List[Json]] aka "arguments" = arguments.aka["arguments"]

    val slot = seek(handlers.commands0, command)

    if slot != null then ambient1(Unset)(slot)(arguments0) else
      fault0.fault =
        Lsp.Error(Lsp.Error.Reason.InvalidParams, t"the command $command is not registered")

      Unset

  def `workspace/didChangeConfiguration`(settings: Json): Unit =
    val settings0: Json aka "settings" = settings.aka["settings"]

    ambient1(())(handlers.configuration0)(settings0)

  def `workspace/didChangeWatchedFiles`(changes: List[FileEvent]): Unit =
    val changes0: List[FileEvent] aka "changes" = changes.aka["changes"]

    ambient1(())(handlers.watchedFiles0)(changes0)

  def `workspace/didChangeWorkspaceFolders`(event: WorkspaceFoldersChangeEvent): Unit =
    val event0: WorkspaceFoldersChangeEvent aka "event" = event.aka["event"]

    ambient1(())(handlers.foldersChanged0)(event0)

  def `workspace/willCreateFiles`(files: List[FileCreate]): Optional[WorkspaceEdit] =
    val files0: List[FileCreate] aka "files" = files.aka["files"]

    ambient1(Unset)(handlers.creatingFiles0)(files0)

  def `workspace/didCreateFiles`(files: List[FileCreate]): Unit =
    val files0: List[FileCreate] aka "files" = files.aka["files"]

    ambient1(())(handlers.createdFiles0)(files0)

  def `workspace/willRenameFiles`(files: List[FileRename]): Optional[WorkspaceEdit] =
    val files0: List[FileRename] aka "files" = files.aka["files"]

    ambient1(Unset)(handlers.renamingFiles0)(files0)

  def `workspace/didRenameFiles`(files: List[FileRename]): Unit =
    val files0: List[FileRename] aka "files" = files.aka["files"]

    ambient1(())(handlers.renamedFiles0)(files0)

  def `workspace/willDeleteFiles`(files: List[FileDelete]): Optional[WorkspaceEdit] =
    val files0: List[FileDelete] aka "files" = files.aka["files"]

    ambient1(Unset)(handlers.deletingFiles0)(files0)

  def `workspace/didDeleteFiles`(files: List[FileDelete]): Unit =
    val files0: List[FileDelete] aka "files" = files.aka["files"]

    ambient1(())(handlers.deletedFiles0)(files0)

  def `$/setTrace`(value: Text): Unit = trace0 = value

  def `completionItem/resolve`(item: CompletionItem): CompletionItem =
    val item0: CompletionItem aka "item" = item.aka["item"]

    ambient1(item)(handlers.resolveCompletion0)(item0)

  def `codeAction/resolve`(codeAction: CodeAction): CodeAction =
    val item0: CodeAction aka "item" = codeAction.aka["item"]

    ambient1(codeAction)(handlers.resolveCodeAction0)(item0)

  def `codeLens/resolve`(codeLens: CodeLens): CodeLens =
    val item0: CodeLens aka "item" = codeLens.aka["item"]

    ambient1(codeLens)(handlers.resolveCodeLens0)(item0)

  def `documentLink/resolve`(documentLink: DocumentLink): DocumentLink =
    val item0: DocumentLink aka "item" = documentLink.aka["item"]

    ambient1(documentLink)(handlers.resolveDocumentLink0)(item0)

  def `inlayHint/resolve`(inlayHint: InlayHint): InlayHint =
    val item0: InlayHint aka "item" = inlayHint.aka["item"]

    ambient1(inlayHint)(handlers.resolveInlayHint0)(item0)

  def `workspaceSymbol/resolve`(workspaceSymbol: WorkspaceSymbol): WorkspaceSymbol =
    val item0: WorkspaceSymbol aka "item" = workspaceSymbol.aka["item"]

    ambient1(workspaceSymbol)(handlers.resolveWorkspaceSymbol0)(item0)
