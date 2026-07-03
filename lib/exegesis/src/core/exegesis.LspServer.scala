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
import ethereal.*
import exoskeleton.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

import backstops.stackTraceBackstop
import executives.completions
import interpreters.posixInterpreter
import probates.awaitProbate
import strategies.throwUnsafely
import threading.virtualThreading

object LspServer:
  // The JSON-RPC dispatch is generated split across one dispatcher per `Lsp` sub-interface, rather
  // than as a single `serve[Lsp]` inside the trait. `JsonRpc.serve` inlines a schema-carrying codec
  // for every method it covers, so one dispatcher for the whole protocol — or even one on the
  // `LspServer` trait alongside its handlers and `main` — overflows the JVM per-class constant-pool
  // limit. Each sub-dispatcher is expanded in its own object, so it compiles into its own small
  // class; `dispatcher` routes an incoming request to the one whose interface declares its method
  // (a JSON-RPC response, which carries no method, is handled by any dispatcher, so the first
  // suffices).

  private object lifecycleRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      JsonRpc.serve[LspLifecycle](server)

  private object languageRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      JsonRpc.serve[LspLanguage](server)

  private object navigationRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      JsonRpc.serve[LspNavigation](server)

  private object editingRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      JsonRpc.serve[LspEditing](server)

  private object advancedRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      JsonRpc.serve[LspAdvanced](server)

  def dispatcher(server: Lsp): Json => Optional[Json] =
    import dynamicJsonAccess.enabled
    import strategies.throwUnsafely

    val routes: List[(Set[Text], Json => Optional[Json])] =
      List
        ( JsonRpc.methods[LspLifecycle]  -> lifecycleRoute(server),
          JsonRpc.methods[LspLanguage]   -> languageRoute(server),
          JsonRpc.methods[LspNavigation] -> navigationRoute(server),
          JsonRpc.methods[LspEditing]    -> editingRoute(server),
          JsonRpc.methods[LspAdvanced]   -> advancedRoute(server) )

    json =>
      safely(json.method.as[Text]).lay(routes.head._2(json)): method =>
        routes.find(_._1.contains(method)) match
          case Some((_, dispatch)) => dispatch(json)
          case None                => Unset

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

  def declaration(uri: Text, position: Position): List[Location] = Nil
  def typeDefinition(uri: Text, position: Position): List[Location] = Nil
  def implementation(uri: Text, position: Position): List[Location] = Nil
  def documentHighlights(uri: Text, position: Position): List[DocumentHighlight] = Nil
  def foldingRanges(uri: Text): List[FoldingRange] = Nil
  def selectionRanges(uri: Text, positions: List[Position]): List[SelectionRange] = Nil
  def documentLinks(uri: Text): List[DocumentLink] = Nil
  def codeLenses(uri: Text): List[CodeLens] = Nil
  def documentColors(uri: Text): List[ColorInformation] = Nil

  def colorPresentations(uri: Text, color: Color, range: Range): List[ColorPresentation] = Nil

  def formatRange(uri: Text, range: Range, options: FormattingOptions): List[TextEdit] = Nil

  def formatOnType(uri: Text, position: Position, character: Text, options: FormattingOptions)
  :   List[TextEdit] =

    Nil

  def prepareRename(uri: Text, position: Position): Optional[Range] = Unset
  def onWillSave(document: TextDocumentIdentifier, reason: TextDocumentSaveReason): Unit = ()

  def willSaveWaitUntil(document: TextDocumentIdentifier, reason: TextDocumentSaveReason)
  :   List[TextEdit] =

    Nil

  def prepareCallHierarchy(uri: Text, position: Position): List[CallHierarchyItem] = Nil
  def incomingCalls(item: CallHierarchyItem): List[CallHierarchyIncomingCall] = Nil
  def outgoingCalls(item: CallHierarchyItem): List[CallHierarchyOutgoingCall] = Nil
  def prepareTypeHierarchy(uri: Text, position: Position): List[TypeHierarchyItem] = Nil
  def supertypes(item: TypeHierarchyItem): List[TypeHierarchyItem] = Nil
  def subtypes(item: TypeHierarchyItem): List[TypeHierarchyItem] = Nil

  def semanticTokens(uri: Text): SemanticTokens = SemanticTokens()
  def semanticTokensRange(uri: Text, range: Range): SemanticTokens = SemanticTokens()

  def semanticTokensDelta(uri: Text, previousResultId: Text): SemanticTokensDelta =
    SemanticTokensDelta()

  def inlayHints(uri: Text, range: Range): List[InlayHint] = Nil

  def inlineValues(uri: Text, range: Range, context: InlineValueContext): List[InlineValueText] =
    Nil

  def linkedEditingRange(uri: Text, position: Position): Optional[LinkedEditingRanges] = Unset
  def monikers(uri: Text, position: Position): List[Moniker] = Nil

  def diagnostics(uri: Text, identifier: Optional[Text], previousResultId: Optional[Text])
  :   DocumentDiagnosticReport =

    DocumentDiagnosticReport()

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

  def `textDocument/declaration`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    declaration(textDocument.uri, position)

  def `textDocument/typeDefinition`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    typeDefinition(textDocument.uri, position)

  def `textDocument/implementation`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Location] =

    implementation(textDocument.uri, position)

  def `textDocument/documentHighlight`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[DocumentHighlight] =

    documentHighlights(textDocument.uri, position)

  def `textDocument/foldingRange`(textDocument: TextDocumentIdentifier): List[FoldingRange] =
    foldingRanges(textDocument.uri)

  def `textDocument/selectionRange`
    ( textDocument: TextDocumentIdentifier, positions: List[Position] )
  :   List[SelectionRange] =

    selectionRanges(textDocument.uri, positions)

  def `textDocument/documentLink`(textDocument: TextDocumentIdentifier): List[DocumentLink] =
    documentLinks(textDocument.uri)

  def `textDocument/codeLens`(textDocument: TextDocumentIdentifier): List[CodeLens] =
    codeLenses(textDocument.uri)

  def `textDocument/documentColor`(textDocument: TextDocumentIdentifier): List[ColorInformation] =
    documentColors(textDocument.uri)

  def `textDocument/colorPresentation`
    ( textDocument: TextDocumentIdentifier, color: Color, range: Range )
  :   List[ColorPresentation] =

    colorPresentations(textDocument.uri, color, range)

  def `textDocument/rangeFormatting`
    ( textDocument: TextDocumentIdentifier, range: Range, options: FormattingOptions )
  :   List[TextEdit] =

    formatRange(textDocument.uri, range, options)

  def `textDocument/onTypeFormatting`
    ( textDocument: TextDocumentIdentifier,
      position:     Position,
      ch:           Text,
      options:      FormattingOptions )
  :   List[TextEdit] =

    formatOnType(textDocument.uri, position, ch, options)

  def `textDocument/prepareRename`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[Range] =

    prepareRename(textDocument.uri, position)

  def `textDocument/willSave`
    ( textDocument: TextDocumentIdentifier, reason: TextDocumentSaveReason )
  :   Unit =

    onWillSave(textDocument, reason)

  def `textDocument/willSaveWaitUntil`
    ( textDocument: TextDocumentIdentifier, reason: TextDocumentSaveReason )
  :   List[TextEdit] =

    willSaveWaitUntil(textDocument, reason)

  def `textDocument/prepareCallHierarchy`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[CallHierarchyItem] =

    prepareCallHierarchy(textDocument.uri, position)

  def `callHierarchy/incomingCalls`(item: CallHierarchyItem): List[CallHierarchyIncomingCall] =
    incomingCalls(item)

  def `callHierarchy/outgoingCalls`(item: CallHierarchyItem): List[CallHierarchyOutgoingCall] =
    outgoingCalls(item)

  def `textDocument/prepareTypeHierarchy`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[TypeHierarchyItem] =

    prepareTypeHierarchy(textDocument.uri, position)

  def `typeHierarchy/supertypes`(item: TypeHierarchyItem): List[TypeHierarchyItem] =
    supertypes(item)

  def `typeHierarchy/subtypes`(item: TypeHierarchyItem): List[TypeHierarchyItem] =
    subtypes(item)

  def `textDocument/semanticTokens/full`(textDocument: TextDocumentIdentifier): SemanticTokens =
    semanticTokens(textDocument.uri)

  def `textDocument/semanticTokens/full/delta`
    ( textDocument: TextDocumentIdentifier, previousResultId: Text )
  :   SemanticTokensDelta =

    semanticTokensDelta(textDocument.uri, previousResultId)

  def `textDocument/semanticTokens/range`(textDocument: TextDocumentIdentifier, range: Range)
  :   SemanticTokens =

    semanticTokensRange(textDocument.uri, range)

  def `textDocument/inlayHint`(textDocument: TextDocumentIdentifier, range: Range)
  :   List[InlayHint] =

    inlayHints(textDocument.uri, range)

  def `textDocument/inlineValue`
    ( textDocument: TextDocumentIdentifier, range: Range, context: InlineValueContext )
  :   List[InlineValueText] =

    inlineValues(textDocument.uri, range, context)

  def `textDocument/linkedEditingRange`(textDocument: TextDocumentIdentifier, position: Position)
  :   Optional[LinkedEditingRanges] =

    linkedEditingRange(textDocument.uri, position)

  def `textDocument/moniker`(textDocument: TextDocumentIdentifier, position: Position)
  :   List[Moniker] =

    monikers(textDocument.uri, position)

  def `textDocument/diagnostic`
    ( textDocument: TextDocumentIdentifier,
      identifier: Optional[Text],
      previousResultId: Optional[Text] )
  :   DocumentDiagnosticReport =

    diagnostics(textDocument.uri, identifier, previousResultId)

  // The stdio transport. Reads `Content-Length`-framed JSON-RPC messages from standard input and
  // dispatches each to the methods above; a single asynchronous writer drains the outgoing channel
  // (both request responses, funnelled in via `put`, and server-initiated notifications such as
  // `publishDiagnostics`) and frames each message back onto standard output, so writes never
  // interleave.
  def serve()(using Stdio, Monitor, Probate): Unit =
    import charEncoders.utf8Encoder
    import strategies.throwUnsafely

    val dispatch: Json => Optional[Json] = LspServer.dispatcher(this)

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

  // A runnable entry point: an `LspServer` object carries its own `main`, running as an
  // Ethereal resident daemon over the stdio JSON-RPC transport. `main` is a plain (non-inline)
  // method, so the object's static `main([Ljava/lang/String;)V` forwarder is a valid JVM entry
  // point (`IArray[Text]` erases to `Array[String]`), and defining the server is enough to run
  // it — no `@main`, and no `cli`/`execute`/`supervise` boilerplate. Ethereal's `cli` reads the
  // real invocation from the daemon environment, so the JVM `args` are unused; it also provides
  // the `-Dbuild.executable=<name>` native-launcher assembly path. To externalize the classpath
  // with Burdock (shipping a thin launcher rather than a fat JAR), override this in the server's
  // own module with `override def main(args: IArray[Text]): Unit = externalize(super.main(args))`.
  def main(args: IArray[Text]): Unit = cli:
    execute:
      supervise(serve())
      Exit.Ok
