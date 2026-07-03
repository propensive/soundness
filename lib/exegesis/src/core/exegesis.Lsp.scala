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

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import jacinta.*
import obligatory.*
import vacuous.*

object Lsp:
  // Basic geometry. LSP and `Span` are both 0-based, so conversions are direct.

  object Position:
    def from(span: Span): Optional[Position] = span.startLine.let: line =>
      Position(line.n0, span.startColumn.lay(0)(_.n0))

  case class Position(line: Int, character: Int):
    def span: Span = Span.line(line.z, character.z, 0)

  object Range:
    def from(span: Span): Optional[Range] = span.startLine.let: startLine =>
      val startColumn = span.startColumn.lay(0)(_.n0)
      val endLine = span.endLine.lay(startLine.n0)(_.n0)
      val endColumn = span.endColumn.lay(startColumn)(_.n0)
      Range(Position(startLine.n0, startColumn), Position(endLine, endColumn))

  case class Range(start: Position, end: Position):
    def span: Span =
      Span.region(start.line.z, start.character.z, end.line.z, end.character.z)

  case class Location(uri: Text, range: Range)

  // Documents

  case class TextDocumentIdentifier(uri: Text)
  case class VersionedTextDocumentIdentifier(uri: Text, version: Int)
  case class TextDocumentItem(uri: Text, languageId: Text, version: Int, text: Text)
  case class TextDocumentContentChangeEvent(range: Optional[Range] = Unset, text: Text)

  // Lifecycle

  case class ClientInfo(name: Text, version: Optional[Text] = Unset)
  case class ServerInfo(name: Text, version: Optional[Text] = Unset)
  case class Folder(uri: Text, name: Text)

  case class CompletionOptions(triggerCharacters: Optional[List[Text]] = Unset)
  case class SignatureHelpOptions(triggerCharacters: Optional[List[Text]] = Unset)
  case class DocumentLinkOptions(resolveProvider: Optional[Boolean] = Unset)
  case class CodeLensOptions(resolveProvider: Optional[Boolean] = Unset)

  case class DocumentOnTypeFormattingOptions
    ( firstTriggerCharacter: Text, moreTriggerCharacter: Optional[List[Text]] = Unset )

  case class SemanticTokensOptions
    ( legend: SemanticTokensLegend,
      range:  Optional[Boolean] = Unset,
      full:   Optional[Boolean] = Unset )

  case class DiagnosticOptions
    ( identifier:            Optional[Text] = Unset,
      interFileDependencies: Boolean,
      workspaceDiagnostics:  Boolean )

  case class ServerCapabilities
    ( textDocumentSync:                 Optional[TextDocumentSyncKind]            = Unset,
      completionProvider:               Optional[CompletionOptions]               = Unset,
      hoverProvider:                    Optional[Boolean]                         = Unset,
      definitionProvider:               Optional[Boolean]                         = Unset,
      referencesProvider:               Optional[Boolean]                         = Unset,
      documentSymbolProvider:           Optional[Boolean]                         = Unset,
      documentFormattingProvider:       Optional[Boolean]                         = Unset,
      renameProvider:                   Optional[Boolean]                         = Unset,
      codeActionProvider:               Optional[Boolean]                         = Unset,
      signatureHelpProvider:            Optional[SignatureHelpOptions]            = Unset,
      declarationProvider:              Optional[Boolean]                         = Unset,
      typeDefinitionProvider:           Optional[Boolean]                         = Unset,
      implementationProvider:           Optional[Boolean]                         = Unset,
      documentHighlightProvider:        Optional[Boolean]                         = Unset,
      foldingRangeProvider:             Optional[Boolean]                         = Unset,
      selectionRangeProvider:           Optional[Boolean]                         = Unset,
      colorProvider:                    Optional[Boolean]                         = Unset,
      documentRangeFormattingProvider:  Optional[Boolean]                         = Unset,
      documentLinkProvider:             Optional[DocumentLinkOptions]             = Unset,
      codeLensProvider:                 Optional[CodeLensOptions]                 = Unset,
      documentOnTypeFormattingProvider: Optional[DocumentOnTypeFormattingOptions] = Unset,
      callHierarchyProvider:            Optional[Boolean]                         = Unset,
      typeHierarchyProvider:            Optional[Boolean]                         = Unset,
      semanticTokensProvider:           Optional[SemanticTokensOptions]           = Unset,
      inlayHintProvider:                Optional[Boolean]                         = Unset,
      inlineValueProvider:              Optional[Boolean]                         = Unset,
      linkedEditingRangeProvider:       Optional[Boolean]                         = Unset,
      monikerProvider:                  Optional[Boolean]                         = Unset,
      diagnosticProvider:               Optional[DiagnosticOptions]               = Unset )

  case class InitializeResult
    ( capabilities: ServerCapabilities, serverInfo: Optional[ServerInfo] = Unset )

  // Language features

  case class MarkupContent(kind: Text = t"markdown", value: Text)

  case class CompletionContext(triggerKind: Int, triggerCharacter: Optional[Text] = Unset)

  case class CompletionItem
    ( label:         Text,
      kind:          Optional[CompletionItemKind] = Unset,
      detail:        Optional[Text]               = Unset,
      documentation: Optional[MarkupContent]      = Unset,
      insertText:    Optional[Text]               = Unset )

  case class CompletionList(isIncomplete: Boolean = false, items: List[CompletionItem] = Nil)

  case class Hover(contents: MarkupContent, range: Optional[Range] = Unset)

  case class ReferenceContext(includeDeclaration: Boolean)

  case class DocumentSymbol
    ( name:           Text,
      kind:           SymbolKind,
      range:          Range,
      selectionRange: Range,
      detail:         Optional[Text]                 = Unset,
      children:       Optional[List[DocumentSymbol]] = Unset )

  case class FormattingOptions(tabSize: Int, insertSpaces: Boolean)
  case class TextEdit(range: Range, newText: Text)
  case class WorkspaceEdit(changes: Optional[Map[Text, List[TextEdit]]] = Unset)

  case class CodeActionContext(diagnostics: List[Diagnostic] = Nil)

  case class CodeAction
    ( title: Text,
      kind:  Optional[Text]          = Unset,
      edit:  Optional[WorkspaceEdit] = Unset )

  case class ParameterInformation(label: Text)

  case class SignatureInformation
    ( label:         Text,
      documentation: Optional[MarkupContent]              = Unset,
      parameters:    Optional[List[ParameterInformation]] = Unset )

  case class SignatureHelp
    ( signatures: List[SignatureInformation] = Nil, activeSignature: Optional[Int] = Unset )

  // Highlights, folding and selection

  case class DocumentHighlight(range: Range, kind: Optional[DocumentHighlightKind] = Unset)

  case class FoldingRange
    ( startLine:      Int,
      startCharacter: Optional[Int]  = Unset,
      endLine:        Int,
      endCharacter:   Optional[Int]  = Unset,
      kind:           Optional[Text] = Unset,
      collapsedText:  Optional[Text] = Unset )

  case class SelectionRange(range: Range, parent: Optional[SelectionRange] = Unset)

  // Links, lenses and commands

  case class Command
    ( title: Text, command: Text, arguments: Optional[List[Json]] = Unset )

  case class CodeLens
    ( range: Range, command: Optional[Command] = Unset, data: Optional[Json] = Unset )

  case class DocumentLink
    ( range:   Range,
      target:  Optional[Text] = Unset,
      tooltip: Optional[Text] = Unset,
      data:    Optional[Json] = Unset )

  // Colors

  case class Color(red: Double, green: Double, blue: Double, alpha: Double)
  case class ColorInformation(range: Range, color: Color)

  case class ColorPresentation
    ( label:               Text,
      textEdit:            Optional[TextEdit]       = Unset,
      additionalTextEdits: Optional[List[TextEdit]] = Unset )

  // Call and type hierarchies

  object CallHierarchyItem:
    given decodable: Tactic[JsonError] => CallHierarchyItem is Json.Decodable =
      Json.DecodableDerivation.derived

  case class CallHierarchyItem
    ( name:           Text,
      kind:           SymbolKind,
      tags:           Optional[List[SymbolTag]] = Unset,
      detail:         Optional[Text]            = Unset,
      uri:            Text,
      range:          Range,
      selectionRange: Range,
      data:           Optional[Json]            = Unset )

  case class CallHierarchyIncomingCall(from: CallHierarchyItem, fromRanges: List[Range])
  case class CallHierarchyOutgoingCall(to: CallHierarchyItem, fromRanges: List[Range])

  object TypeHierarchyItem:
    given decodable: Tactic[JsonError] => TypeHierarchyItem is Json.Decodable =
      Json.DecodableDerivation.derived

  case class TypeHierarchyItem
    ( name:           Text,
      kind:           SymbolKind,
      tags:           Optional[List[SymbolTag]] = Unset,
      detail:         Optional[Text]            = Unset,
      uri:            Text,
      range:          Range,
      selectionRange: Range,
      data:           Optional[Json]            = Unset )

  // Semantic tokens

  case class SemanticTokensLegend(tokenTypes: List[Text], tokenModifiers: List[Text])
  case class SemanticTokens(resultId: Optional[Text] = Unset, data: List[Int] = Nil)
  case class SemanticTokensEdit(start: Int, deleteCount: Int, data: Optional[List[Int]] = Unset)

  case class SemanticTokensDelta
    ( resultId: Optional[Text] = Unset, edits: List[SemanticTokensEdit] = Nil )

  // Inlay hints, inline values, linked editing and monikers

  case class InlayHint
    ( position:     Position,
      label:        Text,
      kind:         Optional[InlayHintKind] = Unset,
      tooltip:      Optional[Text]          = Unset,
      paddingLeft:  Optional[Boolean]       = Unset,
      paddingRight: Optional[Boolean]       = Unset,
      data:         Optional[Json]          = Unset )

  object InlineValueContext:
    given decodable: Tactic[JsonError] => InlineValueContext is Json.Decodable =
      Json.DecodableDerivation.derived

  case class InlineValueContext(frameId: Int, stoppedLocation: Range)
  case class InlineValueText(range: Range, text: Text)

  case class LinkedEditingRanges(ranges: List[Range], wordPattern: Optional[Text] = Unset)
  case class Moniker(scheme: Text, identifier: Text, unique: Text, kind: Optional[Text] = Unset)

  // Pull diagnostics

  case class DocumentDiagnosticReport
    ( kind: Text = t"full", resultId: Optional[Text] = Unset, items: List[Diagnostic] = Nil )

  // Diagnostics and window messages

  object Diagnostic:
    given decodable: Tactic[JsonError] => Diagnostic is Json.Decodable =
      Json.DecodableDerivation.derived

  case class Diagnostic
    ( range:    Range,
      severity: Optional[DiagnosticSeverity] = Unset,
      code:     Optional[Text]               = Unset,
      source:   Optional[Text]               = Unset,
      message:  Text )

  case class MessageActionItem(title: Text)

  // Integer-valued enumerations. The Language Server Protocol numbers these on the wire, so the
  // codecs map to integers rather than to strings (cf. the string-valued enums in synesthesia's
  // MCP module). Most are numbered from 1, so the ordinal is offset by one; `TextDocumentSyncKind`
  // is numbered from 0, so its ordinal is used directly.

  object DiagnosticSeverity:
    given encodable: DiagnosticSeverity is Json.Encodable =
      Json.Encodable(Morphology.Whole): severity => (severity.ordinal + 1).json

    given decodable: Tactic[JsonError] => DiagnosticSeverity is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => DiagnosticSeverity.fromOrdinal(json.as[Int] - 1)

  enum DiagnosticSeverity:
    case Error, Warning, Information, Hint

  object MessageType:
    given encodable: MessageType is Json.Encodable =
      Json.Encodable(Morphology.Whole): level => (level.ordinal + 1).json

    given decodable: Tactic[JsonError] => MessageType is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => MessageType.fromOrdinal(json.as[Int] - 1)

  enum MessageType:
    case Error, Warning, Info, Log

  object TextDocumentSyncKind:
    given encodable: TextDocumentSyncKind is Json.Encodable =
      Json.Encodable(Morphology.Whole)(_.ordinal.json)

    given decodable: Tactic[JsonError] => TextDocumentSyncKind is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => TextDocumentSyncKind.fromOrdinal(json.as[Int])

  enum TextDocumentSyncKind:
    case None, Full, Incremental

  object CompletionItemKind:
    given encodable: CompletionItemKind is Json.Encodable =
      Json.Encodable(Morphology.Whole): kind => (kind.ordinal + 1).json

    given decodable: Tactic[JsonError] => CompletionItemKind is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => CompletionItemKind.fromOrdinal(json.as[Int] - 1)

  enum CompletionItemKind:
    case Text, Method, Function, Constructor, Field, Variable, Class, Interface, Module, Property,
      Unit, Value, Enum, Keyword, Snippet, Color, File, Reference, Folder, EnumMember, Constant,
      Struct, Event, Operator, TypeParameter

  object SymbolKind:
    given encodable: SymbolKind is Json.Encodable =
      Json.Encodable(Morphology.Whole): kind => (kind.ordinal + 1).json

    given decodable: Tactic[JsonError] => SymbolKind is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => SymbolKind.fromOrdinal(json.as[Int] - 1)

  enum SymbolKind:
    case File, Module, Namespace, Package, Class, Method, Property, Field, Constructor, Enum,
      Interface, Function, Variable, Constant, `String`, Number, `Boolean`, `Array`, `Object`,
      Key, `Null`, EnumMember, Struct, Event, Operator, TypeParameter

  object DocumentHighlightKind:
    given encodable: DocumentHighlightKind is Json.Encodable =
      Json.Encodable(Morphology.Whole): kind => (kind.ordinal + 1).json

    given decodable: Tactic[JsonError] => DocumentHighlightKind is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => DocumentHighlightKind.fromOrdinal(json.as[Int] - 1)

  enum DocumentHighlightKind:
    case Text, Read, Write

  object TextDocumentSaveReason:
    given encodable: TextDocumentSaveReason is Json.Encodable =
      Json.Encodable(Morphology.Whole): reason => (reason.ordinal + 1).json

    given decodable: Tactic[JsonError] => TextDocumentSaveReason is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => TextDocumentSaveReason.fromOrdinal(json.as[Int] - 1)

  enum TextDocumentSaveReason:
    case Manual, AfterDelay, FocusOut

  object SymbolTag:
    given encodable: SymbolTag is Json.Encodable =
      Json.Encodable(Morphology.Whole): tag => (tag.ordinal + 1).json

    given decodable: Tactic[JsonError] => SymbolTag is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => SymbolTag.fromOrdinal(json.as[Int] - 1)

  enum SymbolTag:
    case Deprecated

  object InlayHintKind:
    given encodable: InlayHintKind is Json.Encodable =
      Json.Encodable(Morphology.Whole): kind => (kind.ordinal + 1).json

    given decodable: Tactic[JsonError] => InlayHintKind is Json.Decodable =
      Json.Decodable(Morphology.Whole): json => InlayHintKind.fromOrdinal(json.as[Int] - 1)

  enum InlayHintKind:
    case Type, Parameter

// The Language Server Protocol request/notification surface. It is split into several sub-traits
// purely so that each can be compiled into its own JSON-RPC dispatcher class: `JsonRpc.serve`
// inlines a schema-carrying codec for every method it covers, so a single dispatcher for the whole
// protocol would overflow the JVM per-class constant-pool limit. `LspServer.dispatcher` routes each
// request to the sub-dispatcher whose interface declares its method (see `JsonRpc.methods`).

trait LspLifecycle extends JsonRpc:

  @rpc
  def initialize
    ( processId:        Optional[Int],
      clientInfo:       Optional[Lsp.ClientInfo],
      locale:           Optional[Text],
      rootUri:          Optional[Text],
      capabilities:     Json,
      workspaceFolders: Optional[List[Lsp.Folder]] )
  :   Lsp.InitializeResult

  @rpc
  def initialized(): Unit

  @rpc
  def shutdown(): Json

  @rpc
  def exit(): Unit

  @rpc
  def `textDocument/didOpen`(textDocument: Lsp.TextDocumentItem): Unit

  @rpc
  def `textDocument/didChange`
    ( textDocument:   Lsp.VersionedTextDocumentIdentifier,
      contentChanges: List[Lsp.TextDocumentContentChangeEvent] )
  :   Unit

  @rpc
  def `textDocument/didSave`
    ( textDocument: Lsp.TextDocumentIdentifier, text: Optional[Text] )
  :   Unit

  @rpc
  def `textDocument/didClose`(textDocument: Lsp.TextDocumentIdentifier): Unit

trait LspLanguage extends JsonRpc:

  @rpc
  def `textDocument/completion`
    ( textDocument: Lsp.TextDocumentIdentifier,
      position:     Lsp.Position,
      context:      Optional[Lsp.CompletionContext] )
  :   Lsp.CompletionList

  @rpc
  def `textDocument/hover`(textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position)
  :   Optional[Lsp.Hover]

  @rpc
  def `textDocument/definition`(textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position)
  :   List[Lsp.Location]

  @rpc
  def `textDocument/references`
    ( textDocument: Lsp.TextDocumentIdentifier,
      position:     Lsp.Position,
      context:      Lsp.ReferenceContext )
  :   List[Lsp.Location]

  @rpc
  def `textDocument/documentSymbol`(textDocument: Lsp.TextDocumentIdentifier)
  :   List[Lsp.DocumentSymbol]

  @rpc
  def `textDocument/formatting`
    ( textDocument: Lsp.TextDocumentIdentifier, options: Lsp.FormattingOptions )
  :   List[Lsp.TextEdit]

  @rpc
  def `textDocument/rename`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position, newName: Text )
  :   Lsp.WorkspaceEdit

  @rpc
  def `textDocument/codeAction`
    ( textDocument: Lsp.TextDocumentIdentifier,
      range:        Lsp.Range,
      context:      Lsp.CodeActionContext )
  :   List[Lsp.CodeAction]

  @rpc
  def `textDocument/signatureHelp`(textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position)
  :   Optional[Lsp.SignatureHelp]

trait LspNavigation extends JsonRpc:

  @rpc
  def `textDocument/declaration`(textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position)
  :   List[Lsp.Location]

  @rpc
  def `textDocument/typeDefinition`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   List[Lsp.Location]

  @rpc
  def `textDocument/implementation`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   List[Lsp.Location]

  @rpc
  def `textDocument/documentHighlight`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   List[Lsp.DocumentHighlight]

  @rpc
  def `textDocument/foldingRange`(textDocument: Lsp.TextDocumentIdentifier)
  :   List[Lsp.FoldingRange]

  @rpc
  def `textDocument/selectionRange`
    ( textDocument: Lsp.TextDocumentIdentifier, positions: List[Lsp.Position] )
  :   List[Lsp.SelectionRange]

  @rpc
  def `textDocument/documentLink`(textDocument: Lsp.TextDocumentIdentifier)
  :   List[Lsp.DocumentLink]

  @rpc
  def `textDocument/codeLens`(textDocument: Lsp.TextDocumentIdentifier): List[Lsp.CodeLens]

  @rpc
  def `textDocument/documentColor`(textDocument: Lsp.TextDocumentIdentifier)
  :   List[Lsp.ColorInformation]

  @rpc
  def `textDocument/colorPresentation`
    ( textDocument: Lsp.TextDocumentIdentifier, color: Lsp.Color, range: Lsp.Range )
  :   List[Lsp.ColorPresentation]

trait LspEditing extends JsonRpc:

  @rpc
  def `textDocument/rangeFormatting`
    ( textDocument: Lsp.TextDocumentIdentifier,
      range:        Lsp.Range,
      options:      Lsp.FormattingOptions )
  :   List[Lsp.TextEdit]

  @rpc
  def `textDocument/onTypeFormatting`
    ( textDocument: Lsp.TextDocumentIdentifier,
      position:     Lsp.Position,
      ch:           Text,
      options:      Lsp.FormattingOptions )
  :   List[Lsp.TextEdit]

  @rpc
  def `textDocument/prepareRename`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   Optional[Lsp.Range]

  @rpc
  def `textDocument/willSave`
    ( textDocument: Lsp.TextDocumentIdentifier, reason: Lsp.TextDocumentSaveReason )
  :   Unit

  @rpc
  def `textDocument/willSaveWaitUntil`
    ( textDocument: Lsp.TextDocumentIdentifier, reason: Lsp.TextDocumentSaveReason )
  :   List[Lsp.TextEdit]

  @rpc
  def `textDocument/prepareCallHierarchy`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   List[Lsp.CallHierarchyItem]

  @rpc
  def `callHierarchy/incomingCalls`(item: Lsp.CallHierarchyItem)
  :   List[Lsp.CallHierarchyIncomingCall]

  @rpc
  def `callHierarchy/outgoingCalls`(item: Lsp.CallHierarchyItem)
  :   List[Lsp.CallHierarchyOutgoingCall]

  @rpc
  def `textDocument/prepareTypeHierarchy`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   List[Lsp.TypeHierarchyItem]

  @rpc
  def `typeHierarchy/supertypes`(item: Lsp.TypeHierarchyItem): List[Lsp.TypeHierarchyItem]

  @rpc
  def `typeHierarchy/subtypes`(item: Lsp.TypeHierarchyItem): List[Lsp.TypeHierarchyItem]

trait LspAdvanced extends JsonRpc:

  @rpc
  def `textDocument/semanticTokens/full`(textDocument: Lsp.TextDocumentIdentifier)
  :   Lsp.SemanticTokens

  @rpc
  def `textDocument/semanticTokens/full/delta`
    ( textDocument: Lsp.TextDocumentIdentifier, previousResultId: Text )
  :   Lsp.SemanticTokensDelta

  @rpc
  def `textDocument/semanticTokens/range`
    ( textDocument: Lsp.TextDocumentIdentifier, range: Lsp.Range )
  :   Lsp.SemanticTokens

  @rpc
  def `textDocument/inlayHint`(textDocument: Lsp.TextDocumentIdentifier, range: Lsp.Range)
  :   List[Lsp.InlayHint]

  @rpc
  def `textDocument/inlineValue`
    ( textDocument: Lsp.TextDocumentIdentifier,
      range:        Lsp.Range,
      context:      Lsp.InlineValueContext )
  :   List[Lsp.InlineValueText]

  @rpc
  def `textDocument/linkedEditingRange`
    ( textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position )
  :   Optional[Lsp.LinkedEditingRanges]

  @rpc
  def `textDocument/moniker`(textDocument: Lsp.TextDocumentIdentifier, position: Lsp.Position)
  :   List[Lsp.Moniker]

  @rpc
  def `textDocument/diagnostic`
    ( textDocument:     Lsp.TextDocumentIdentifier,
      identifier:       Optional[Text],
      previousResultId: Optional[Text] )
  :   Lsp.DocumentDiagnosticReport

// The full protocol: the union of every sub-interface, fixing `Origin` to `LspClient`. `LspServer`
// implements this; `LspServer.dispatcher` serves each sub-interface separately and routes by method.
trait Lsp
extends LspLifecycle, LspLanguage, LspNavigation, LspEditing, LspAdvanced:
  type Origin = LspClient
