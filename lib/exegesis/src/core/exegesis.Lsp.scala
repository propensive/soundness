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
import zephyrine.*

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

  case class CompletionOptions
    ( triggerCharacters: Optional[List[Text]] = Unset, resolveProvider: Optional[Boolean] = Unset )

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

  case class ExecuteCommandOptions(commands: List[Text])

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
      diagnosticProvider:               Optional[DiagnosticOptions]               = Unset,
      workspaceSymbolProvider:          Optional[Boolean]                         = Unset,
      executeCommandProvider:           Optional[ExecuteCommandOptions]           = Unset )

  case class InitializeResult
    ( capabilities: ServerCapabilities, serverInfo: Optional[ServerInfo] = Unset )

  // Language features

  case class MarkupContent(kind: Text = t"markdown", value: Text)

  case class CompletionContext(triggerKind: Int, triggerCharacter: Optional[Text] = Unset)

  object CompletionItem:
    given decodable: CompletionItem is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class CompletionItem
    ( label:         Text,
      kind:          Optional[CompletionItemKind] = Unset,
      detail:        Optional[Text]               = Unset,
      documentation: Optional[MarkupContent]      = Unset,
      insertText:    Optional[Text]               = Unset,
      data:          Optional[Json]               = Unset )

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

  object WorkspaceEdit:
    given decodable: WorkspaceEdit is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class WorkspaceEdit(changes: Optional[Map[Text, List[TextEdit]]] = Unset)

  case class CodeActionContext(diagnostics: List[Diagnostic] = Nil)

  object CodeAction:
    given decodable: CodeAction is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class CodeAction
    ( title: Text,
      kind:  Optional[Text]          = Unset,
      edit:  Optional[WorkspaceEdit] = Unset,
      data:  Optional[Json]          = Unset )

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

  // `arguments` is an arbitrary JSON array (`LSPAny[]`), kept as raw `Json`.
  case class Command
    ( title: Text, command: Text, arguments: Optional[Json] = Unset )

  object CodeLens:
    given decodable: CodeLens is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class CodeLens
    ( range: Range, command: Optional[Command] = Unset, data: Optional[Json] = Unset )

  object DocumentLink:
    given decodable: DocumentLink is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

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
    given decodable: CallHierarchyItem is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

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
    given decodable: TypeHierarchyItem is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

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

  object InlayHint:
    given decodable: InlayHint is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class InlayHint
    ( position:     Position,
      label:        Text,
      kind:         Optional[InlayHintKind] = Unset,
      tooltip:      Optional[Text]          = Unset,
      paddingLeft:  Optional[Boolean]       = Unset,
      paddingRight: Optional[Boolean]       = Unset,
      data:         Optional[Json]          = Unset )

  object InlineValueContext:
    given decodable: InlineValueContext is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class InlineValueContext(frameId: Int, stoppedLocation: Range)
  case class InlineValueText(range: Range, text: Text)

  case class LinkedEditingRanges(ranges: List[Range], wordPattern: Optional[Text] = Unset)
  case class Moniker(scheme: Text, identifier: Text, unique: Text, kind: Optional[Text] = Unset)

  // Pull diagnostics

  case class DocumentDiagnosticReport
    ( kind: Text = t"full", resultId: Optional[Text] = Unset, items: List[Diagnostic] = Nil )

  // Workspace

  object WorkspaceSymbol:
    given decodable: WorkspaceSymbol is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class WorkspaceSymbol
    ( name:          Text,
      kind:          SymbolKind,
      tags:          Optional[List[SymbolTag]] = Unset,
      location:      Location,
      containerName: Optional[Text]            = Unset,
      data:          Optional[Json]            = Unset )

  object FileEvent:
    given decodable: FileEvent is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class FileEvent(uri: Text, `type`: FileChangeType)

  object WorkspaceFoldersChangeEvent:
    given decodable: WorkspaceFoldersChangeEvent is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class WorkspaceFoldersChangeEvent(added: List[Folder], removed: List[Folder])

  object FileCreate:
    given decodable: FileCreate is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class FileCreate(uri: Text)

  object FileRename:
    given decodable: FileRename is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class FileRename(oldUri: Text, newUri: Text)

  object FileDelete:
    given decodable: FileDelete is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class FileDelete(uri: Text)

  // Diagnostics and window messages

  object Diagnostic:
    given decodable: Diagnostic is Json.Decodable =
      // A pure, throwing instance: each internal summon of the derivation mints its own
      // throwing tactic, and a decode failure surfaces as a `JsonError` handled at the
      // transport. Threading a caller's tactic through the capture-polymorphic derivation
      // is rejected by separation checking; sealed per jacinta's codec-thunk pattern.
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

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
      Json.Encodable(() => Morphology.Whole): severity => (severity.ordinal + 1).in[Json]

    given decodable: DiagnosticSeverity is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => DiagnosticSeverity.fromOrdinal(json.as[Int] - 1)

  enum DiagnosticSeverity:
    case Error, Warning, Information, Hint

  object MessageType:
    given encodable: MessageType is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): level => (level.ordinal + 1).in[Json]

    given decodable: MessageType is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => MessageType.fromOrdinal(json.as[Int] - 1)

  enum MessageType:
    case Error, Warning, Info, Log

  object TextDocumentSyncKind:
    given encodable: TextDocumentSyncKind is Json.Encodable =
      Json.Encodable(() => Morphology.Whole)(_.ordinal.in[Json])

    given decodable: TextDocumentSyncKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => TextDocumentSyncKind.fromOrdinal(json.as[Int])

  enum TextDocumentSyncKind:
    case None, Full, Incremental

  object CompletionItemKind:
    given encodable: CompletionItemKind is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): kind => (kind.ordinal + 1).in[Json]

    given decodable: CompletionItemKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => CompletionItemKind.fromOrdinal(json.as[Int] - 1)

  enum CompletionItemKind:
    case Text, Method, Function, Constructor, Field, Variable, Class, Interface, Module, Property,
      Unit, Value, Enum, Keyword, Snippet, Color, File, Reference, Folder, EnumMember, Constant,
      Struct, Event, Operator, TypeParameter

  object SymbolKind:
    given encodable: SymbolKind is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): kind => (kind.ordinal + 1).in[Json]

    given decodable: SymbolKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => SymbolKind.fromOrdinal(json.as[Int] - 1)

  enum SymbolKind:
    case File, Module, Namespace, Package, Class, Method, Property, Field, Constructor, Enum,
      Interface, Function, Variable, Constant, `String`, Number, `Boolean`, `Array`, `Object`,
      Key, `Null`, EnumMember, Struct, Event, Operator, TypeParameter

  object DocumentHighlightKind:
    given encodable: DocumentHighlightKind is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): kind => (kind.ordinal + 1).in[Json]

    given decodable: DocumentHighlightKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json =>
          DocumentHighlightKind.fromOrdinal(json.as[Int] - 1)

  enum DocumentHighlightKind:
    case Text, Read, Write

  object TextDocumentSaveReason:
    given encodable: TextDocumentSaveReason is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): reason => (reason.ordinal + 1).in[Json]

    given decodable: TextDocumentSaveReason is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json =>
          TextDocumentSaveReason.fromOrdinal(json.as[Int] - 1)

  enum TextDocumentSaveReason:
    case Manual, AfterDelay, FocusOut

  object SymbolTag:
    given encodable: SymbolTag is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): tag => (tag.ordinal + 1).in[Json]

    given decodable: SymbolTag is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => SymbolTag.fromOrdinal(json.as[Int] - 1)

  enum SymbolTag:
    case Deprecated

  object InlayHintKind:
    given encodable: InlayHintKind is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): kind => (kind.ordinal + 1).in[Json]

    given decodable: InlayHintKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => InlayHintKind.fromOrdinal(json.as[Int] - 1)

  enum InlayHintKind:
    case Type, Parameter

  object FileChangeType:
    given encodable: FileChangeType is Json.Encodable =
      Json.Encodable(() => Morphology.Whole): kind => (kind.ordinal + 1).in[Json]

    given decodable: FileChangeType is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the wire-integer decode cannot
      // thread a caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Whole): json => FileChangeType.fromOrdinal(json.as[Int] - 1)

  enum FileChangeType:
    case Created, Changed, Deleted

  object Document:
    private[exegesis] def state(uri: Text, language: Text, version: Int, text: Text): State =
      val state = State(uri, language)
      state.version0 = version
      state.text0 = text
      state

    // The store-side record of one open document: pure, mutable data owned by the session and
    // mutated only by the single-threaded dispatch loop. The line index is built lazily and
    // invalidated by every edit. All offsets are in UTF-16 code units, which is both the
    // protocol's default position encoding and the unit of `Text`'s underlying characters, so no
    // transcoding is needed.
    private[exegesis] class State(val uri: Text, val language: Text):
      @scala.caps.unsafe.untrackedCaptures private[Document] var version0: Int = 0
      @scala.caps.unsafe.untrackedCaptures private[Document] var text0: Text = t""
      @scala.caps.unsafe.untrackedCaptures private var index: Optional[scala.Array[Int]] = Unset

      def version: Int = version0
      def text: Text = text0

      // Offsets of the first character of each line. Lines are delimited by `\n`, `\r\n` or `\r`,
      // per the protocol.
      def lineStarts: scala.Array[Int] = index.or:
        val buffer = scm.ArrayBuffer[Int](0)
        val string: String = text.s
        var next: Int = 0

        while next < string.length do
          val char = string.charAt(next)

          if char == '\n' then buffer += next + 1
          else if char == '\r' then
            if next + 1 < string.length && string.charAt(next + 1) == '\n' then next += 1
            buffer += next + 1

          next += 1

        buffer.toArray.tap: starts => index = starts

      // The offset of a protocol position, clamped (as the protocol requires) to the document: a
      // line beyond the end resolves to the end of the text, and a character beyond the end of
      // its line resolves to the end of that line.
      def offset(position: Position): Int =
        val starts = lineStarts

        if position.line < 0 then 0
        else if position.line >= starts.length then text.s.length
        else
          val start = starts(position.line)

          val limit =
            if position.line + 1 < starts.length then starts(position.line + 1) else text.s.length

          java.lang.Math.min(start + java.lang.Math.max(position.character, 0), limit)

      def position(offset: Int): Position =
        val starts = lineStarts
        var line: Int = starts.length - 1
        while line > 0 && starts(line) > offset do line -= 1
        Position(line, offset - starts(line))

      // Applies a `didChange` batch in order: a ranged event splices at UTF-16 offsets; an
      // unranged event replaces the whole text, so full-sync clients work unchanged.
      // The chain runs on the stdlib view with an explicit match rather than umbrella `each` and
      // `let`/`or`: extension resolution inside a lambda typed under the enclosing call's live
      // type variables crashes the compiler (the `wildApprox` assertion; scala/scala3#24824).
      def edit(version: Int, changes: List[TextDocumentContentChangeEvent]): Unit =
        changes.stdlib.foreach: change =>
          change.range match
            case range: Range =>
              val string = text.s
              val start = offset(range.start)
              val end = java.lang.Math.max(offset(range.end), start)

              text0 =
                (string.substring(0, start).nn + change.text.s + string.substring(end).nn).tt

            case _ =>
              text0 = change.text

          index = Unset

        version0 = version

  // A live view of one open document, lent to a handler for the duration of one dispatch. The
  // handle is a capability, so it cannot leave the handler's scope; the `Text` values it returns
  // are pure snapshots, which may. Positions and offsets are in UTF-16 code units, the protocol's
  // default encoding.
  class Document private[exegesis] (state: Document.State) extends caps.ExclusiveCapability:
    def uri: Text = state.uri
    def language: Text = state.language
    def version: Int = state.version
    def text: Text = state.text
    def lineCount: Int = state.lineStarts.length

    // The content of a line, without its terminator.
    def line(number: Int): Optional[Text] =
      val starts = state.lineStarts

      if number < 0 || number >= starts.length then Unset else
        def terminal(char: Char): Boolean = char == '\n' || char == '\r'
        val string = state.text.s
        val start = starts(number)
        val end = if number + 1 < starts.length then starts(number + 1) else string.length
        var limit = end
        while limit > start && terminal(string.charAt(limit - 1)) do limit -= 1
        string.substring(start, limit).nn.tt

    // The text within a protocol range.
    def apply(range: Range): Text =
      val start = state.offset(range.start)
      val end = java.lang.Math.max(state.offset(range.end), start)
      state.text.s.substring(start, end).nn.tt

    def offset(position: Position): Ordinal = state.offset(position).z
    def position(offset: Ordinal): Position = state.position(offset.n0)

    // The word (a run of letters, digits and underscores) under the given position, if any.
    def word(position: Position): Optional[Text] =
      val string = state.text.s
      val point = state.offset(position)
      def wordChar(char: Char): Boolean = Character.isLetterOrDigit(char) || char == '_'
      var start: Int = point
      while start > 0 && wordChar(string.charAt(start - 1)) do start -= 1
      var end: Int = point
      while end < string.length && wordChar(string.charAt(end)) do end += 1
      if start == end then Unset else string.substring(start, end).nn.tt

    def fullRange: Range = Range(Position(0, 0), state.position(state.text.s.length))

  // The workspace facade lent to every handler: read access to the open-document store and to
  // the details the client reported at initialization, plus the client handle for sending
  // notifications. Lookups return pure snapshots, which may outlive the handler; the handle
  // itself may not.
  class Workspace private[exegesis] (session: LspSession^) extends caps.ExclusiveCapability:
    def documents: List[Text] = session.uris
    def document(uri: Text): Optional[TextDocumentItem] = session.snapshot(uri)
    def processId: Optional[Int] = session.processId0
    def clientInfo: Optional[ClientInfo] = session.clientInfo0
    def root: Optional[Text] = session.rootUri0
    def folders: List[Folder] = session.folders0
    def clientCapabilities: Json = session.clientCapabilities0
    def trace: Optional[Text] = session.trace0
    def client: LspClient^{this} = session.client0

  // Handler context shapes. A document-scoped (`Focused`) handler is lent the current document,
  // the workspace and an error emitter; a workspace-scoped (`Ambient`) handler gets no single
  // subject document. Request payloads are tagged context parameters (`aka`), read in handler
  // bodies through the accessors below. Each shape is a single context-parameter clause: results
  // are pure, so nothing lent to a handler can leave it — by return value, closure or task.
  type Focused[result] =
    (document: Document^, workspace: Workspace^, emit: Emit[LspError]^) ?=> result

  type Focused1[payload, result] =
    (payload: payload, document: Document^, workspace: Workspace^, emit: Emit[LspError]^)
    ?=> result

  type Focused2[payload1, payload2, result] =
    ( payload1:  payload1,
      payload2:  payload2,
      document:  Document^,
      workspace: Workspace^,
      emit:      Emit[LspError]^ )
    ?=> result

  type Focused3[payload1, payload2, payload3, result] =
    ( payload1:  payload1,
      payload2:  payload2,
      payload3:  payload3,
      document:  Document^,
      workspace: Workspace^,
      emit:      Emit[LspError]^ )
    ?=> result

  type Ambient[result] = (workspace: Workspace^, emit: Emit[LspError]^) ?=> result

  type Ambient1[payload, result] =
    (payload: payload, workspace: Workspace^, emit: Emit[LspError]^) ?=> result

  // The handler signature of each feature, named for its registration combinator.

  type ReadyHandler = Ambient[Unit]
  type TerminatingHandler = Ambient[Unit]
  type OpenedHandler = Focused[Unit]
  type ChangedHandler = Focused1[List[TextDocumentContentChangeEvent] aka "changes", Unit]
  type SavedHandler = Focused1[Optional[Text] aka "savedText", Unit]
  type ClosedHandler = Focused[Unit]
  type SavingHandler = Focused1[TextDocumentSaveReason aka "reason", Unit]
  type SavingEditsHandler = Focused1[TextDocumentSaveReason aka "reason", List[TextEdit]]
  type HoverHandler = Focused1[Position aka "position", Optional[Hover]]

  type CompleteHandler =
    Focused2[Position aka "position", Optional[CompletionContext] aka "context", CompletionList]

  type DefinitionHandler = Focused1[Position aka "position", List[Location]]

  type ReferencesHandler =
    Focused2[Position aka "position", Boolean aka "includeDeclaration", List[Location]]

  type DocumentSymbolsHandler = Focused[List[DocumentSymbol]]
  type FormatHandler = Focused1[FormattingOptions aka "options", List[TextEdit]]
  type RenameHandler = Focused2[Position aka "position", Text aka "newName", WorkspaceEdit]

  type CodeActionsHandler =
    Focused2[Range aka "range", CodeActionContext aka "context", List[CodeAction]]

  type SignatureHelpHandler = Focused1[Position aka "position", Optional[SignatureHelp]]
  type DeclarationHandler = Focused1[Position aka "position", List[Location]]
  type TypeDefinitionHandler = Focused1[Position aka "position", List[Location]]
  type ImplementationHandler = Focused1[Position aka "position", List[Location]]
  type DocumentHighlightsHandler = Focused1[Position aka "position", List[DocumentHighlight]]
  type FoldingRangesHandler = Focused[List[FoldingRange]]
  type SelectionRangesHandler = Focused1[List[Position] aka "positions", List[SelectionRange]]
  type DocumentLinksHandler = Focused[List[DocumentLink]]
  type CodeLensesHandler = Focused[List[CodeLens]]
  type DocumentColorsHandler = Focused[List[ColorInformation]]

  type ColorPresentationsHandler =
    Focused2[Color aka "color", Range aka "range", List[ColorPresentation]]

  type FormatRangeHandler =
    Focused2[Range aka "range", FormattingOptions aka "options", List[TextEdit]]

  type FormatOnTypeHandler =
    Focused3
      [ Position aka "position",
        Text aka "character",
        FormattingOptions aka "options",
        List[TextEdit] ]

  type PrepareRenameHandler = Focused1[Position aka "position", Optional[Range]]
  type CallHierarchyHandler = Focused1[Position aka "position", List[CallHierarchyItem]]

  type IncomingCallsHandler =
    Ambient1[CallHierarchyItem aka "item", List[CallHierarchyIncomingCall]]

  type OutgoingCallsHandler =
    Ambient1[CallHierarchyItem aka "item", List[CallHierarchyOutgoingCall]]

  type TypeHierarchyHandler = Focused1[Position aka "position", List[TypeHierarchyItem]]
  type SupertypesHandler = Ambient1[TypeHierarchyItem aka "item", List[TypeHierarchyItem]]
  type SubtypesHandler = Ambient1[TypeHierarchyItem aka "item", List[TypeHierarchyItem]]
  type SemanticTokensHandler = Focused[SemanticTokens]
  type SemanticTokensDeltaHandler = Focused1[Text aka "previousResultId", SemanticTokensDelta]
  type SemanticTokensRangeHandler = Focused1[Range aka "range", SemanticTokens]
  type InlayHintsHandler = Focused1[Range aka "range", List[InlayHint]]

  type InlineValuesHandler =
    Focused2[Range aka "range", InlineValueContext aka "context", List[InlineValueText]]

  type LinkedEditingRangeHandler = Focused1[Position aka "position", Optional[LinkedEditingRanges]]
  type MonikersHandler = Focused1[Position aka "position", List[Moniker]]

  type DiagnosticsHandler =
    Focused2
      [ Optional[Text] aka "identifier",
        Optional[Text] aka "previousResultId",
        DocumentDiagnosticReport ]

  type WorkspaceSymbolsHandler = Ambient1[Text aka "query", List[WorkspaceSymbol]]
  type CommandHandler = Ambient1[Optional[List[Json]] aka "arguments", Optional[Json]]
  type ConfigurationHandler = Ambient1[Json aka "settings", Unit]
  type WatchedFilesHandler = Ambient1[List[FileEvent] aka "changes", Unit]
  type FoldersChangedHandler = Ambient1[WorkspaceFoldersChangeEvent aka "event", Unit]
  type CreatingFilesHandler = Ambient1[List[FileCreate] aka "files", Optional[WorkspaceEdit]]
  type CreatedFilesHandler = Ambient1[List[FileCreate] aka "files", Unit]
  type RenamingFilesHandler = Ambient1[List[FileRename] aka "files", Optional[WorkspaceEdit]]
  type RenamedFilesHandler = Ambient1[List[FileRename] aka "files", Unit]
  type DeletingFilesHandler = Ambient1[List[FileDelete] aka "files", Optional[WorkspaceEdit]]
  type DeletedFilesHandler = Ambient1[List[FileDelete] aka "files", Unit]
  type ResolveHandler[item] = Ambient1[item aka "item", item]

  // Contextual accessors for the handles lent to a handler.

  transparent inline def document(using document: Document^): document.type = document
  transparent inline def workspace(using workspace: Workspace^): workspace.type = workspace
  inline def client(using workspace: Workspace^): LspClient^{workspace} = workspace.client

  // Contextual accessors for tagged request payloads.

  inline def position(using position: Position aka "position"): Position = position()
  inline def range(using range: Range aka "range"): Range = range()

  inline def options(using options: FormattingOptions aka "options"): FormattingOptions =
    options()

  inline def newName(using newName: Text aka "newName"): Text = newName()
  inline def character(using character: Text aka "character"): Text = character()

  inline def savedText(using savedText: Optional[Text] aka "savedText"): Optional[Text] =
    savedText()

  inline def reason(using reason: TextDocumentSaveReason aka "reason"): TextDocumentSaveReason =
    reason()

  inline def color(using color: Color aka "color"): Color = color()

  inline def positions(using positions: List[Position] aka "positions"): List[Position] =
    positions()

  inline def previousResultId(using previousResultId: Text aka "previousResultId"): Text =
    previousResultId()

  inline def identifier(using identifier: Optional[Text] aka "identifier"): Optional[Text] =
    identifier()

  inline def query(using query: Text aka "query"): Text = query()

  inline def arguments(using arguments: Optional[List[Json]] aka "arguments")
  :   Optional[List[Json]] =

    arguments()

  inline def settings(using settings: Json aka "settings"): Json = settings()

  inline def event(using event: WorkspaceFoldersChangeEvent aka "event")
  :   WorkspaceFoldersChangeEvent =

    event()

  inline def includeDeclaration(using includeDeclaration: Boolean aka "includeDeclaration")
  :   Boolean =

    includeDeclaration()

  inline def item[item](using item: item aka "item"): item = item()
  inline def context[context](using context: context aka "context"): context = context()
  inline def changes[changes](using changes: changes aka "changes"): changes = changes()
  inline def files[files](using files: files aka "files"): files = files()

  // Registration combinators, callable only where a registry is in scope: within the block given
  // to `listen`, or a helper method that takes `(using LspRegistry^)`. Each records its handler
  // in the registry, from which the server's capabilities are derived; options a capability needs
  // beyond mere presence (trigger characters, legends, diagnostic options) are parameters of the
  // registration, never guessed. Inline, so the registry capability flows from the use site
  // rather than into a fresh root minted by a method boundary.

  transparent inline def ready(handler: ReadyHandler)(using registry: LspRegistry^): Unit =
    registry.ready0 = LspRegistry.Slot[ReadyHandler](handler)

  transparent inline def terminating(handler: TerminatingHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.terminating0 = LspRegistry.Slot[TerminatingHandler](handler)

  transparent inline def opened(handler: OpenedHandler)(using registry: LspRegistry^): Unit =
    registry.opened0 = LspRegistry.Slot[OpenedHandler](handler)

  transparent inline def changed(handler: ChangedHandler)(using registry: LspRegistry^): Unit =
    registry.changed0 = LspRegistry.Slot[ChangedHandler](handler)

  transparent inline def saved(handler: SavedHandler)(using registry: LspRegistry^): Unit =
    registry.saved0 = LspRegistry.Slot[SavedHandler](handler)

  transparent inline def closed(handler: ClosedHandler)(using registry: LspRegistry^): Unit =
    registry.closed0 = LspRegistry.Slot[ClosedHandler](handler)

  transparent inline def saving(handler: SavingHandler)(using registry: LspRegistry^): Unit =
    registry.saving0 = LspRegistry.Slot[SavingHandler](handler)

  transparent inline def savingEdits(handler: SavingEditsHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.savingEdits0 = LspRegistry.Slot[SavingEditsHandler](handler)

  transparent inline def hover(handler: HoverHandler)(using registry: LspRegistry^): Unit =
    registry.hover0 = LspRegistry.Slot[HoverHandler](handler)

  transparent inline def complete(triggers: Text*)(handler: CompleteHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.complete0 = LspRegistry.Slot[CompleteHandler](handler)
    registry.completeTriggers0 = triggers.to(List)

  transparent inline def definition(handler: DefinitionHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.definition0 = LspRegistry.Slot[DefinitionHandler](handler)

  transparent inline def references(handler: ReferencesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.references0 = LspRegistry.Slot[ReferencesHandler](handler)

  transparent inline def documentSymbols(handler: DocumentSymbolsHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.documentSymbols0 = LspRegistry.Slot[DocumentSymbolsHandler](handler)

  transparent inline def format(handler: FormatHandler)(using registry: LspRegistry^): Unit =
    registry.format0 = LspRegistry.Slot[FormatHandler](handler)

  transparent inline def rename(handler: RenameHandler)(using registry: LspRegistry^): Unit =
    registry.rename0 = LspRegistry.Slot[RenameHandler](handler)

  transparent inline def codeActions(handler: CodeActionsHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.codeActions0 = LspRegistry.Slot[CodeActionsHandler](handler)

  transparent inline def signatureHelp(triggers: Text*)(handler: SignatureHelpHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.signatureHelp0 = LspRegistry.Slot[SignatureHelpHandler](handler)
    registry.signatureHelpTriggers0 = triggers.to(List)

  transparent inline def declaration(handler: DeclarationHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.declaration0 = LspRegistry.Slot[DeclarationHandler](handler)

  transparent inline def typeDefinition(handler: TypeDefinitionHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.typeDefinition0 = LspRegistry.Slot[TypeDefinitionHandler](handler)

  transparent inline def implementation(handler: ImplementationHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.implementation0 = LspRegistry.Slot[ImplementationHandler](handler)

  transparent inline def documentHighlights(handler: DocumentHighlightsHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.documentHighlights0 = LspRegistry.Slot[DocumentHighlightsHandler](handler)

  transparent inline def foldingRanges(handler: FoldingRangesHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.foldingRanges0 = LspRegistry.Slot[FoldingRangesHandler](handler)

  transparent inline def selectionRanges(handler: SelectionRangesHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.selectionRanges0 = LspRegistry.Slot[SelectionRangesHandler](handler)

  transparent inline def documentLinks(handler: DocumentLinksHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.documentLinks0 = LspRegistry.Slot[DocumentLinksHandler](handler)

  transparent inline def codeLenses(handler: CodeLensesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.codeLenses0 = LspRegistry.Slot[CodeLensesHandler](handler)

  transparent inline def documentColors(handler: DocumentColorsHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.documentColors0 = LspRegistry.Slot[DocumentColorsHandler](handler)

  transparent inline def colorPresentations(handler: ColorPresentationsHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.colorPresentations0 = LspRegistry.Slot[ColorPresentationsHandler](handler)

  transparent inline def formatRange(handler: FormatRangeHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.formatRange0 = LspRegistry.Slot[FormatRangeHandler](handler)

  transparent inline def formatOnType(first: Text, more: Text*)(handler: FormatOnTypeHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.formatOnType0 = LspRegistry.Slot[FormatOnTypeHandler](handler)
    registry.formatOnTypeFirst0 = first
    registry.formatOnTypeMore0 = more.to(List)

  transparent inline def prepareRename(handler: PrepareRenameHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.prepareRename0 = LspRegistry.Slot[PrepareRenameHandler](handler)

  // The three call-hierarchy handlers are registered together, so the capability is atomic.
  transparent inline def callHierarchy(prepare: CallHierarchyHandler)
    ( incoming: IncomingCallsHandler, outgoing: OutgoingCallsHandler )
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.prepareCallHierarchy0 = LspRegistry.Slot[CallHierarchyHandler](prepare)
    registry.incomingCalls0 = LspRegistry.Slot[IncomingCallsHandler](incoming)
    registry.outgoingCalls0 = LspRegistry.Slot[OutgoingCallsHandler](outgoing)

  transparent inline def typeHierarchy(prepare: TypeHierarchyHandler)
    ( supertypes: SupertypesHandler, subtypes: SubtypesHandler )
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.prepareTypeHierarchy0 = LspRegistry.Slot[TypeHierarchyHandler](prepare)
    registry.supertypes0 = LspRegistry.Slot[SupertypesHandler](supertypes)
    registry.subtypes0 = LspRegistry.Slot[SubtypesHandler](subtypes)

  transparent inline def semanticTokens(legend: SemanticTokensLegend)
    ( handler: SemanticTokensHandler )
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.semanticTokens0 = LspRegistry.Slot[SemanticTokensHandler](handler)
    registry.semanticTokensLegend0 = legend

  transparent inline def semanticTokensDelta(handler: SemanticTokensDeltaHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.semanticTokensDelta0 = LspRegistry.Slot[SemanticTokensDeltaHandler](handler)

  transparent inline def semanticTokensRange(handler: SemanticTokensRangeHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.semanticTokensRange0 = LspRegistry.Slot[SemanticTokensRangeHandler](handler)

  transparent inline def inlayHints(handler: InlayHintsHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.inlayHints0 = LspRegistry.Slot[InlayHintsHandler](handler)

  transparent inline def inlineValues(handler: InlineValuesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.inlineValues0 = LspRegistry.Slot[InlineValuesHandler](handler)

  transparent inline def linkedEditingRange(handler: LinkedEditingRangeHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.linkedEditingRange0 = LspRegistry.Slot[LinkedEditingRangeHandler](handler)

  transparent inline def monikers(handler: MonikersHandler)(using registry: LspRegistry^): Unit =
    registry.monikers0 = LspRegistry.Slot[MonikersHandler](handler)

  transparent inline def diagnostics(options: DiagnosticOptions)(handler: DiagnosticsHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.diagnostics0 = LspRegistry.Slot[DiagnosticsHandler](handler)
    registry.diagnosticOptions0 = options

  transparent inline def workspaceSymbols(handler: WorkspaceSymbolsHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.workspaceSymbols0 = LspRegistry.Slot[WorkspaceSymbolsHandler](handler)

  // Each command is registered under its own name; `workspace/executeCommand` requests are
  // multiplexed across them, and an unregistered command yields an `InvalidParams` error
  // response.
  transparent inline def command(name: Text)(handler: CommandHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.commands0 =
      (name, LspRegistry.Slot[CommandHandler](handler): AnyRef) :: registry.commands0
    registry.commandNames0 = name :: registry.commandNames0

  transparent inline def configuration(handler: ConfigurationHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.configuration0 = LspRegistry.Slot[ConfigurationHandler](handler)

  transparent inline def watchedFiles(handler: WatchedFilesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.watchedFiles0 = LspRegistry.Slot[WatchedFilesHandler](handler)

  transparent inline def foldersChanged(handler: FoldersChangedHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.foldersChanged0 = LspRegistry.Slot[FoldersChangedHandler](handler)

  transparent inline def creatingFiles(handler: CreatingFilesHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.creatingFiles0 = LspRegistry.Slot[CreatingFilesHandler](handler)

  transparent inline def createdFiles(handler: CreatedFilesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.createdFiles0 = LspRegistry.Slot[CreatedFilesHandler](handler)

  transparent inline def renamingFiles(handler: RenamingFilesHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.renamingFiles0 = LspRegistry.Slot[RenamingFilesHandler](handler)

  transparent inline def renamedFiles(handler: RenamedFilesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.renamedFiles0 = LspRegistry.Slot[RenamedFilesHandler](handler)

  transparent inline def deletingFiles(handler: DeletingFilesHandler)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.deletingFiles0 = LspRegistry.Slot[DeletingFilesHandler](handler)

  transparent inline def deletedFiles(handler: DeletedFilesHandler)(using registry: LspRegistry^)
  :   Unit =

    registry.deletedFiles0 = LspRegistry.Slot[DeletedFilesHandler](handler)

  transparent inline def resolveCompletion(handler: ResolveHandler[CompletionItem])
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.resolveCompletion0 = LspRegistry.Slot[ResolveHandler[CompletionItem]](handler)

  transparent inline def resolveCodeAction(handler: ResolveHandler[CodeAction])
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.resolveCodeAction0 = LspRegistry.Slot[ResolveHandler[CodeAction]](handler)

  transparent inline def resolveCodeLens(handler: ResolveHandler[CodeLens])
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.resolveCodeLens0 = LspRegistry.Slot[ResolveHandler[CodeLens]](handler)

  transparent inline def resolveDocumentLink(handler: ResolveHandler[DocumentLink])
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.resolveDocumentLink0 = LspRegistry.Slot[ResolveHandler[DocumentLink]](handler)

  transparent inline def resolveInlayHint(handler: ResolveHandler[InlayHint])
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.resolveInlayHint0 = LspRegistry.Slot[ResolveHandler[InlayHint]](handler)

  transparent inline def resolveWorkspaceSymbol(handler: ResolveHandler[WorkspaceSymbol])
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.resolveWorkspaceSymbol0 = LspRegistry.Slot[ResolveHandler[WorkspaceSymbol]](handler)

  // An escape hatch: adjusts the derived capabilities, applied last, at initialization.
  transparent inline def capabilities(adjust: ServerCapabilities => ServerCapabilities)
    ( using registry: LspRegistry^ )
  :   Unit =

    registry.adjust0 = adjust

  // The JSON-RPC dispatcher for an arbitrary implementation of the `Lsp` trait: the low-level
  // escape hatch, and the routing used by `listen`.
  def dispatcher(server: Lsp): Json => Optional[Json] = LspDispatch(server)

  private[exegesis] def requestId(json: Json): Optional[Json] =
    // Throwing rather than `safely`: the request decodable cannot thread the boundary tactic
    // under separation checking, and an unreadable id is simply absent.
    import strategies.throwUnsafely
    try json.as[JsonRpc.Request].id catch case _: Exception => Unset

  // An observer of the raw traffic crossing the transport, for a server that exposes a log of the
  // messages it exchanges. Each message is reported as the text that was read from — or framed
  // onto — the wire, before parsing, so a malformed message is observed too. Both methods are
  // abstract: a concrete default would oblige every implementation to write `override`, which
  // costs more than the one-line no-op it saves.
  object Observer:
    // The default: a server that does not expose its traffic pays nothing for the hook.
    object Silent extends Observer:
      def received(message: Text): Unit = ()
      def sent(message: Text): Unit = ()

  trait Observer:
    def received(message: Text): Unit
    def sent(message: Text): Unit

  // Establishes a Language Server over the stdio transport. The block registers the server's
  // feature handlers on the lent registry; once it returns, the registry is consumed and frozen,
  // the server's capabilities are derived from what was registered, and the loop serves
  // `Content-Length`-framed JSON-RPC messages from standard input until it is exhausted. A single
  // asynchronous writer drains the outgoing channel (request responses and server-initiated
  // notifications alike) and frames each message onto standard output, so writes never
  // interleave. Everything stateful — registry, session, dispatcher — is local to this call:
  // nothing capability-carrying is ever stored in an application-lifetime object. `observer`, if
  // given, sees every message in both directions as it crosses the transport.
  def listen(name: Text, version: Optional[Text] = Unset, observer: Observer^ = Observer.Silent)
    ( register: (registry: LspRegistry^) ?=> Unit )
    ( using Stdio^, Monitor, Probate )
  :   Unit =

    import charEncoders.utf8Encoder
    import strategies.throwUnsafely
    import Json.jsonEncodableInText

    val registry: LspRegistry^ = LspRegistry()
    register(using registry)

    val session: LspSession^ = LspSession(registry, name, version)

    // The session is confined by its own type and the dispatch closures are locals of this
    // method, so sealing the reference the generated dispatchers hold is sound; the macro cannot
    // take a capability-typed splice.
    val dispatch: Json => Optional[Json] = LspDispatch(caps.unsafe.unsafeAssumePure(session))

    // The writer drains the channel and frames each message onto stdout. The observer sees the
    // encoded body, not the framing, so both directions read alike in a log.
    val writer: Task[Unit] = async:
      session.outgoing.stdlib.iterator.each: json =>
        val body: Text = json.encode
        observer.sent(body)
        val payload: Data = body.in[Data]
        summon[Stdio].write(t"Content-Length: ${payload.length}\r\n\r\n".in[Data])
        summon[Stdio].write(payload)
        summon[Stdio].out.flush()

    summon[Stdio].in.source[Data].toProgression.stdlib.iterator.frames[ContentLength].each:
      frame =>
        // Observed before parsing, so a message that fails to decode is logged as it arrived.
        val message: Text = frame.utf8
        observer.received(message)

        safely(message.as[Json]).lay(session.put(JsonRpc.failure(-32700, t"Parse error"))):
          json =>
            val response: Optional[Json] =
              try dispatch(json) catch
                case error: JsonError =>
                  JsonRpc.failure(-32602, t"Invalid params", requestId(json))

                case error: Exception =>
                  JsonRpc.failure(-32603, t"Internal error", requestId(json))

            session.conclude(json, response).let(session.put)

    writer.cancel()

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

trait LspWorkspace extends JsonRpc:

  @rpc
  def `workspace/symbol`(query: Text): List[Lsp.WorkspaceSymbol]

  @rpc
  def `workspace/executeCommand`(command: Text, arguments: Optional[List[Json]]): Optional[Json]

  @rpc
  def `workspace/didChangeConfiguration`(settings: Json): Unit

  @rpc
  def `workspace/didChangeWatchedFiles`(changes: List[Lsp.FileEvent]): Unit

  @rpc
  def `workspace/didChangeWorkspaceFolders`(event: Lsp.WorkspaceFoldersChangeEvent): Unit

  @rpc
  def `workspace/willCreateFiles`(files: List[Lsp.FileCreate]): Optional[Lsp.WorkspaceEdit]

  @rpc
  def `workspace/didCreateFiles`(files: List[Lsp.FileCreate]): Unit

  @rpc
  def `workspace/willRenameFiles`(files: List[Lsp.FileRename]): Optional[Lsp.WorkspaceEdit]

  @rpc
  def `workspace/didRenameFiles`(files: List[Lsp.FileRename]): Unit

  @rpc
  def `workspace/willDeleteFiles`(files: List[Lsp.FileDelete]): Optional[Lsp.WorkspaceEdit]

  @rpc
  def `workspace/didDeleteFiles`(files: List[Lsp.FileDelete]): Unit

  @rpc
  def `$/setTrace`(value: Text): Unit

// The `*/resolve` requests: the client sends back an item it received earlier for the server to
// fill in lazily-computed fields. Their wire `params` is the bare item, not a `params` object with
// named fields, so each parameter is marked `@bare` (see `obligatory.bare`).
trait LspResolve extends JsonRpc:

  @rpc
  def `completionItem/resolve`(@bare item: Lsp.CompletionItem): Lsp.CompletionItem

  @rpc
  def `codeAction/resolve`(@bare codeAction: Lsp.CodeAction): Lsp.CodeAction

  @rpc
  def `codeLens/resolve`(@bare codeLens: Lsp.CodeLens): Lsp.CodeLens

  @rpc
  def `documentLink/resolve`(@bare documentLink: Lsp.DocumentLink): Lsp.DocumentLink

  @rpc
  def `inlayHint/resolve`(@bare inlayHint: Lsp.InlayHint): Lsp.InlayHint

  @rpc
  def `workspaceSymbol/resolve`(@bare workspaceSymbol: Lsp.WorkspaceSymbol): Lsp.WorkspaceSymbol

// The full protocol: the union of every sub-interface, fixing `Origin` to `LspClient`. `LspServer`
// implements this; `LspServer.dispatcher` serves each sub-interface separately, routing by method.
trait Lsp
extends LspLifecycle, LspLanguage, LspNavigation, LspEditing, LspAdvanced, LspWorkspace, LspResolve:
  type Origin = LspClient
