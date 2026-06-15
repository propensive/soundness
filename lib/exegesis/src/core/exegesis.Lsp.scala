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
import prepositional.*
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

  case class ServerCapabilities
    ( textDocumentSync:           Optional[TextDocumentSyncKind] = Unset,
      completionProvider:         Optional[CompletionOptions]    = Unset,
      hoverProvider:              Optional[Boolean]              = Unset,
      definitionProvider:         Optional[Boolean]              = Unset,
      referencesProvider:         Optional[Boolean]              = Unset,
      documentSymbolProvider:     Optional[Boolean]              = Unset,
      documentFormattingProvider: Optional[Boolean]              = Unset,
      renameProvider:             Optional[Boolean]              = Unset,
      codeActionProvider:         Optional[Boolean]              = Unset,
      signatureHelpProvider:      Optional[SignatureHelpOptions] = Unset )

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

  // Diagnostics and window messages

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

trait Lsp extends JsonRpc:
  type Origin = LspClient

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
