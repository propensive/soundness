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
import vacuous.*

import Lsp.*

object LspRegistry:
  // A context-function value adapts to any non-context-function expected type by being applied,
  // so a handler cannot inhabit an `Optional[...]` union directly: the box holds it intact.
  case class Slot[handler](value: handler)

// The target of the registration combinators, lent to the block given to `Lsp.listen` for its
// duration, then consumed by the session it configures: registration after the server begins
// serving is impossible by construction. An exclusive capability, so it cannot escape the block;
// its slots are public untracked vars — the combinators are inline, assigning from their
// expansion sites. The slots are an erased rim (`AnyRef | Null`): a union mentioning a
// context-function type freshens its capture sets at every adaptation, so the typed boundary is
// the combinator (whose parameter is the pure handler type — a handler closing over the registry
// is rejected there) and the session's invocation helpers, which restore the type by cast.
class LspRegistry private[exegesis] () extends caps.ExclusiveCapability:

  @scala.caps.unsafe.untrackedCaptures
  var ready0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var terminating0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var opened0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var changed0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var saved0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var closed0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var saving0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var savingEdits0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var hover0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var complete0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var completeTriggers0: List[Text] = Nil

  @scala.caps.unsafe.untrackedCaptures
  var definition0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var references0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var documentSymbols0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var format0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var rename0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var codeActions0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var signatureHelp0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var signatureHelpTriggers0: List[Text] = Nil

  @scala.caps.unsafe.untrackedCaptures
  var declaration0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var typeDefinition0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var implementation0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var documentHighlights0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var foldingRanges0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var selectionRanges0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var documentLinks0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var codeLenses0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var documentColors0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var colorPresentations0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var formatRange0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var formatOnType0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var formatOnTypeFirst0: Optional[Text] = Unset

  @scala.caps.unsafe.untrackedCaptures
  var formatOnTypeMore0: List[Text] = Nil

  @scala.caps.unsafe.untrackedCaptures
  var prepareRename0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var prepareCallHierarchy0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var incomingCalls0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var outgoingCalls0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var prepareTypeHierarchy0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var supertypes0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var subtypes0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var semanticTokens0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var semanticTokensLegend0: Optional[SemanticTokensLegend] = Unset

  @scala.caps.unsafe.untrackedCaptures
  var semanticTokensDelta0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var semanticTokensRange0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var inlayHints0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var inlineValues0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var linkedEditingRange0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var monikers0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var diagnostics0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var diagnosticOptions0: Optional[DiagnosticOptions] = Unset

  @scala.caps.unsafe.untrackedCaptures
  var workspaceSymbols0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var commands0: List[(Text, AnyRef)] = Nil

  // The registered command names, tracked apart from the handlers: the capability derivation
  // and lookup paths must not traverse a list whose element type mentions a context function.
  @scala.caps.unsafe.untrackedCaptures
  var commandNames0: List[Text] = Nil

  @scala.caps.unsafe.untrackedCaptures
  var configuration0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var watchedFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var foldersChanged0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var creatingFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var createdFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var renamingFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var renamedFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var deletingFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var deletedFiles0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var resolveCompletion0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var resolveCodeAction0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var resolveCodeLens0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var resolveDocumentLink0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var resolveInlayHint0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var resolveWorkspaceSymbol0: AnyRef | Null = null

  @scala.caps.unsafe.untrackedCaptures
  var adjust0: Optional[ServerCapabilities => ServerCapabilities] = Unset

  private def flag(registered: AnyRef | Null): Optional[Boolean] =
    if registered == null then Unset else true

  private def triggers(list: List[Text]): Optional[List[Text]] =
    if list.stdlib.isEmpty then Unset else list

  private def commandOptions: Optional[ExecuteCommandOptions] =
    if commandNames0.stdlib.isEmpty then Unset else ExecuteCommandOptions(commandNames0)

  private def adjusted(derived: ServerCapabilities): ServerCapabilities =
    adjust0.lay(derived)(_(derived))

  // The server capabilities implied by the registrations: each capability is advertised exactly
  // when its handler was registered, so the declaration can never disagree with the
  // implementation. Options a capability needs beyond mere presence were given at registration.
  // The `adjust` escape hatch, if registered, is applied last.
  def capabilities: ServerCapabilities = adjusted:
    ServerCapabilities
      ( textDocumentSync                 = TextDocumentSyncKind.Incremental,
        completionProvider               = flag(complete0).let: _ =>
                                             CompletionOptions
                                              ( triggerCharacters = triggers(completeTriggers0),
                                                resolveProvider = flag(resolveCompletion0) ),
        hoverProvider                    = flag(hover0),
        definitionProvider               = flag(definition0),
        referencesProvider               = flag(references0),
        documentSymbolProvider           = flag(documentSymbols0),
        documentFormattingProvider       = flag(format0),
        renameProvider                   = flag(rename0),
        codeActionProvider               = flag(codeActions0),
        signatureHelpProvider            = flag(signatureHelp0).let: _ =>
                                             SignatureHelpOptions(triggers(signatureHelpTriggers0)),
        declarationProvider              = flag(declaration0),
        typeDefinitionProvider          = flag(typeDefinition0),
        implementationProvider           = flag(implementation0),
        documentHighlightProvider        = flag(documentHighlights0),
        foldingRangeProvider             = flag(foldingRanges0),
        selectionRangeProvider           = flag(selectionRanges0),
        colorProvider                    = flag(documentColors0),
        documentRangeFormattingProvider  = flag(formatRange0),
        documentLinkProvider             = flag(documentLinks0).let: _ =>
                                             DocumentLinkOptions(flag(resolveDocumentLink0)),
        codeLensProvider                 = flag(codeLenses0).let: _ =>
                                             CodeLensOptions(flag(resolveCodeLens0)),
        documentOnTypeFormattingProvider = formatOnTypeFirst0.let: first =>
                                             DocumentOnTypeFormattingOptions
                                              ( first, triggers(formatOnTypeMore0) ),
        callHierarchyProvider            = flag(prepareCallHierarchy0),
        typeHierarchyProvider            = flag(prepareTypeHierarchy0),
        semanticTokensProvider           = semanticTokensLegend0.let: legend =>
                                             SemanticTokensOptions
                                              ( legend,
                                                range = flag(semanticTokensRange0),
                                                full  = flag(semanticTokens0) ),
        inlayHintProvider                = flag(inlayHints0),
        inlineValueProvider              = flag(inlineValues0),
        linkedEditingRangeProvider       = flag(linkedEditingRange0),
        monikerProvider                  = flag(monikers0),
        diagnosticProvider               = flag(diagnostics0).let(_ => diagnosticOptions0),
        workspaceSymbolProvider          = flag(workspaceSymbols0),
        executeCommandProvider           = commandOptions )
