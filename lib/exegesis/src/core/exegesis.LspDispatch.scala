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
import contingency.*
import jacinta.*
import obligatory.*
import prepositional.*
import rudiments.*
import vacuous.*

// The JSON-RPC dispatch is generated split across one dispatcher per `Lsp` sub-interface, rather
// than as a single `serve[Lsp]`: `JsonRpc.serve` inlines a schema-carrying codec for every method
// it covers, so one dispatcher for the whole protocol would overflow the JVM per-class
// constant-pool limit. Each sub-dispatcher is expanded in its own object, so it compiles into its
// own small class; `apply` routes an incoming request to the one whose interface declares its
// method (a JSON-RPC response, which carries no method, is handled by any dispatcher, so the
// first suffices).
object LspDispatch:
  private object lifecycleRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspLifecycle](server))

  // Materialized here because the `JsonRpc.serve` macro's `Expr.summon` cannot
  // expand jacinta's inline `encodable` given for the opaque `List` alias.
  private given documentSymbolList: (List[Lsp.DocumentSymbol] is Encodable in Json) =
    scala.compiletime.summonInline[List[Lsp.DocumentSymbol] is Encodable in Json]

  private given selectionRangeList: (List[Lsp.SelectionRange] is Encodable in Json) =
    scala.compiletime.summonInline[List[Lsp.SelectionRange] is Encodable in Json]

  private object languageRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspLanguage](server))

  private object navigationRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspNavigation](server))

  private object editingRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspEditing](server))

  private object advancedRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspAdvanced](server))

  private object workspaceRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspWorkspace](server))

  private object resolveRoute:
    def apply(server: Lsp): Json => Optional[Json] =
      import strategies.throwUnsafely
      scala.caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspResolve](server))

  def apply(server: Lsp): Json => Optional[Json] =
    import dynamicJsonAccess.enabled
    import strategies.throwUnsafely

    // Each expansion is bound to a bare local, and the routing is an if-chain over locals
    // rather than a list of tuples: an expected type at the expansion site freshens the opaque
    // `Text` inside the result, and function values in an invariant container hit boxed-capture
    // mismatches under capture checking.
    val lifecycleMethods = JsonRpc.methods[LspLifecycle]
    val languageMethods = JsonRpc.methods[LspLanguage]
    val navigationMethods = JsonRpc.methods[LspNavigation]
    val editingMethods = JsonRpc.methods[LspEditing]
    val advancedMethods = JsonRpc.methods[LspAdvanced]
    val workspaceMethods = JsonRpc.methods[LspWorkspace]
    val resolveMethods = JsonRpc.methods[LspResolve]

    val lifecycle = lifecycleRoute(server)
    val language = languageRoute(server)
    val navigation = navigationRoute(server)
    val editing = editingRoute(server)
    val advanced = advancedRoute(server)
    val workspace = workspaceRoute(server)
    val resolve = resolveRoute(server)

    json =>
      val method: Optional[Text] = try json.method.as[Text] catch case _: Exception => Unset

      // A message with no method is a response, which any dispatcher handles; an unknown
      // method is dropped.
      method.lay(lifecycle(json)): method =>
        if lifecycleMethods.has(method) then lifecycle(json)
        else if languageMethods.has(method) then language(json)
        else if navigationMethods.has(method) then navigation(json)
        else if editingMethods.has(method) then editing(json)
        else if advancedMethods.has(method) then advanced(json)
        else if workspaceMethods.has(method) then workspace(json)
        else if resolveMethods.has(method) then resolve(json)
        else Unset
