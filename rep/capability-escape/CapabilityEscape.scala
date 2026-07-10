package exegesis

import soundness.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely

// ── capability-escape (a GENUINE capture, not a spurious box) ─────────────────────────────────────
// Unlike case-2 the capture here is real. `JsonRpc.serve[Lsp](this)` inlines a JSON codec for every
// LSP method; those codecs summon a `Tactic[JsonError]`. Storing the result in a `lazy val` FIELD
// makes the enclosing object close over that tactic, so its type is
//   () ?->{given_Tactic_JsonError} Morphology
// and CC demands: "object TestServer needs to extend Capability since it has a field `dispatch` with
// `any` in its type."
// WHAT WE WANT (a design decision, not a compiler ask): `dispatch` should not RETAIN the error
// capability — `provide` the tactic inside, or make `dispatch` a `def` taking the tactic.
// Compile with `rep/compile.sh capability-escape` (dotc). WHERE (1 suite): exegesis.

object TestServer extends LspServer():
  def name: Text = t"Test"
  override def version: Optional[Text] = t"1.0"
  def capabilities: Lsp.ServerCapabilities = Lsp.ServerCapabilities(hoverProvider = true)

  override def hover(uri: Text, position: Lsp.Position): Optional[Lsp.Hover] =
    Lsp.Hover(Lsp.MarkupContent(value = t"hi"))

  // Expanding the `serve` dispatch macro into a `lazy val` field is what leaks the Tactic capability.
  lazy val dispatch: Json => Optional[Json] = JsonRpc.serve[Lsp](this)
