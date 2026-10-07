package exegesis

import soundness.*

import codepages.utf8Codepage
import strategies.throwUnsafely

// ── capability-escape (a GENUINE capture, not a spurious box) ─────────────────────────────────────
// Unlike case-2 the capture here is real. `JsonRpc.serve[Lsp.Client](inbound)` inlines a JSON codec
// for every RPC method; those codecs summon a `Tactic[Json.Error]`. Storing the result in a
// `lazy val` FIELD makes the enclosing object close over that tactic, so its type is
//   () ?->{given_Tactic_JsonError} Morphology
// and CC demands: "object TestServer needs to extend Capability since it has a field `dispatch` with
// `any` in its type."
// WHAT WE WANT (a design decision, not a compiler ask): `dispatch` should not RETAIN the error
// capability — `provide` the tactic inside, or make `dispatch` a `def` taking the tactic.
// RESOLVED at source: the production sites (`Lsp.listen`, `LspSessional`) keep the dispatcher a
// LOCAL of the serving method, lent to the reader and writer and dead when they return. This
// repro therefore stays RED by design. On 3.9.1-dev-p17 it ALSO shows the exclusive-tactic
// overlap (rep/sepcheck-probes/p15) inside the macro's codec summons — "Separation failure:
// argument … hides capabilities {given_Tactic_Error} … overlap with the captures of the second
// argument" — which is why the production sites wrap `JsonRpc.serve` in `unsafeAssumeSeparate`.
// That second error is the Soundness-backed gate for safety-8: it must disappear when `Emit`
// becomes a `caps.SharedCapability`.
// Compile with `rep/compile.sh capability-escape` (dotc). WHERE (1 suite): exegesis.

object TestServer:
  val inbound: Lsp.Client = LspInbound(new Lsp.Listener {})

  // Expanding the `serve` dispatch macro into a `lazy val` field is what leaks the Tactic capability.
  lazy val dispatch: Json => Optional[Json] = JsonRpc.serve[Lsp.Client](inbound)
