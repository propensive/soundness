//> using scala 3.8.4
//> using options -language:experimental.captureChecking -Ycc-new -language:experimental.modularity -source future

// ── case-2 "direct-mint" — SELF-CONTAINED (no Soundness dependency) ───────────────────────────────
// Compile with:  scala-cli compile rep/case2-directmint
//
// A value of a PURE type acquires a spurious empty capture set `^{}` when it flows out of an INLINE
// given of a `Self`-typeclass instantiated at that pure type, via an INLINE extension. The compiler
// rejects the box: "Text is a pure type, it makes no sense to add a capture set to it".
//
// Reduced from chiaroscuro's `t"hello".decompose`. The essential ingredients (each verified load-
// bearing by bisection):
//   (1) a pure OPAQUE type over a real type (`opaque type <: … & caps.Pure`). A plain
//       `class Text extends caps.Pure` does NOT trigger it.
//   (2) a `Self`-based typeclass whose method returns the pure type.
//   (3) an INLINE given materialising `entity is Decomposable` for any entity.
//   (4) an INLINE extension that summons (3) and calls it. A direct `summon[…].decomposition(…)`
//       (no inline extension) does NOT trigger it.
//
// WHAT WE WANT: `Text("hello").decompose` should type-check; `Text` is pure, nothing captures a
// capability, so no capture set should be added.
// WHERE IN SOUNDNESS (≈11 suites): chiaroscuro (`.decompose`), spectacular, anamnesis, profanity,
// telekinesis, scintillate, exoskeleton, apoplexy, ethereal, breviloquence, distillate.

import language.experimental.captureChecking
import scala.caps

opaque type Text <: Matchable & caps.Pure = String & caps.Pure
object Text:
  def apply(s: String): Text = s.asInstanceOf[Text]

trait Decomposable extends caps.Pure:
  type Self
  def decomposition(value: Self): Text

inline given derived: [entity] => entity is Decomposable = new Decomposable:
  type Self = entity
  def decomposition(value: entity): Text = Text("x")

extension [left](left: left)
  inline def decompose(using value: left is Decomposable): Text = value.decomposition(left)

val d: Text = Text("hello").decompose
