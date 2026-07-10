package austronesian

import soundness.*

// ── capturing-derivation ───────────────────────────────────────────────────────────────────────
// A Wisteria-derived `Decodable` legitimately captures the outer `Tactic[VariantError]` used to
// report a bad variant. Wisteria's derivation (`fieldInstance`'s `asExprOf[typeclass[e]]` cast and
// `derivedOne`'s `asMatchable.match { case tc => tc }`) launders then re-imposes the clean type,
// boxing the derived instance's type-argument, so `group.pojo.decode[Group]` cannot unify the
// derived `Decodable`'s capture with the expected one.
// Derived typeclasses are NOT always pure — `Decodable` reports errors, so it captures a `Tactic`;
// we cannot make it pure and cannot change the compiler.
// WHAT WE WANT: deriving + using a `Decodable` that captures a `Tactic` should be accepted.
// LIKELY FIX: self-`provide[Tactic[VariantError]]` at the capturing site (jacinta/stratiform pattern).
// Compile with `rep/compile.sh capturing-derivation` (dotc). WHERE (1 suite): austronesian.

case class Person(name: Text, age: Int)
case class Group(persons: List[Person], size: Int)

object CapturingDerivation:
  val group = Group(List(Person(t"John", 30), Person(t"Jane", 25)), 2)
  val roundtripped = unsafely(group.pojo.decode[Group])
