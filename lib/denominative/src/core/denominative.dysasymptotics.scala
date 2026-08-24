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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package denominative

// Gates for dysasymptotic operations. Each is a plain marker type — *not* a capability —
// required in a `using` position by the instance that provides the expensive operation. A
// downstream file opts into a whole class of such operations by importing the matching enabler
// from the `dysasymptotics` package, e.g. `import dysasymptotics.linearSize`, which leaves every
// acknowledgement greppable. Because they carry no authority, they need none of the
// `erased`/scope-function machinery a capability would; a harmless runtime residue is fine.
//
// An operation's *semantic scope* is the portion of the data involved that the operation's
// specification identifies as its subject: the data whose values must be observed, compared,
// transformed, replaced, or produced in order to yield the specified result. Data that is merely
// incorporated into the result unchanged, or traversed only to reach the subject, lies outside
// the semantic scope.
//
// An operation on a particular data structure is *dysasymptotic* when its computational cost
// grows more than logarithmically with some measure of the data outside its semantic scope —
// typically the size of the containing structure, or the distance of the subject from the
// structure's point of access — while the scope itself remains bounded with respect to that
// measure. Dysasymptoticity is a property of the operation on that *representation*, not of the
// operation in the abstract — `size` is free on `Sequence` but dysasymptotic on `List` — which
// is why the gates sit on typeclass instances rather than on methods. Cost that merely tracks
// the semantic scope is never gated, however large the scope grows: `fold` over an infinite
// `Chain` diverges semantically, not dysasymptotically, whereas `Chain.size` (bounded scope,
// unbounded cost) is dysasymptotic. (A few whole-scope operations on `List`, such as `iterate`
// and `retrace`, are gated only because they route through `Countable`, whose `size` is
// dysasymptotic there — a granularity artefact, not policy.)
object Dysasymptotic:
  // Computing the length of a strict linked structure — `List.size`, and the size-derived
  // ordinal operations `gamut`/`limit`/`ult`/`pen`/`ant` — or rebuilding a structure wholesale
  // for a bounded-scope change — `:+` on a `List`, and `:+`/`+:`/`lead`/`define` on a frozen
  // array: O(n) in the data outside the scope.
  sealed trait LinearSize

  // Positional access into a strict linked structure — indexing a `List` via `at` — a walk
  // whose cost is the distance of the subject from the head. `prim`/`sec`/`ter` stay free:
  // their distance is bounded.
  sealed trait LinearAccess

  // Computing the size of a lazy structure — `Chain.size` — which forces the whole stream and
  // diverges on an infinite one: unbounded rather than merely linear.
  sealed trait UnboundedSize

package dysasymptotics:
  given linearSize: Dysasymptotic.LinearSize = new Dysasymptotic.LinearSize {}
  given linearAccess: Dysasymptotic.LinearAccess = new Dysasymptotic.LinearAccess {}
  given unboundedSize: Dysasymptotic.UnboundedSize = new Dysasymptotic.UnboundedSize {}
