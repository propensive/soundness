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
package dissonance

import scala.compiletime.*

import proscenium.compat.*

import vacuous.*

// A phantom marker recording a *proof of retention* in a value's type: producers which populate
// every edit's `value` by construction (the Myers walk, redraft analysis) mint `& Retained` at
// the construction site, and `kept` narrows `value` from `Optional[element]` to a bare `element`
// on any edit whose type carries the proof, so absence-handling is discharged at compile time:
//
//     statically-proven `Retained` edits access their values directly
//
// A *structural refinement*, not a trait: refinements are erasure-transparent, so
// `Edit[Text] & Retained` still erases to `Edit`. A nominal marker would erase intersections and
// `self <: Retained` bounds to the marker's own class, inserting checkcasts that fail at
// runtime — the underlying value never actually implements it. (The same technique as
// rudiments' `Populated`, the non-emptiness proof.)
type Retained = Any { type Retain = true }

extension [element](edit: Edit[element])
  // The `Retained` producer for a single edit: the cast *is* the mint, used only at a site where
  // presence is established by construction — an edit whose `value` was just supplied. (The same
  // discipline as `occupied`: one check, here construction itself, recorded in the type.)
  def retained: edit.type & Retained = edit.asInstanceOf[edit.type & Retained]

  // A single accessor that dispatches at compile time on the edit's type: an edit statically
  // known to be `Retained` returns a bare `element` with no absence-handling; any other edit
  // returns its `Optional` value unchanged. The declared return type is `Optional`, so
  // non-reducing (e.g. generic) call sites are safe; a `Retained` edit narrows to a bare
  // `element`.
  transparent inline def kept: Optional[element] = summonFrom:
    case _: (edit.type <:< Retained) => edit.value.asInstanceOf[element]
    case _                           => edit.value

extension [element](diff: Diff[element])
  // The `Retained` producer for a whole `Diff`, sanctioned only where every edit passed to the
  // constructor was itself `Retained` (the varargs field cannot carry the brand through, so it
  // is re-established on the assembled value).
  def retained: diff.type & Retained = diff.asInstanceOf[diff.type & Retained]

extension [element](diff: Diff[element] & Retained)
  // Transfers a `Diff`'s retention proof back to its edits: a `Diff & Retained` was assembled
  // exclusively from retained edits, so re-minting each one discharges no new obligation.
  def retentions: List[Edit[element] & Retained] =
    diff.edits.to(List).asInstanceOf[List[Edit[element] & Retained]]
