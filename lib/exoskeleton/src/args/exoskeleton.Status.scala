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
package exoskeleton

import anticipation.*
import prepositional.*
import rudiments.*

// A meaningful exit status, declared alongside the `Flag`s and `Subcommand`s it accompanies,
// as an object:
//
//     object CannotConnect extends Status(1, t"the server could not be reached")
//
// Returning one from an `execute` block both sets the process's exit code and makes the status
// discoverable: each `Status.exit` demands a `Registry` for its own singleton type, so the
// block's `execute` accumulates the union of every status it can return, and documents them
// without the application having to list them twice.
//
// A status must be an object rather than a `val`, because capture checking currently rejects
// `value.type <: value.type | other.type` for the singleton type of a `val` (soundness#1811),
// which would stop that union from forming. Module singletons are unaffected.
object Status:
  // Contravariant, so that each `Registry[status.type]` demanded within a block adds a lower
  // bound to the block's `result` type, which then instantiates to the union of them all —
  // the same mechanism by which `Tactic[-error]` accumulates a `raises` union. The `Precise`
  // context bound on `execute` is what stops that union being widened to `Status`.
  trait Registry[-status]

  object Admissible:
    // Union types cannot be decomposed by recursive given resolution (the compiler will not
    // unify `a | b` with a concrete union, and instantiates both to `Nothing`), so the members
    // are read off the type directly. The type already carries the answer; this only reifies
    // it, and never inspects the block's code.
    transparent inline given derived: [result] => result is Admissible =
      ${exoskeleton.internal.admissible[result]}

  trait Admissible:
    type Self
    def statuses: List[Status]

// `caps.Pure`: a status is a plain datum which may never hold a live capability, and so its
// singleton type carries no capture set to obstruct the union.
open class Status(val code: Int, val description: Text) extends scala.caps.Pure:
  def exit(using Status.Registry[this.type]): Exit = Exit(code)
