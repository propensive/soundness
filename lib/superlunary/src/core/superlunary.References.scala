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
package superlunary

import scala.quoted.*

import anticipation.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*
import denominative.*
import denominative.dysasymptotics.linearSize

object References:
  def apply[transport <: Object](): References over transport = new References:
    type Transport = transport

  // A memoized extraction, once per slot per JVM: the box replaces the raw transported
  // value in its array slot on first use (the `value` field is final, so racing threads
  // publish it safely and at worst extract twice). Without this, a `$value` splice sitting
  // inside a hot loop — a benchmark body, for instance — would deserialize the transported
  // value on every iteration. The extracted instance is consequently SHARED between
  // evaluations, which is the intuitive semantics of splicing a value. The memoization is
  // generated inline by `embeddings.automatic` rather than through a helper method: the
  // extraction is an inline call with deferred given summons, which must stay in statement
  // position in the staged program, not under a closure.
  class Boxed(val value: Any)

abstract class References():
  type Transport <: Object

  private var ref: Optional[Expr[scala.Array[Object]]] = Unset
  private var allocations: List[Transport] = Nil

  def update(expr: Expr[scala.Array[Object]]): Unit = ref = expr

  // A protocol invariant of the rig, not a local check: `references() = array` is always
  // assigned inside the staged lambda before any `$value` conversion splices `array`. The
  // binding happens in quote scope, so it cannot be a constructor argument without
  // restructuring the staging protocol.
  def array: Expr[scala.Array[Object]] =
    ref.or(panic(m"the reference array is bound by the rig before any value is spliced"))
  def current: Int = allocations.size
  def allocate(value: => Transport): Int = allocations.size.also { allocations ::= value }
  inline def apply(): scala.Array[Object] = scala.Array.from[Object](allocations.reverse.stdlib)
