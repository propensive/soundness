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
package rudiments

import anticipation.*
import prepositional.*

// Order-reversal, unified across shapes so a single `reverse` extension serves both collections
// (`List`/`Series`) and textual types (gossamer contributes a `Reversible` *instance* for `Textual`,
// not a competing extension — that is what keeps the name un-clashed at the `soundness` umbrella).
// Instances are subtype-parametric (`container <: List[element]`) to match `& Populated` receivers
// and the distinct `soundness.*` re-export aliases; `Result` is bound as an ordinary type parameter
// at the extension, never referenced path-dependently, so it survives the cross-package export
// forwarder (#1411). Unordered shapes (`Set`, `Map`) have no instance: reversing them is meaningless.
object Reversible:
  given list: [element, container <: List[element]]
  =>  (container is Reversible { type Result = List[element] }) =
    new Reversible:
      type Self = container
      type Result = List[element]
      def reverse(self: container): List[element] = List.of(self.stdlib.reverse)

  given series: [element, container <: Series[element]]
  =>  (container is Reversible { type Result = Series[element] }) =
    new Reversible:
      type Self = container
      type Result = Series[element]
      def reverse(self: container): Series[element] = Series.of(self.stdlib.reverse)

  // `Text`'s companion (in `anticipation`) cannot host this — it sits below both `Reversible` and
  // `Textual` — but `Reversible`'s own companion is in implicit scope for `Text is Reversible`, and
  // `anticipation` exposes the `.s` bridge. `StringBuilder#reverse` is surrogate-pair-aware.
  given text: [text <: Text] => (text is Reversible { type Result = Text }) =
    new Reversible:
      type Self = text
      type Result = Text
      def reverse(value: text): Text = StringBuilder(value.s).reverse.nn.toString.nn.tt

extension [self, result](value: self)(using reversible: self is Reversible { type Result = result })
  def reverse: result = reversible.reverse(value)

trait Reversible extends Typeclass.Pure, Resultant:
  def reverse(self: Self): Result
