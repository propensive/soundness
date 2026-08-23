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
package murmuration

import prepositional.*

// The keyed-container surface: `keys` and `values`, better-typed than the stdlib's `Iterable`
// results — a `Map`'s keys are a `Set` (they are unique and unordered), a `Ledger`'s are a
// `List` (insertion order is its identity), and both containers' values are a `List`.
object Keyed:
  // The refinements appear in the declared types (not just the instance bodies) so a summon
  // with a `Keys`/`Values` refinement can solve them — the `Mappable` pattern.
  given map: [key, value]
  =>  (Map[key, value] is Keyed { type Keys = Set[key]; type Values = List[value] }) =
    new Keyed:
      type Self = Map[key, value]
      type Keys = Set[key]
      type Values = List[value]

      def keys(map: Self): Keys = Set.of(map.stdlib.keySet)
      def values(map: Self): Values = List.from(map.stdlib.values.iterator)

  given ledger: [key, value]
  =>  (Ledger[key, value] is Keyed { type Keys = List[key]; type Values = List[value] }) =
    new Keyed:
      type Self = Ledger[key, value]
      type Keys = List[key]
      type Values = List[value]

      def keys(ledger: Self): Keys = List.from(ledger.stdlib.keys.iterator)
      def values(ledger: Self): Values = List.from(ledger.stdlib.values.iterator)

trait Keyed extends Typeclass.Pure:
  type Keys
  type Values

  def keys(self: Self): Keys
  def values(self: Self): Values

// The result types are bound as extension type parameters rather than referenced
// path-dependently, so they survive the cross-package export forwarder (#1411).
extension [self, keys](self: self)(using keyed: self is Keyed { type Keys = keys })
  def keys: keys = keyed.keys(self)

extension [self, values](self: self)(using keyed: self is Keyed { type Values = values })
  def values: values = keyed.values(self)
