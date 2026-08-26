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

// Proscenium's collections are opaque types, and an opaque type stays transparent throughout the
// scope that defines it — including nested packages. A suite in `package proscenium` (or in a
// `proscenium.*` subpackage) therefore sees `List[Int]` as `scala.collection.immutable.List[Int]`
// and tests the stdlib rather than Proscenium. This suite lives outside that scope entirely, so
// the abstraction holds and it sees what users see: the `stdlib` bridge, `.to(…)` construction,
// the `Populated` gating, and the absence of any implicit conversion to a stdlib type.
package prosceniumtest

import soundness.*

// By name, not through `soundness.*`: the umbrella's export forwarders do not carry the opaque
// types' companions into implicit scope, so `.stdlib` and the other companion extensions do not
// resolve through them. Every library in the repo reaches these types through
// `-Yimports:…,proscenium` instead and so never meets this; a downstream user importing only
// `soundness.*` would.
import proscenium.{List, Set, Map, Sequence, Ledger, Array}
import rudiments.occupied

object Tests extends Suite(m"Proscenium consumer-view tests"):
  def run(): Unit =
    suite(m"The stdlib bridge"):
      test(m"a list crosses to the stdlib"):
        List(1, 2, 3).stdlib
      . assert(_ == scala.collection.immutable.List(1, 2, 3))

      test(m"a set crosses to the stdlib"):
        Set(1, 2, 2).stdlib
      . assert(_ == scala.collection.immutable.Set(1, 2))

      test(m"a map crosses to the stdlib"):
        Map(t"a" -> 1).stdlib
      . assert(_ == scala.collection.immutable.Map(t"a" -> 1))

      test(m"a sequence crosses to the stdlib"):
        Sequence(1, 2, 3).stdlib
      . assert(_ == scala.collection.immutable.Vector(1, 2, 3))

      test(m"a ledger crosses to the stdlib keeping its order"):
        Ledger(t"b" -> 2, t"a" -> 1).stdlib.keys.toList
      . assert(_ == scala.collection.immutable.List(t"b", t"a"))

      test(m"a frozen array crosses to the stdlib as an IArray"):
        Array(1, 2, 3).readable.toList
      . assert(_ == scala.collection.immutable.List(1, 2, 3))

    suite(m"Building the opaque types from stdlib collections"):
      test(m"a stdlib collection builds a list"):
        val list: List[Int] = scala.collection.immutable.List(1, 2).to(List)
        list.stdlib
      . assert(_ == scala.collection.immutable.List(1, 2))

      test(m"a stdlib collection builds a set"):
        val set: Set[Int] = scala.collection.immutable.List(1, 1, 2).to(Set)
        set.stdlib
      . assert(_ == scala.collection.immutable.Set(1, 2))

      test(m"a stdlib collection builds a ledger in insertion order"):
        val ledger: Ledger[Int, Text] =
          scala.collection.immutable.List((2, t"b"), (1, t"a")).to(Ledger)

        ledger.stdlib.keys.toList
      . assert(_ == scala.collection.immutable.List(2, 1))

      // Aspirational: `List`, `Set`, `Map` and `Ledger` each provide a `factory` conversion so
      // that `.to(…)` builds them, but `Sequence` does not, so it is the one opaque collection
      // that cannot be built this way. The omission looks accidental rather than intended —
      // `Sequence`'s only deliberate omission is documented as the conversion in the *other*
      // direction.
      test(m"a stdlib collection builds a sequence"):
        demilitarize:
          val sequence: Sequence[Int] = scala.collection.immutable.List(1, 2).to(Sequence)
        . map(_.message)
      . aspire(_.isEmpty)

    suite(m"Non-emptiness proofs"):
      // From outside the package these are the operations that matter: `head` is total only on
      // a `Populated` receiver, and the proof comes either from a literal constructor's arity
      // or from `occupied`'s single check.
      test(m"a literal list is branded as populated"):
        demilitarize:
          val list: List[Int] & Populated = List(1, 2, 3)
      . assert(_ == Nil)

      test(m"a varargs list is not branded, since its arity is not statically known"):
        demilitarize:
          val elements = scala.Seq(1, 2, 3)
          val list: List[Int] & Populated = List(elements*)
      . assert(_.nonEmpty)

      test(m"a literal sequence is branded as populated"):
        demilitarize:
          val sequence: Sequence[Int] & Populated = Sequence(1, 2, 3)
      . assert(_ == Nil)

      test(m"head reads the first element of a branded list"):
        List(4, 5, 6).head
      . assert(_ == 4)

      test(m"occupied mints a proof for a non-empty list"):
        List(7, 8).occupied.let(_.head)
      . assert(_ == 7)

      test(m"occupied yields Unset for an empty list"):
        List.empty[Int].occupied
      . assert(_ == Unset)

      test(m"head is unavailable without a proof"):
        demilitarize:
          val elements = scala.Seq(1, 2, 3)
          List(elements*).head
      . assert(_.nonEmpty)

      test(m"head is available once occupied has proven non-emptiness"):
        demilitarize:
          val elements = scala.Seq(1, 2, 3)
          List(elements*).occupied.let(_.head)
      . assert(_ == Nil)

    suite(m"Opacity"):
      // The point of the opaque alias: a Proscenium collection is not a stdlib one, and there is
      // deliberately no implicit conversion either way.
      test(m"a list is not a stdlib list"):
        demilitarize:
          val list: scala.collection.immutable.List[Int] = List(1, 2, 3)
      . assert(_.nonEmpty)

      test(m"a stdlib list is not a list"):
        demilitarize:
          val list: List[Int] = scala.collection.immutable.List(1, 2, 3)
      . assert(_.nonEmpty)

      test(m"a ledger is not a map"):
        demilitarize:
          val map: Map[Int, Int] = Ledger(1 -> 1)
      . assert(_.nonEmpty)
