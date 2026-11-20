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

object Tests extends Suite("prelude-consumer", m"Proscenium consumer-view tests"):
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
        Ledger(t"b" -> 2, t"a" -> 1).keys.to[List]
      . assert(_ == List(t"b", t"a"))

      test(m"a frozen array crosses to the stdlib as an ArraySeq"):
        Array(1, 2, 3).to[List]
      . assert(_ == List(1, 2, 3))

    suite(m"Building the opaque types from stdlib collections"):
      test(m"a stdlib collection builds a list"):
        val list: List[Int] = List.from(scala.collection.immutable.List(1, 2))
        list.stdlib
      . assert(_ == scala.collection.immutable.List(1, 2))

      test(m"a stdlib collection builds a set"):
        val set: Set[Int] = Set.from(scala.collection.immutable.List(1, 1, 2))
        set.stdlib
      . assert(_ == scala.collection.immutable.Set(1, 2))

      test(m"a stdlib collection builds a ledger in insertion order"):
        val ledger: Ledger[Int, Text] =
          scala.collection.immutable.List((2, t"b"), (1, t"a")).to(Ledger)

        ledger.keys.to[List]
      . assert(_ == List(2, 1))

      // Aspirational: `List`, `Set`, `Map` and `Ledger` each provide a `factory` conversion so
      // that `.to(…)` builds them, but `Sequence` does not, so it is the one opaque collection
      // that cannot be built this way. The omission looks accidental rather than intended —
      // `Sequence`'s only deliberate omission is documented as the conversion in the *other*
      // direction.
      aspirationally:
        test(m"a stdlib collection builds a sequence"):
          demilitarize:
            val sequence: Sequence[Int] = scala.collection.immutable.List(1, 2).to[Sequence]
          . map(_.message)
        . assert(_.nil)

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
      . assert(!_.nil)

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

      test(m"head without a proof is Optional"):
        val elements = scala.Seq(1, 2, 3)
        List(elements*).head
      . assert(_ == 1)

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
      . assert(!_.nil)

      test(m"a stdlib list is not a list"):
        demilitarize:
          val list: List[Int] = scala.collection.immutable.List(1, 2, 3)
      . assert(!_.nil)

      test(m"a ledger is not a map"):
        demilitarize:
          val map: Map[Int, Int] = Ledger(1 -> 1)
      . assert(!_.nil)

    // `Map.build` lends a mutable builder to a block and hands back an ordinary `Map`. Six
    // entries or so matter here: a build of five or more is a flat hash table, and four or
    // fewer the standard library's small maps, and the two must be indistinguishable.
    suite(m"Building a map in place"):
      def cast: Map[Text, Int] = Map.build[Text, Int]: map =>
        map(t"Hamlet") = 30
        map(t"Ophelia") = 18
        map(t"Laertes") = 25
        map(t"Gertrude") = 50
        map(t"Claudius") = 55
        map(t"Horatio") = 30

      val literal: Map[Text, Int] =
        Map
          ( t"Hamlet" -> 30, t"Ophelia" -> 18, t"Laertes" -> 25, t"Gertrude" -> 50,
            t"Claudius" -> 55, t"Horatio" -> 30 )

      test(m"a built map equals the literal with the same entries"):
        cast == literal
      . assert(_ == true)

      test(m"the literal equals the built map too"):
        literal == cast
      . assert(_ == true)

      test(m"a built map hashes as the literal does"):
        cast.hashCode
      . assert(_ == literal.hashCode)

      test(m"a built map reads a key that was written"):
        cast.at(t"Laertes")
      . assert(_ == 25)

      test(m"a built map has no value for a key that was not written"):
        cast.at(t"Yorick")
      . assert(_ == Unset)

      test(m"a built map counts its entries"):
        cast.size
      . assert(_ == 6)

      test(m"a built map yields every key"):
        cast.keys
      . assert(_ == literal.keys)

      test(m"a later write to the same key replaces the earlier one"):
        Map.build[Text, Int]: map =>
          map(t"Hamlet") = 29
          map(t"Ophelia") = 18
          map(t"Laertes") = 25
          map(t"Gertrude") = 50
          map(t"Claudius") = 55
          map(t"Hamlet") = 30
      . assert(_ == Map(t"Hamlet" -> 30, t"Ophelia" -> 18, t"Laertes" -> 25, t"Gertrude" -> 50,
          t"Claudius" -> 55))

      test(m"the builder answers what it holds while it is being filled"):
        var seen: (Boolean, Boolean, Int) = (false, false, 0)

        Map.build[Text, Int]: map =>
          map(t"Hamlet") = 30
          seen = (map.defines(t"Hamlet"), map.defines(t"Ophelia"), map.size)

        seen
      . assert(_ == (true, false, 1))

      test(m"an empty build is the empty map"):
        Map.build[Text, Int] { map => () }
      . assert(_ == Map.empty[Text, Int])

      test(m"a build of one entry equals the literal"):
        Map.build[Text, Int] { map => map(t"Hamlet") = 30 }
      . assert(_ == Map(t"Hamlet" -> 30))

      test(m"a build of four entries equals the literal"):
        Map.build[Int, Int] { map => List(1, 2, 3, 4).each { index => map(index) = index*index } }
      . assert(_ == Map(1 -> 1, 2 -> 4, 3 -> 9, 4 -> 16))

      test(m"a build of five entries equals the literal"):
        Map.build[Int, Int]: map =>
          List(1, 2, 3, 4, 5).each { index => map(index) = index*index }
      . assert(_ == Map(1 -> 1, 2 -> 4, 3 -> 9, 4 -> 16, 5 -> 25))

      test(m"a large build keeps every entry through its growth"):
        val squares = Map.build[Int, Int]: map =>
          var index = 0

          while index < 10000 do
            map(index) = index*index
            index += 1

        var found = 0
        var index = 0

        while index < 10000 do
          if squares.at(index) == index*index then found += 1
          index += 1

        (squares.size, found, squares.at(10000))
      . assert(_ == (10000, 10000, Unset))

      test(m"defining a key on a built map leaves the original as it was"):
        val original = cast
        val extended = original.define(t"Yorick", 7)
        (original.size, original.at(t"Yorick"), extended.size, extended.at(t"Yorick"))
      . assert(_ == (6, Unset, 7, 7))

      test(m"a built map that has been defined on still equals the literal"):
        val original = cast
        original.define(t"Yorick", 7)
        original == literal
      . assert(_ == true)

      test(m"omitting a key from a built map removes only that key"):
        cast.omit(t"Claudius")
      . assert(_ == Map(t"Hamlet" -> 30, t"Ophelia" -> 18, t"Laertes" -> 25, t"Gertrude" -> 50,
          t"Horatio" -> 30))

      test(m"omitting a key that is not there gives an equal map"):
        cast.omit(t"Yorick")
      . assert(_ == literal)

      // A test of the primitive beneath `.to(Map)` and the companion's `Factory`, not a way to
      // convert: the standard library's own `from` would copy a built map into a tree.
      test(m"the from primitive hands a built map back uncopied"):
        val raw = cast.stdlib
        Map.from(raw).stdlib eq raw
      . assert(_ == true)

      // Keys are compared as the standard library compares them, so that a map answers the
      // same before and after it has grown its persistent tree: a `Long` one is an `Int` one.
      test(m"numeric keys of different types are the same key"):
        val numbers = Map.build[Any, Text]: map =>
          map(1) = t"one"
          map(2) = t"two"
          map(3) = t"three"
          map(4) = t"four"
          map(5) = t"five"
          map(1L) = t"uno"

        (numbers.size, numbers.at(1), numbers.at(2.0), numbers.define(6, t"six").at(1L))
      . assert(_ == (5, t"uno", t"two", t"uno"))

      test(m"a map may be keyed by null"):
        val keyed = Map.build[Text | Null, Int]: map =>
          map(null) = 0
          map(t"Hamlet") = 30
          map(t"Ophelia") = 18
          map(t"Laertes") = 25
          map(t"Gertrude") = 50

        (keyed.size, keyed.at(null), keyed.defines(null), keyed.define(t"Horatio", 30).at(null))
      . assert(_ == (5, 0, true, 0))

      test(m"a block that only writes to its builder compiles"):
        demilitarize:
          Map.build[Int, Int] { map => map(1) = 2 }
      . assert(_ == Nil)

      test(m"the builder cannot outlive its block"):
        demilitarize:
          var later: () => Unit = () => ()
          Map.build[Int, Int] { map => later = () => map(1) = 2 }
          later()
      . assert(!_.nil)
