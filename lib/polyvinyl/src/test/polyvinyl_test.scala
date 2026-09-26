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
package polyvinyl

import soundness.*

import strategies.throwUnsafely

object Tests extends Suite(m"Polyvinyl tests"):
  val alice: Tree = Tree.Node(Map(
    t"name"   -> Tree.Leaf(t"Alice"),
    t"size"   -> Tree.Leaf(t"12345"),
    t"active" -> Tree.Leaf(t"yes"),
    t"raw"    -> Tree.Leaf(t"x"),
    t"extras" -> Tree.Leaf(t"")))

  val bob: Tree = Tree.Node(Map(t"name" -> Tree.Leaf(t"Bob")))

  val dave: Tree = Tree.Node(Map(
    t"nickname" -> Tree.Leaf(t"Dai"),
    t"aliases"  -> Tree.Items(List(Tree.Leaf(t"D"), Tree.Leaf(t"Davey"))),
    t"partner"  -> Tree.Node(Map(t"name" -> Tree.Leaf(t"Eve"))),
    t"pets"     -> Tree.Items(List(
      Tree.Node(Map(t"name" -> Tree.Leaf(t"Rex"))),
      Tree.Node(Map(t"name" -> Tree.Leaf(t"Tom")))))))

  val estate: Tree = Tree.Node(Map(
    t"owner" -> Tree.Node(Map(
      t"name"    -> Tree.Leaf(t"Carol"),
      t"address" -> Tree.Node(Map(t"city" -> Tree.Leaf(t"Tallinn"))))),
    t"tags"  -> Tree.Items(List(
      Tree.Node(Map(t"label" -> Tree.Leaf(t"old"))),
      Tree.Node(Map(t"label" -> Tree.Leaf(t"large")))))))

  def run(): Unit =
    suite(m"Scalar fields"):
      test(m"a text field is read with its declared Text type"):
        PersonRecords.record(alice).name
      . assert(_ == t"Alice")

      test(m"a field's result type may differ from its origin type"):
        val size: Int = PersonRecords.record(alice).size
        size
      . assert(_ == 5)

      test(m"a flag field is true when the field is present"):
        PersonRecords.record(alice).active
      . assert(_ == true)

      test(m"a flag field is false when the field is absent"):
        PersonRecords.record(bob).active
      . assert(_ == false)

      test(m"a field may return the raw origin value"):
        PersonRecords.record(alice).raw
      . assert(_ == Tree.Leaf(t"x"))

      test(m"a record retains its underlying data"):
        PersonRecords.record(alice).data
      . assert(_ == alice)

      test(m"selectDynamic dispatches on the field name"):
        PersonRecords.record(alice).selectDynamic("name")
      . assert(_ == t"Alice")

    suite(m"Member parameters"):
      test(m"a member's parameters reach the Intensional instance verbatim"):
        PersonRecords.record(alice).extras
      . assert(_ == List(t"alpha", t"beta"))

      test(m"a record evaluates a field on every access"):
        val record = PersonRecords.record(alice)
        val before = TreeProvider.evaluations.get
        record.count
        record.count
        TreeProvider.evaluations.get - before
      . assert(_ == 2)

    suite(m"Nested records"):
      test(m"a nested member yields a nested record"):
        NestedRecords.record(estate).owner.name
      . assert(_ == t"Carol")

      test(m"nested records nest again"):
        NestedRecords.record(estate).owner.address.city
      . assert(_ == t"Tallinn")

      test(m"a repeated record member yields a list of records"):
        NestedRecords.record(estate).tags.map(_.label)
      . assert(_ == List(t"old", t"large"))

      test(m"an absent list member yields an empty list"):
        NestedRecords.record(Tree.Node(Map())).tags
      . assert(_ == List())

    suite(m"Multiplicity"):
      test(m"an optional field is Unset when absent"):
        MultiplicityRecords.record(bob).nickname
      . assert(_ == Unset)

      test(m"an optional field is present when supplied"):
        MultiplicityRecords.record(dave).nickname
      . assert(_ == t"Dai")

      test(m"an optional field has an Optional type"):
        val nickname: Optional[Text] = MultiplicityRecords.record(dave).nickname
        nickname
      . assert(_ == t"Dai")

      test(m"a repeated scalar field is a list"):
        MultiplicityRecords.record(dave).aliases
      . assert(_ == List(t"D", t"Davey"))

      test(m"an absent repeated field is an empty list"):
        MultiplicityRecords.record(bob).aliases
      . assert(_ == List())

      test(m"an optional record is Unset when absent"):
        MultiplicityRecords.record(bob).partner
      . assert(_ == Unset)

      test(m"an optional record is read when present"):
        MultiplicityRecords.record(dave).partner.let(_.name)
      . assert(_ == t"Eve")

      test(m"a repeated record is a list of records"):
        MultiplicityRecords.record(dave).pets.map(_.name)
      . assert(_ == List(t"Rex", t"Tom"))

      test(m"a tuple carries optional and repeated elements"):
        val tuple = MultiplicityRecords.tuple(dave)
        (tuple.nickname, tuple.aliases, tuple.partner.let(_.name), tuple.pets.map(_.name))
      . assert(_ == (t"Dai", List(t"D", t"Davey"), t"Eve", List(t"Rex", t"Tom")))

    suite(m"Unions and keyed members"):
      val shapes: Tree = Tree.Node(Map(
        t"id"     -> Tree.Leaf(t"x1"),
        t"tags"   -> Tree.Items(List(Tree.Leaf(t"a"), Tree.Leaf(t"b"))),
        t"sizes"  -> Tree.Leaf(t"four"),
        t"labels" -> Tree.Node(Map(t"k" -> Tree.Leaf(t"v"))),
        t"owners" -> Tree.Node(Map(t"o" -> Tree.Node(Map(t"name" -> Tree.Leaf(t"Olga"))))),
        t"rows"   -> Tree.Items(List(Tree.Items(List(Tree.Leaf(t"r1"))), Tree.Items(List())))))

      val shapes2: Tree = Tree.Node(Map(
        t"id"   -> Tree.Node(Map(t"name" -> Tree.Leaf(t"n1"))),
        t"tags" -> Tree.Leaf(t"solo")))

      test(m"a union reads the alternative matching the value's kind"):
        ShapeRecords.record(shapes).id
      . assert(_ == t"x1")

      test(m"a union's other alternative is a nested record"):
        ShapeRecords.record(shapes2).id match
          case text: Text => text
          case record: Record => record.selectDynamic("name")
      . assert(_ == t"n1")

      test(m"a union has the union of its alternatives' types"):
        val id: Text | Record = ShapeRecords.record(shapes).id
        id
      . assert(_ == t"x1")

      test(m"a repeated alternative reads as a list"):
        ShapeRecords.record(shapes).tags
      . assert(_ == List(t"a", t"b"))

      test(m"a single alternative reads as one value"):
        ShapeRecords.record(shapes2).tags
      . assert(_ == t"solo")

      test(m"an optional union reads Unset when absent"):
        ShapeRecords.record(shapes2).sizes
      . assert(_ == Unset)

      test(m"an optional union reads its alternative when present"):
        ShapeRecords.record(shapes).sizes
      . assert(_ == (4: Optional[Int | List[Int]]))

      test(m"a keyed value member reads as a map"):
        ShapeRecords.record(shapes).labels
      . assert(_ == Map(t"k" -> t"v"))

      test(m"a keyed record member reads as a map of records"):
        ShapeRecords.record(shapes).owners.stdlib.map { (key, owner) => (key, owner.name) }
      . assert(_ == scala.collection.immutable.Map(t"o" -> t"Olga"))

      test(m"an absent keyed member reads as an empty map"):
        ShapeRecords.record(shapes2).labels
      . assert(_ == Map())

      test(m"a single-alternative union nests a multiplicity"):
        ShapeRecords.record(shapes).rows.stdlib.map(_.stdlib)
      . assert(_ == scala.collection.immutable.List(scala.collection.immutable.List(t"r1"), Nil))

      test(m"a tuple reads unions and maps too"):
        val tuple = ShapeRecords.tuple(shapes)
        (tuple.id, tuple.tags, tuple.labels)
      . assert(_ == (t"x1", List(t"a", t"b"), Map(t"k" -> t"v")))

    suite(m"Independence"):
      test(m"records from the same specification are independent"):
        val a = PersonRecords.record(alice)
        val b = PersonRecords.record(bob)
        (a.name, b.name)
      . assert(_ == (t"Alice", t"Bob"))

      test(m"specifications coexist with distinct fields"):
        val title = TitleRecords.record(Tree.Node(Map(t"title" -> Tree.Leaf(t"Dr"))))
        (title.title, PersonRecords.record(alice).name)
      . assert(_ == (t"Dr", t"Alice"))

    suite(m"Named tuples"):
      test(m"a tuple has the specification's names and types, in order"):
        val tuple
        :   (name: Text, size: Int, active: Boolean, raw: Tree, extras: List[Text], count: Int) =
          PersonRecords.tuple(alice)

        tuple.name
      . assert(_ == t"Alice")

      test(m"a tuple's elements are accessed by name"):
        PersonRecords.tuple(alice).size
      . assert(_ == 5)

      test(m"a tuple destructures positionally"):
        val (name, size, active, _, _, _) = PersonRecords.tuple(alice)
        (name, size, active)
      . assert(_ == (t"Alice", 5, true))

      test(m"a tuple's underlying tuple is plain"):
        TitleRecords.tuple(Tree.Node(Map(t"title" -> Tree.Leaf(t"Dr")))).toTuple
      . assert(_ == Tuple1(t"Dr"))

      test(m"a nested node becomes a nested tuple"):
        NestedRecords.tuple(estate).owner.address.city
      . assert(_ == t"Tallinn")

      test(m"a list member becomes a list of tuples"):
        NestedRecords.tuple(estate).tags.map(_.label)
      . assert(_ == List(t"old", t"large"))

      test(m"a tuple evaluates each field once, when it is built"):
        val before = TreeProvider.evaluations.get
        val tuple = PersonRecords.tuple(alice)
        tuple.count
        tuple.count
        TreeProvider.evaluations.get - before
      . assert(_ == 1)

      test(m"an undeclared tuple element does not compile"):
        demilitarize(PersonRecords.tuple(Tree.Absent).nope)
      . assert(_.exists(_.reason == CompileError.Reason.NotAMember))

    suite(m"Compiletime checks"):
      test(m"an undeclared field does not compile"):
        demilitarize(PersonRecords.record(Tree.Absent).nope)
      . assert(_.exists(_.reason == CompileError.Reason.NotAMember))

      test(m"a field cannot be used at the wrong type"):
        demilitarize:
          val size: Text = PersonRecords.record(Tree.Absent).size
      . assert(_.exists(_.reason == CompileError.Reason.TypeMismatch))

      test(m"a missing Intensional instance is a compile error"):
        demilitarize(UnknownValueRecords.record(Tree.Absent))
      . assert(_.exists(_.message.contains("could not find an Intensional instance")))

      test(m"an instance is chosen by its exact label"):
        demilitarize(MiscasedRecords.record(Tree.Absent))
      . assert(_.exists(_.message.contains("with type Text")))

      test(m"a nested record does not expose its parent's fields"):
        demilitarize(NestedRecords.record(Tree.Absent).owner.tags)
      . assert(_.exists(_.reason == CompileError.Reason.NotAMember))
