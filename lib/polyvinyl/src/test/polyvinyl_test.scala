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
        val before = TreeBlueprint.evaluations.get
        record.count
        record.count
        TreeBlueprint.evaluations.get - before
      . assert(_ == 2)

    suite(m"Nested records"):
      test(m"an identity Structural yields a nested record"):
        NestedRecords.record(estate).owner.name
      . assert(_ == t"Carol")

      test(m"nested records nest again"):
        NestedRecords.record(estate).owner.address.city
      . assert(_ == t"Tallinn")

      test(m"a List Structural yields a list of records"):
        NestedRecords.record(estate).tags.map(_.label)
      . assert(_ == List(t"old", t"large"))

      test(m"an absent list member yields an empty list"):
        NestedRecords.record(Tree.Node(Map())).tags
      . assert(_ == List())

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

      test(m"a missing Structural instance is a compile error"):
        demilitarize(UnknownRecordRecords.record(Tree.Absent))
      . assert(_.exists(_.message.contains("could not find a Structural instance")))

      test(m"an instance is chosen by its exact label"):
        demilitarize(MiscasedRecords.record(Tree.Absent))
      . assert(_.exists(_.message.contains("with type Text")))

      test(m"a nested record does not expose its parent's fields"):
        demilitarize(NestedRecords.record(Tree.Absent).owner.tags)
      . assert(_.exists(_.reason == CompileError.Reason.NotAMember))
