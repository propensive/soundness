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
package adversaria

import soundness.*

// `unique` is also an example annotation here; this named import takes precedence
// over `soundness.*`'s `unique` (the sole-element-of-a-`Seq` helper).
import adversaria.unique

// A sum type with `@name`-annotated variants, for `subtypeAnnotations` /
// `variantRelabelling`. `Accept` is renamed for one type argument, `Reject` by a
// bare `@name` (i.e. `name[Any]`), and `Defer` is unannotated.
sealed trait Decision
@name[Person](t"yes")
case object Accept extends Decision

@name(t"no")
case object Reject extends Decision

case object Defer  extends Decision

object Tests extends Suite(m"Adversaria tests"):

  def run(): Unit =
    test(m"access field annotations"):
      summon[Employee is Annotated on "code"]()
    . assert(_ == Set(ident(), unique()))

    test(m"access specific field annotation"):
      summon[Employee is Annotated by unique on "code"]()
    . assert(_ == Set(unique()))

    test(m"access type annotations"):
      summon[Company is Annotated]()
    . assert(_.has(number(10)))

    test(m"access type annotations with no annotations"):
      summon[Colored is Annotated]()
    . assert(_ == Set())

    test(m"exclude type annotations"):
      summon[Company is Annotated by unique].annotations
    . assert(_ == Set())

    test(m"unique annotation"):
      summon[Letters is Annotated].fields
    . assert: result =>
        result ==
          Map("alpha" -> Set(ref(1)), "beta" -> Set(ref(2), ref(3)), "delta" -> Set(ref(4)))

    test(m"unique annotation 2"):
      summon[adversaria.Hsv is Annotated by ident].field
    . assert(_ == "value")

    test(m"subtype annotations"):
      summon[Annotated under Colored].subtypes
    . assert(_ == Map("Rgb" -> Set(unique()), "Hsv" -> Set(number(3))))

    test(m"read a type-parameterized annotation, filtered by type argument"):
      summon[Marked is Annotated by marker[Person]].fields
    . assert(_ == Map("one" -> Set(marker[Person](1)), "two" -> Set()))

    test(m"a different type argument selects different annotations"):
      summon[Marked is Annotated by marker[Company]].fields
    . assert(_ == Map("one" -> Set(marker[Company](2)), "two" -> Set(marker[Company](3))))

    test(m"fieldAnnotations drops fields without the queried annotation"):
      fieldAnnotations[Marked, marker[Person]]
    . assert(_ == Map(t"one" -> Set(marker[Person](1))))

    test(m"a bare annotation (no type argument) is read as the `Any` instance"):
      fieldAnnotations[Tagged, marker[Any]]
    . assert(_ == Map(t"bare" -> Set(marker(1)), t"anyArg" -> Set(marker(2))))

    test(m"a bare annotation is not read by a specific type-argument query"):
      fieldAnnotations[Tagged, marker[Person]]
    . assert(_ == Map(t"specific" -> Set(marker[Person](3))))

    test(m"subtypeAnnotations reads @name on sum-type variants"):
      subtypeAnnotations[Decision, name[Person]]
    . assert(_ == Map(t"Accept" -> Set(adversaria.name[Person](t"yes"))))

    test(m"variantRelabelling merges per-format and bare variant renames"):
      variantRelabelling[Decision, Person]
    . assert(_ == Map(t"Accept" -> t"yes", t"Reject" -> t"no"))

    test(m"List map of fields of an object"):
      summon[Example1.type is Dereferenceable to Int].members(Example1)
    . assert(_ == Map(t"foo" -> 42, t"baz" -> 12))

    test(m"Get all members of a particular type"):
      Example1.membersOfType[Int].to[Set]
    . assert(_ == Set(12, 42))

    // The discovered accessors are panopticon `Lens`es (#490), so they write as well as read.
    suite(m"Field accessors as lenses"):
      val dereferenceable = summon[Letters is Dereferenceable to Int]
      val letters = Letters(1, 2, 3, 4)

      test(m"a lens reads the field it names"):
        dereferenceable.lens(t"beta").let(_(letters))
      . assert(_ == 2)

      test(m"a lens writes the field it names, leaving the others"):
        dereferenceable.lens(t"beta").let(_.update(letters, 20))
      . assert(_ == Letters(1, 20, 3, 4))

      test(m"a lens modifies in place"):
        dereferenceable.lens(t"delta").let(_.modify(letters)(_*10))
      . assert(_ == Letters(1, 2, 3, 40))

      test(m"update through the typeclass"):
        dereferenceable.update(letters, t"alpha", 100)
      . assert(_ == Letters(100, 2, 3, 4))

      test(m"modify through the typeclass"):
        dereferenceable.modify(letters, t"gamma")(_ + 7)
      . assert(_ == Letters(1, 2, 10, 4))

      test(m"a name which is not a field has no lens"):
        dereferenceable.lens(t"epsilon")
      . assert(_ == Unset)

      // Reading is unchanged: every discovered field still reports, whether or not it is
      // writable, so nothing that depended on the read-only accessors changes.
      test(m"reading still covers every discovered field"):
        dereferenceable.members(letters)
      . assert(_ == Map(t"alpha" -> 1, t"beta" -> 2, t"gamma" -> 3, t"delta" -> 4))

      // An object's `val`s are not constructor parameters, so there is nothing to write: they
      // remain readable, and simply have no lens.
      test(m"a field which is not a constructor parameter is readable"):
        summon[Example1.type is Dereferenceable to Int].select(Example1, t"foo")
      . assert(_ == 42)

      test(m"a field which is not a constructor parameter has no lens"):
        summon[Example1.type is Dereferenceable to Int].lens(t"foo")
      . assert(_ == Unset)

      // Finding an annotated field and then writing through it, which is what the name alone
      // could not do.
      test(m"an annotated field yields a lens onto itself"):
        val annotated = summon[Person is Annotated by ident]
        annotated.lens.update(Person(t"Jack", t"jack@example.com"), t"jill@example.com")
      . assert(_ == Person(t"Jack", t"jill@example.com"))

      test(m"an annotated field's lens reads it"):
        summon[Person is Annotated by ident].lens(Person(t"Jack", t"jack@example.com"))
      . assert(_ == t"jack@example.com")
