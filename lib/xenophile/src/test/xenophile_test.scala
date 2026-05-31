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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package xenophile

import soundness.*
import jacinta.*

type TsInterface = Interface in Typescript at "/xenophile/definitions.ts"

given tsInterface: TsInterface = Interface[Typescript](cp"/xenophile/definitions.ts")

val document: Json =
  j"""{"Foo": {"baz": "hello", "bar": {"count": 42}, "tags": ["a", "b"], "nickname": "Bob",
       "id": "abc123", "lookup": {"1": "one", "2": "two"}}}"""

given Evaluator in Typescript by Json = Typescript.evaluator(document)

object Tests extends Suite(m"Xenophile tests"):
  def run(): Unit =
    val foo: Foreign of "Foo" from Typescript = Foreign["Foo", Typescript]

    suite(m"Foreign type navigation"):
      test(m"a member access has the precise refined static type"):
        val bar: Foreign of "Bar" from Typescript = foo.bar
        bar.expr
      . assert(_ == ForeignExpr.Select(ForeignExpr.Reference(t"Foo"), t"bar"))

      test(m"navigate a cyclic foreign type graph"):
        val cyclic: Foreign of "Foo" from Typescript = foo.bar.qux
        cyclic.expr
      . assert(_ == ForeignExpr.Select(ForeignExpr.Select(ForeignExpr.Reference(t"Foo"), t"bar"), t"qux"))

    suite(m"Function application"):
      test(m"applyDynamic builds an `Apply` node typed by the method's result"):
        val greeting: Foreign of "string" from Typescript = foo.greet(t"hello")
        greeting.expr
      . assert:
          case ForeignExpr.Apply(ForeignExpr.Select(_, member), List(_)) => member == t"greet"
          case _                                                         => false

      test(m"a Foreign argument of the declared parameter type is accepted"):
        val linked: Foreign of "Foo" from Typescript = foo.link(foo.bar)
        linked.expr
      . assert:
          case ForeignExpr.Apply(ForeignExpr.Select(_, m), List(ForeignExpr.Select(_, b))) =>
            m == t"link" && b == t"bar"

          case _ =>
            false

    suite(m"Interoperability"):
      test(m"a Scala value converts into a `Foreign` literal"):
        val text: Foreign of "string" from Typescript = t"hello"
        text.expr
      . assert:
          case ForeignExpr.Literal(_) => true
          case _                      => false

      test(m"a foreign literal converts back to a Scala value via `as`"):
        val text: Foreign of "string" from Typescript = t"hello"
        text.as[Text]
      . assert(_ == t"hello")

    suite(m"Evaluation"):
      test(m"evaluate a selected leaf field against the backend and decode it"):
        foo.baz.as[Text]
      . assert(_ == t"hello")

      test(m"evaluate a nested selection against the backend and decode it"):
        foo.bar.count.as[Int]
      . assert(_ == 42)

    suite(m"Complex types"):
      test(m"an array field is read as `Array<T>` and decodes to a List"):
        val tags: Foreign of ("Array" applied "string") from Typescript = foo.tags
        tags.as[List[Text]]
      . assert(_ == List(t"a", t"b"))

      test(m"an optional field is a union with `undefined` and decodes to an Optional"):
        val nickname: Foreign of ("string" | "undefined") from Typescript = foo.nickname
        nickname.as[Optional[Text]]
      . assert(_ == t"Bob")

      test(m"a union field has a bare-union foreign type and decodes to a Scala union"):
        val id: Foreign of ("string" | "number") from Typescript = foo.id
        id.as[Text | Int]
      . assert(_ == t"abc123")

      test(m"a generic field has an applied foreign type and decodes to a Scala Map"):
        val lookup: Foreign of ("Map" applied ("number", "string")) from Typescript = foo.lookup
        lookup.as[Map[Int, Text]]
      . assert(_ == Map(1 -> t"one", 2 -> t"two"))

    suite(m"Compile-time safety"):
      test(m"selecting an undefined member is a compile error"):
        demilitarize(foo.nonexistent).map(_.message)
      . assert(_ == List(t"xenophile: the foreign type Foo has no member nonexistent"))

      test(m"calling a method with the wrong arity is a compile error"):
        demilitarize(foo.greet(t"a", t"b")).map(_.message)
      . assert(_ == List(t"xenophile: greet expects 1 arguments, not 2"))

      test(m"passing an argument of the wrong foreign type is a compile error"):
        demilitarize(foo.greet(42)).map(_.message)
      . assert(_ == List(t"xenophile: cannot pass this argument as string to greet"))
