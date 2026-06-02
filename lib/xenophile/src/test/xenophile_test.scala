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

import java.lang.foreign.Arena, java.lang.foreign.ValueLayout.JAVA_INT
import java.nio.file.Files

import strategies.throwUnsafely

type TsInterface = Interface in Typescript at "/xenophile/definitions.ts"

given tsInterface: TsInterface = Interface[Typescript](cp"/xenophile/definitions.ts")

val document: Json =
  j"""{"Foo": {"baz": "hello", "bar": {"count": 42}, "tags": ["a", "b"], "counts": [1, 2, 3],
       "nickname": "Bob", "id": "abc123", "lookup": {"1": "one", "2": "two"}}}"""

given Evaluator in Typescript by Json = Typescript(document)

type NativeLibrary = Interface in Native at "/xenophile/library.h"

given nativeLibrary: NativeLibrary = Interface[Native](cp"/xenophile/library.h")

given Evaluator in Native by Memory =
  Native(t"typedef struct Point { int x; int y; } Point; int abs(int n);")

type WitApi = Interface in Wit at "/xenophile/api.wit"

given witApi: WitApi = Interface[Wit](cp"/xenophile/api.wit")

val witDocument: Json = j"""{"point": {"x": 3, "y": 4}}"""

given witEvaluator: (Evaluator in Wit by Json) = Wit(witDocument)

object Tests extends Suite(m"Xenophile tests"):
  def run(): Unit =
    val foo: Foreign of "Foo" from Typescript = Foreign["Foo", Typescript]

    suite(m"Foreign type navigation"):
      test(m"a member access has the precise refined static type"):
        val bar: Foreign of "Bar" from Typescript = foo.bar
        bar.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Foo"), t"bar", t"Foo"))

      test(m"navigate a cyclic foreign type graph"):
        val cyclic: Foreign of "Foo" from Typescript = foo.bar.qux
        cyclic.expr
      . assert: expr =>
          val inner = Foreign.Expression.Select(Foreign.Expression.Reference(t"Foo"), t"bar", t"Foo")
          expr == Foreign.Expression.Select(inner, t"qux", t"Bar")

    suite(m"Function application"):
      test(m"applyDynamic builds an `Apply` node typed by the method's result"):
        val greeting: Foreign of "string" from Typescript = foo.greet(t"hello")
        greeting.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, member, _), List(_)) => member == t"greet"
          case _                                                         => false

      test(m"a Foreign argument of the declared parameter type is accepted"):
        val linked: Foreign of "Foo" from Typescript = foo.link(foo.bar)
        linked.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(Foreign.Expression.Select(_, b, _))) =>
            m == t"link" && b == t"bar"

          case _ =>
            false

    suite(m"Interoperability"):
      test(m"a Scala value converts into a `Foreign` literal"):
        val text: Foreign of "string" from Typescript = t"hello"
        text.expr
      . assert:
          case Foreign.Expression.Literal(_) => true
          case _                      => false

      test(m"a Scala argument is converted to a `Foreign` literal upon application"):
        foo.greet(t"hi").expr
      . assert:
          case Foreign.Expression.Apply(_, List(Foreign.Expression.Literal(_))) => true
          case _                                                  => false

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
        val tags: Foreign of ("Array" over "string") from Typescript = foo.tags
        tags.as[List[Text]]
      . assert(_ == List(t"a", t"b"))

      test(m"the generic List instance decodes an `Array<number>` to a List[Int]"):
        val counts: Foreign of ("Array" over "number") from Typescript = foo.counts
        counts.as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"an optional field is a union with `undefined` and decodes to an Optional"):
        val nickname: Foreign of ("string" | "undefined") from Typescript = foo.nickname
        nickname.as[Optional[Text]]
      . assert(_ == t"Bob")

      test(m"a present Optional value converts to a `Foreign` literal and round-trips"):
        val opt: Foreign of ("string" | "undefined") from Typescript = t"hi": Optional[Text]
        opt.as[Optional[Text]]
      . assert(_ == t"hi")

      test(m"an absent Optional value converts to `undefined` and decodes to Unset"):
        val opt: Foreign of ("string" | "undefined") from Typescript = Unset: Optional[Text]
        opt.as[Optional[Text]]
      . assert(_ == Unset)

      test(m"a union field has a bare-union foreign type and decodes to a Scala union"):
        val id: Foreign of ("string" | "number") from Typescript = foo.id
        id.as[Text | Int]
      . assert(_ == t"abc123")

      test(m"a generic field has an `over` foreign type and decodes to a Scala Map"):
        val lookup: Foreign of ("Map" over ("number", "string")) from Typescript = foo.lookup
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
      . assert(_ == List(t"xenophile: greet expects an argument of foreign type string"))

    suite(m"Native (C headers)"):
      val library: Foreign of "library" from Native = Foreign["library", Native]

      test(m"a C struct field has the field's foreign type"):
        val point: Foreign of "Point" from Native = Foreign["Point", Native]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Point"), t"x", t"Point"))

      test(m"applying a C function builds an `Apply` node typed by its result"):
        val absolute: Foreign of "int" from Native = library.abs(5)
        absolute.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(Foreign.Expression.Literal(_))) =>
            m == t"abs"

          case _ =>
            false

      test(m"a function returning `const char*` has the C-string foreign type"):
        val version: Foreign of "string" from Native = library.version()
        version.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == t"version"
          case _                                                => false

      test(m"passing a C argument of the wrong foreign type is a compile error"):
        demilitarize(library.abs(t"five")).map(_.message)
      . assert(_ == List(t"xenophile: abs expects an argument of foreign type int"))

      test(m"a native function downcall through FFM returns the real result"):
        library.abs(-7).as[Int]
      . assert(_ == 7)

      test(m"a `union` field has the field's foreign type"):
        val number: Foreign of "Number" from Native = Foreign["Number", Native]
        number.f.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Number"), t"f", t"Number"))

      test(m"a `typedef` alias resolves to its underlying foreign type"):
        val counter: Foreign of "int" from Native = library.increment(1)
        counter.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"increment"
          case _                                                    => false

      test(m"an `enum`-typed parameter and result are treated as `int`"):
        val direction: Foreign of "int" from Native = library.flip(0)
        direction.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"flip"
          case _                                                    => false

      test(m"a fixed-width `int32_t` is canonicalised to `int`"):
        val value: Foreign of "int" from Native = library.identity(42)
        value.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"identity"
          case _                                                    => false

      test(m"struct fields are read from native memory at their computed offsets"):
        val segment = Arena.ofConfined().nn.allocate(8L).nn
        segment.set(JAVA_INT, 0L, 3)
        segment.set(JAVA_INT, 4L, 4)

        val point: Foreign of "Point" from Native =
          Foreign.make(Foreign.Expression.Literal(Memory(segment))).asInstanceOf[Foreign of "Point" from Native]

        (point.x.as[Int], point.y.as[Int])
      . assert(_ == (3, 4))

      // Builds a tiny C `cdylib` (the same C ABI a Rust crate's `extern "C"` API exposes), loads it
      // by path, and calls into it — the path Scala↔Rust interop would take.
      test(m"a function in a loaded shared library is called through FFM"):
        val directory = Files.createTempDirectory("xenophile").nn
        val source = directory.resolve("add.c").nn
        Files.writeString(source, "int add(int a, int b) { return a + b; }")
        val target = directory.resolve("libadd.so").nn

        // If `cc` fails, the library is not produced and `libraryLookup` below raises, failing
        // the test with a clear native-loading error.
        val _ =
          ProcessBuilder("cc", "-shared", "-fPIC", "-o", target.toString.nn, source.toString.nn)
            .inheritIO().nn.start().nn.waitFor()

        given Evaluator in Native by Memory =
          Native(t"int add(int a, int b);", target.toString.nn.tt)

        val library: Foreign of "library" from Native = Foreign["library", Native]
        library.add(2, 3).as[Int]
      . assert(_ == 5)

    suite(m"Wit (WebAssembly Interface Types)"):
      val api: Foreign of "api" from Wit = Foreign["api", Wit]

      test(m"a WIT record field has the field's foreign type"):
        val point: Foreign of "point" from Wit = Foreign["point", Wit]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"point"), t"x", t"point"))

      test(m"a WIT record field decodes from the backend document"):
        val point: Foreign of "point" from Wit = Foreign["point", Wit]
        point.x.as[Int]
      . assert(_ == 3)

      test(m"a WIT function application is typed by its result"):
        val sum: Foreign of "s32" from Wit = api.add(2, 3)
        sum.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_, _)) => m == t"add"
          case _                                                                         => false

      test(m"a WIT `list<T>` result has an `over` foreign type"):
        val tags: Foreign of ("list" over "string") from Wit = api.tags()
        tags.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == t"tags"
          case _                                                                  => false

      test(m"a WIT `option<T>` result is a union with `none`"):
        val found: Foreign of ("string" | "none") from Wit = api.lookup(t"k")
        found.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"lookup"
          case _                                                                      => false

      test(m"passing a WIT argument of the wrong foreign type is a compile error"):
        demilitarize(api.add(t"two", 3)).map(_.message)
      . assert(_ == List(t"xenophile: add expects an argument of foreign type s32"))
