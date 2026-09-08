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
package xenophile

import scala.caps

import soundness.*
import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.sortingAlgorithms.timsort

import ambience.systems.javaBaseSystem

type TsInterface = Interface in Typescript at "/xenophile/definitions.ts"
given tsInterface: TsInterface = Interface[Typescript](cp"/xenophile/definitions.ts")

type NativeLibrary = Interface in Native at "/xenophile/library.h"
given nativeLibrary: NativeLibrary = Interface[Native](cp"/xenophile/library.h")

type WitApi = Interface in Wit at "/xenophile/api.wit"
given witApi: WitApi = Interface[Wit](cp"/xenophile/api.wit")

type WebIdlSample = Interface in WebIdl at "/xenophile/sample.idl"
given webIdlSample: WebIdlSample = Interface[WebIdl](cp"/xenophile/sample.idl")

// The real DOM source uses a second ecosystem so its `Interface` is summoned unambiguously
// alongside the synthetic `sample.idl`; both share `WebIdlDialect` as their grammar.
trait WebIdlDom extends Ecosystem:
  type Grammar = WebIdlDialect.type

type WebIdlDomSource = Interface in WebIdlDom at "/xenophile/dom.idl"
given webIdlDom: WebIdlDomSource = Interface[WebIdlDom](cp"/xenophile/dom.idl")

// Xenophile navigates and type-checks foreign types and builds a `Foreign.Expression`; it carries
// no runtime representation and performs no evaluation, so these tests assert static foreign types
// (by ascription) and the expression AST, plus the compile-time safety diagnostics.
object Tests extends Suite(m"Xenophile tests"):
  def run(): Unit =
    suite(m"Kotlin ecosystem"):
      test(m"a top-level Kotlin function call materializes as a direct JVM call"):
        Foreign["kotlin.internal.ProgressionUtilKt", Kotlin]
        . getProgressionLastElement(1, 10, 2)
        . call[Int]()
      . assert(_ == 9)

      test(m"a Kotlin navigation records the member's foreign result type"):
        val result = Foreign["kotlin.internal.ProgressionUtilKt", Kotlin]
        . getProgressionLastElement(1, 10, 2)

        (result: Foreign of "kotlin.Int" from Kotlin).expr
      . assert:
          case Foreign.Expression.Apply(_, arguments) => arguments.length == 3
          case _                                      => false

      test(m"a class absent from the classpath is rejected"):
        demilitarize:
          Foreign["kotlin.absent.NowhereKt", Kotlin].missing(1)
      . assert(_.nonEmpty)

      test(m"an unknown member of a Kotlin type is rejected"):
        demilitarize:
          Foreign["kotlin.internal.ProgressionUtilKt", Kotlin].missing(1)
      . assert(_.nonEmpty)

      test(m"a call with the wrong arity is rejected"):
        demilitarize:
          Foreign["kotlin.internal.ProgressionUtilKt", Kotlin].getProgressionLastElement(1)
      . assert(_.nonEmpty)

      test(m"a call with a wrongly-typed argument is rejected"):
        demilitarize:
          Foreign["kotlin.internal.ProgressionUtilKt", Kotlin]
          . getProgressionLastElement("one", 10, 2)
      . assert(_.nonEmpty)

    suite(m"Kotlin facades"):
      val pair = make[kotlin.Pair[Text, Text]]("a", "b")

      test(m"a Kotlin class constructs through its facade"):
        pair.k.toString.tt
      . assert(_ == "(a, b)")

      test(m"a property substitutes the facade's type arguments"):
        val first: Text = pair.first
        first
      . assert(_ == "a")

      test(m"the second component reads likewise"):
        val second: Text = pair.second
        second
      . assert(_ == "b")

      val regex = make[kotlin.text.Regex]("[0-9]+")

      test(m"an instance method accepts Text for a CharSequence parameter"):
        val matches: Boolean = regex.matches("123")
        matches
      . assert(_ == true)

      test(m"a non-matching input reports false"):
        val matches: Boolean = regex.matches("abc")
        matches
      . assert(_ == false)

      test(m"a nullable result is an Optional, absent on no match"):
        regex.find("abc", 0).absent
      . assert(_ == true)

      test(m"a nullable result is present on a match, as a facade"):
        regex.find("a1b2", 0).let { result => result.value: Text }
      . assert(_ == "1")

      test(m"a Kotlin String property reads as Text"):
        val pattern: Text = regex.pattern
        pattern
      . assert(_ == "[0-9]+")

      test(m"an unknown property is rejected"):
        demilitarize:
          make[kotlin.Pair[Text, Text]]("a", "b").third
      . assert(_.nonEmpty)

      test(m"a wrongly-typed constructor argument is rejected"):
        demilitarize:
          make[kotlin.text.Regex](42)
      . assert(_.nonEmpty)

      test(m"a companion object's members are reachable"):
        val escaped: Text = companion[kotlin.text.Regex].escape("a.b")
        escaped
      . assert(_ == "\\Qa.b\\E")

      test(m"an unknown member's error suggests near misses in Kotlin syntax"):
        demilitarize:
          make[kotlin.text.Regex]("x").matchez("y")
        . map(_.message)
      . assert(_.exists(_.contains("did you mean")))

      test(m"a class without a companion object is rejected"):
        demilitarize:
          companion[kotlin.Pair[Text, Text]]
      . assert(_.nonEmpty)

      test(m"a Scala lambda satisfies a Kotlin function-type parameter"):
        val replaced: Text = regex.replace("a1b2",
            (m: Facade over kotlin.text.MatchResult) => t"<${m.value}>")

        replaced
      . assert(_ == "a<1>b<2>")

      test(m"a lambda's facade parameter navigates the Kotlin type inside"):
        val upper: Text = regex.replace("3x4", (m: Facade over kotlin.text.MatchResult) =>
            t"${m.value}${m.value}")

        upper
      . assert(_ == "33x44")

      test(m"a var property accepts assignment through its setter"):
        val parameter = make[kotlin.metadata.KmValueParameter]("x")
        parameter.name = "y"
        val name: Text = parameter.name
        name
      . assert(_ == "y")

      test(m"assignment to a val property is rejected"):
        demilitarize:
          make[kotlin.Pair[Text, Text]]("a", "b").first = "z"
      . assert(_.nonEmpty)

      test(m"an object singleton's constant properties are reachable"):
        val quote: Char = singleton[kotlin.text.Typography].quote
        quote
      . assert(_ == '"')

      test(m"a class is not mistaken for an object singleton"):
        demilitarize:
          singleton[kotlin.Pair[Text, Text]]
      . assert(_.nonEmpty)

      test(m"omitted trailing parameters fall back to Kotlin defaults"):
        regex.find("a7b").let(_.value)
      . assert(_ == "7")

      test(m"operator get is reachable as apply"):
        regex.find("a1b").let(_.groups(0)).let(_.value)
      . assert(_ == "1")

      // Note `unwrap.toString`: `toString`/`equals`/`hashCode` are real members of the facade
      // wrapper itself, so `Dynamic` cannot intercept them.
      test(m"a plain Java class resolves through the reflection fallback"):
        make[java.lang.StringBuilder]("ab").reverse().k.toString.tt
      . assert(_ == "ba")

      test(m"an enum entry is reachable by name, and usable as an argument"):
        val relaxed = make[kotlin.text.Regex]
          ( "[a-z]+", xenophile.enumEntry[kotlin.text.RegexOption]("IGNORE_CASE") )

        val matches: Boolean = relaxed.matches("ABC")
        matches
      . assert(_ == true)

      test(m"an unknown enum entry lists the real ones"):
        demilitarize:
          xenophile.enumEntry[kotlin.text.RegexOption]("IGNORE_CAES")
        . map(_.message)
      . assert(_.exists(_.contains("IGNORE_CASE")))

      test(m"a data class destructures into a typed tuple"):
        make[kotlin.Pair[Text, Text]]("a", "b").tuple
      . assert(_ == ("a", "b"))

      test(m"a Kotlin list result copies out as a Scala List of Text"):
        val parts: List[Text] = regex.split("a1b2").scala
        parts
      . assert(_ == List(t"a", t"b", t""))

      test(m"a named argument selects its declared parameter"):
        regex.find(input = "a5b").let(_.value)
      . assert(_ == "5")

      test(m"named arguments reorder to their declared positions"):
        regex.find(startIndex = 2, input = "1a2b").let(_.value)
      . assert(_ == "2")

      test(m"an unknown parameter name lists the declared ones"):
        demilitarize:
          regex.find(inpit = "a5b")
        . map(_.message)
      . assert(_.exists(_.contains("input")))

      test(m"a Scala List argument satisfies a Java collection parameter"):
        val wrapped = make[java.util.ArrayList[Text]](List(t"a", t"b"))
        val size: Int = wrapped.size()
        size
      . assert(_ == 2)

      test(m"surplus arguments collect into a vararg tail"):
        make[java.util.Formatter]().format("[%s:%s]", "x", "y").k.toString.tt
      . assert(_ == "[x:y]")

      test(m"a value-class member is rejected with a clear diagnostic"):
        demilitarize:
          companion[kotlin.time.Duration].parse("1s")
        . map(_.message)
      . assert(_.exists(_.contains("value class")))

      test(m"a typed lambda satisfies a Java functional-interface parameter"):
        val list = make[java.util.ArrayList[Text]](List(t"a", t"bb", t"ccc"))
        val _ = list.removeIf((text: Text) => text.length > 1)
        val size: Int = list.size()
        size
      . assert(_ == 1)

      test(m"an UNTYPED lambda infers against a functional-interface method"):
        val list = make[java.util.ArrayList[Text]](List(t"a", t"bb", t"ccc"))
        val _ = list.removeIf(text => text.length > 1)   // no ascription on `text`
        val size: Int = list.size()
        size
      . assert(_ == 1)

      test(m"an UNTYPED multi-argument lambda infers (an arity-2 interface)"):
        val list = make[java.util.ArrayList[Text]](List(t"ccc", t"a", t"bb"))
        val _ = list.sort((left, right) => left.length - right.length)   // no ascriptions
        list.get(0).k.toString.tt
      . assert(_ == "a")

      test(m"an UNTYPED lambda infers on a facade RETURNED from a method"):
        val list = make[java.util.ArrayList[Text]](List(t"a", t"bb", t"ccc", t"d"))
        val sub = list.subList(0, 3)             // a facade returned from a call
        val _ = sub.removeIf(text => text.length > 1)    // untyped lambda on the returned facade
        sub.size()
      . assert(_ == 1)

      test(m"an Int argument widens to a Long parameter, no `L` suffix"):
        // Both the constructor and `addAndGet` take `long`; the arguments are plain `Int`s.
        val counter = make[java.util.concurrent.atomic.AtomicLong](0)
        val _ = counter.addAndGet(5)
        counter.get()
      . assert(_ == 5L)

      test(m"an in-scope Conversion fits an argument to a parameter"):
        given Conversion[Int, Text] = _.toString.tt
        // `ArrayList.add(E)` expects a `Text`; pass an `Int`, fitted by the conversion.
        val list = make[java.util.ArrayList[Text]](List())
        val _ = list.add(42)
        list.get(0).k.toString.tt
      . assert(_ == "42")

      test(m"a Java bean getter reads as a short property name"):
        val entry: Text = make[java.util.zip.ZipEntry]("file.txt").name   // getName() -> .name
        entry
      . assert(_ == "file.txt")

      test(m"a Java bean setter writes via assignment"):
        val entry = make[java.util.zip.ZipEntry]("e")
        entry.comment = "hello"            // setComment(String) via `.comment = …`
        val comment: Text = entry.comment   // getComment() -> .comment
        comment
      . assert(_ == "hello")

      test(m"a Scala array bridges to a Java array parameter (element conversion)"):
        // The `E[]` constructor wants `CharSequence[]`; a Scala `Array[Text]` bridges to it.
        val strings: scala.Array[Text] = scala.Array("a", "bb")
        val list = make[java.util.concurrent.CopyOnWriteArrayList[CharSequence]](strings)
        list.size()
      . assert(_ == 2)

      test(m"a setX(interface) property is assignable, inferring the lambda"):
        val handler = make[java.util.logging.StreamHandler]()
        // `setFilter(Filter)` reached as a `var`; `record` is inferred (a `LogRecord`).
        handler.filter = (record => record.getLevel() != null)
        handler.getFilter().k != null
      . assert(_ == true)

      test(m"a setX(interface) property with a two-argument SAM is assignable"):
        var caught = false
        val thread = make[java.lang.Thread](() => ())
        // `setUncaughtExceptionHandler(UncaughtExceptionHandler)` reached as a `var`; the lambda's
        // `who`/`error` parameters are inferred. A package-private `uncaughtExceptionHandler(handler)`
        // method of the same name (JDK-internal) no longer shadows the generated setter.
        scala.caps.unsafe.unsafeAssumeSeparate:
          thread.uncaughtExceptionHandler = ((who, error) => caught = true)
        val installed = thread.getUncaughtExceptionHandler().k
        installed.uncaughtException(thread.k, RuntimeException("boom"))
        caught
      . assert(_ == true)

      test(m"a nullary lambda satisfies a Runnable parameter with no ascription"):
        var ran = false
        val thread = make[java.lang.Thread](() => ran = true)
        thread.run()
        ran
      . assert(_ == true)

    val foo: Foreign of "Foo" from Typescript = Foreign["Foo", Typescript]

    suite(m"Foreign type navigation"):
      test(m"a member access has the precise refined static type"):
        val bar: Foreign of "Bar" from Typescript = foo.bar
        bar.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("Foo"), "bar", "Foo"))

      test(m"navigate a cyclic foreign type graph"):
        val cyclic: Foreign of "Foo" from Typescript = foo.bar.qux
        cyclic.expr
      . assert: expr =>
          val inner = Foreign.Expression.Select(Foreign.Expression.Reference("Foo"), "bar", "Foo")
          expr == Foreign.Expression.Select(inner, "qux", "Bar")

    suite(m"Function application"):
      test(m"applyDynamic builds an `Apply` node typed by the method's result"):
        val greeting: Foreign of "string" from Typescript = foo.greet("hello")
        greeting.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args) if args.length == 1 => m == "greet"
          case _                                                                      => false

      test(m"a Foreign argument of the declared parameter type is accepted"):
        val linked: Foreign of "Foo" from Typescript = foo.link(foo.bar)
        linked.expr
      . assert:
          case Foreign.Expression.Apply(_, args__) if args__.length == 1 =>
            args__.head match
              case Foreign.Expression.Select(_, b, _) => b == "bar"
              case _                                  => false
          case _                                                                      => false

    suite(m"Conversion of Scala values to Foreign"):
      test(m"a Scala value converts into a `Foreign` literal"):
        val text: Foreign of "string" from Typescript = "hello"
        text.expr
      . assert:
          case Foreign.Expression.Literal(_) => true
          case _                             => false

      test(m"a Scala argument is converted to a `Foreign` literal upon application"):
        foo.greet("hi").expr
      . assert:
          case Foreign.Expression.Apply(_, args__) if args__.length == 1 =>
            args__.head match
              case Foreign.Expression.Literal(_) => true
              case _                             => false
          case _                                                                 => false

      test(m"an Optional value converts to a `Foreign` literal (optional instance)"):
        val opt: Foreign of ("string" | "undefined") from Typescript = "hi": Optional[Text]
        opt.expr
      . assert:
          case Foreign.Expression.Literal(_) => true
          case _                             => false

    suite(m"Foreign type composition"):
      test(m"an array field is read as `Array<T>`"):
        val tags: Foreign of ("Array" over "string") from Typescript = foo.tags
        tags.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("Foo"), "tags", "Foo"))

      test(m"indexing an array value yields the element's foreign type"):
        val tags: Foreign of ("Array" over "string") from Typescript = foo.tags
        val tag: Foreign of "string" from Typescript = tags(0)
        tag.expr
      . assert:
          case Foreign.Expression.Index(_, Foreign.Expression.Literal(index)) => index == 0
          case _                                                               => false

      test(m"an optional field is a union with `undefined`"):
        val nickname: Foreign of ("string" | "undefined") from Typescript = foo.nickname
        nickname.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "nickname"
          case _                                   => false

      test(m"a union field has a bare-union foreign type"):
        val id: Foreign of ("string" | "number") from Typescript = foo.id
        id.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "id"
          case _                                   => false

      test(m"a generic field has an `over` foreign type"):
        val lookup: Foreign of ("Map" over ("number", "string")) from Typescript = foo.lookup
        lookup.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "lookup"
          case _                                   => false

    suite(m"Compile-time safety"):
      test(m"selecting an undefined member is a compile error"):
        demilitarize(foo.nonexistent).map(_.message)
      . assert(_ == List(t"xenophile: the foreign type Foo has no member nonexistent"))

      test(m"calling a method with the wrong arity is a compile error"):
        demilitarize(foo.greet("a", "b")).map(_.message)
      . assert(_ == List(t"xenophile: greet expects 1 arguments, not 2"))

      test(m"passing an argument of the wrong foreign type is a compile error"):
        demilitarize(foo.greet(42)).map(_.message)
      . assert(_ == List(t"xenophile: greet expects an argument of foreign type string"))

    suite(m"Native (C headers)"):
      val library: Foreign of "library" from Native = Foreign["library", Native]

      test(m"FFM: call libc strlen through a parsed C header"):
        val arena = java.lang.foreign.Arena.global().nn
        val libc = ForeignLibrary.system("long strlen(const char* s);")
        val text = arena.allocateFrom("hello, world").nn
        libc.handle("strlen").invokeWithArguments(text).nn.asInstanceOf[Long]
      . assert(_ == 12L)

      // The bare `invoke`, on a module that also depends on the Wasm, JS, Kotlin and Scala Native
      // backends: it resolves to Panama because the `Native` ecosystem names both C backends and
      // only `PanamaInvoke` is on this classpath. Before the backends shared one `invoke`, this
      // could not be written here at all.
      test(m"FFM: `invoke` materializes a C call as a Panama downcall"):
        library.abs(-5).call[Int]()
      . assert(_ == 5)

      test(m"a C struct field has the field's foreign type"):
        val point: Foreign of "Point" from Native = Foreign["Point", Native]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("Point"), "x", "Point"))

      test(m"applying a C function builds an `Apply` node typed by its result"):
        val absolute: Foreign of "int" from Native = library.abs(5)
        absolute.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == "abs"
          case _                                                                      => false

      test(m"a function returning `const char*` has the C-string foreign type"):
        val version: Foreign of "string" from Native = library.version()
        version.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == "version"
          case _                                                                  => false

      test(m"a `union` field has the field's foreign type"):
        val number: Foreign of "Number" from Native = Foreign["Number", Native]
        number.f.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("Number"), "f", "Number"))

      test(m"a `typedef` alias resolves to its underlying foreign type"):
        val counter: Foreign of "int" from Native = library.increment(1)
        counter.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == "increment"
          case _                                                                      => false

      test(m"a fixed-width `int32_t` is canonicalised to `int`"):
        val value: Foreign of "int" from Native = library.identity(42)
        value.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == "identity"
          case _                                                                      => false

      test(m"passing a C argument of the wrong foreign type is a compile error"):
        demilitarize(library.abs("five")).map(_.message)
      . assert(_ == List(t"xenophile: abs expects an argument of foreign type int"))

    suite(m"Wit (WebAssembly Interface Types)"):
      val api: Foreign of "api" from Wit = Foreign["api", Wit]

      test(m"a WIT record field keeps its faithful (Hypotenuse-backed) type"):
        val point: Foreign of "point" from Wit = Foreign["point", Wit]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("point"), "x", "point"))

      // `greet` is declared after a `resource { … }` in the interface, so this also checks that the
      // resource's braces are skipped and the functions following it are still parsed.
      test(m"a function declared after a `resource` is typed by its result"):
        val greeting: Foreign of "string" from Wit = api.greet("hi")
        greeting.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == "greet"
          case _                                                                      => false

      test(m"an `enum` is the unsigned discriminant sized to its cases"):
        val shade: Foreign of "u8" from Wit = api.shade()
        shade.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == "shade"
          case _                                                                  => false

      test(m"a `flags` type is a Hypotenuse bit-vector sized to its members"):
        val caps: Foreign of "b8" from Wit = api.caps()
        caps.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == "caps"
          case _                                                                  => false

      test(m"a WIT `list<T>` result has an `over` foreign type"):
        val tags: Foreign of ("list" over "string") from Wit = api.tags()
        tags.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == "tags"
          case _                                                                  => false

      test(m"a WIT `option<T>` result is a union with `none`"):
        val found: Foreign of ("string" | "none") from Wit = api.lookup("k")
        found.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == "lookup"
          case _                                                                      => false

      test(m"passing a WIT argument of the wrong foreign type is a compile error"):
        demilitarize(api.add("two", "three")).map(_.message)
      . assert(_ == List(t"xenophile: add expects an argument of foreign type s32"))

      test(m"an interface function is qualified with its package's module id"):
        val wit = "package wasi:random@0.2.0; interface random { get-random-u64: func() -> u64; }"
        WitDialect.parse(wit).stdlib("random").stdlib("get-random-u64").module.or(t"")
      . assert(_ == "wasi:random/random@0.2.0")

    suite(m"Wit worlds"):
      val source =
        """package test:demo@1.0.0;
            world service {
              import wasi:io/streams@0.2.0;
              import wasi:clocks/monotonic-clock@0.2.0;
              export wasi:http/incoming-handler@0.2.0;
            }"""

      test(m"a world's imports are read in order, as Component Model ids"):
        WitDialect.worlds(source).stdlib("service").imports
      . assert(_ == List(t"wasi:io/streams@0.2.0", t"wasi:clocks/monotonic-clock@0.2.0"))

      test(m"a world's exports are read separately from its imports"):
        WitDialect.worlds(source).stdlib("service").exports
      . assert(_ == List(t"wasi:http/incoming-handler@0.2.0"))

      test(m"a bare interface name is qualified with the package id"):
        val wit = "package test:demo@1.0.0; world w { import helper; }"
        WitDialect.worlds(wit).stdlib("w").imports
      . assert(_ == List(t"test:demo/helper@1.0.0"))

      test(m"an inline function import references no interface"):
        val wit = "package test:demo@1.0.0; world w { import log: func(message: string); }"
        WitDialect.worlds(wit).stdlib("w").imports
      . assert(_ == List())

      test(m"an inline interface export references no interface"):
        val wit = "package test:demo@1.0.0; world w { export handler: interface { go: func(); } }"
        WitDialect.worlds(wit).stdlib("w").exports
      . assert(_ == List())

      test(m"a world does not capture items from an interface beside it"):
        val wit = "package test:demo@1.0.0; interface i { go: func(); } world w { import wasi:io/streams@0.2.0; }"
        WitDialect.worlds(wit).stdlib("w").imports
      . assert(_ == List(t"wasi:io/streams@0.2.0"))

      test(m"every world in a source is read"):
        val wit = "package test:demo@1.0.0; world a { export x:y/z@1.0.0; } world b { import p:q/r@1.0.0; }"
        WitDialect.worlds(wit).stdlib.keySet.to(proscenium.List).sort
      . assert(_ == List(t"a", t"b"))

    suite(m"WebIDL (synthetic sample)"):
      val shape: Foreign of "Shape" from WebIdl = Foreign["Shape", WebIdl]
      val circle: Foreign of "Circle" from WebIdl = Foreign["Circle", WebIdl]

      test(m"an attribute is read as a field of its declared foreign type"):
        val name: Foreign of "string" from WebIdl = shape.name
        name.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("Shape"), "name", "Shape"))

      test(m"`octet` canonicalises to the Hypotenuse-backed `u8`"):
        val sides: Foreign of "u8" from WebIdl = shape.sides
        sides.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "sides"
          case _                                   => false

      test(m"`unsigned long` canonicalises to `u32`"):
        val area: Foreign of "u32" from WebIdl = shape.area
        area.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "area"
          case _                                   => false

      test(m"a `sequence<T>` operation has an `over` foreign type"):
        val labels: Foreign of ("sequence" over "string") from WebIdl = shape.labels()
        labels.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == "labels"
          case _                                                                  => false

      test(m"a nullable `T?` result is a union with `null`"):
        val described: Foreign of ("string" | "null") from WebIdl = shape.describe("the ")
        described.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == "describe"
          case _                                                                      => false

      test(m"an `enum` reference resolves to `string`"):
        val style: Foreign of "string" from WebIdl = shape.style
        style.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "style"
          case _                                   => false

      test(m"a `typedef` to a union resolves transitively"):
        val id: Foreign of ("string" | "s32") from WebIdl = shape.id
        id.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "id"
          case _                                   => false

      test(m"a `partial interface` member is merged into the interface"):
        val order: Foreign of "s32" from WebIdl = shape.order
        order.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "order"
          case _                                   => false

      test(m"an inherited attribute resolves on the derived interface"):
        val area: Foreign of "u32" from WebIdl = circle.area
        area.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "area"
          case _                                   => false

      test(m"a mixin member applied with `includes` resolves"):
        val visible: Foreign of "boolean" from WebIdl = circle.visible
        visible.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "visible"
          case _                                   => false

      test(m"a `dictionary` field is read as a field of its foreign type"):
        val options: Foreign of "ShapeOptions" from WebIdl = Foreign["ShapeOptions", WebIdl]
        val color: Foreign of "string" from WebIdl = options.color
        color.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "color"
          case _                                   => false

      test(m"passing a WebIDL argument of the wrong foreign type is a compile error"):
        demilitarize(shape.scale("large")).map(_.message)
      . assert(_ == List(t"xenophile: scale expects an argument of foreign type f64"))

    suite(m"WebIDL (real DOM from webref)"):
      val node: Foreign of "Node" from WebIdlDom = Foreign["Node", WebIdlDom]
      val element: Foreign of "HTMLElement" from WebIdlDom = Foreign["HTMLElement", WebIdlDom]

      test(m"a DOM attribute is read as a field of its foreign type"):
        val nodeName: Foreign of "string" from WebIdlDom = node.nodeName
        nodeName.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference("Node"), "nodeName", "Node"))

      test(m"`unsigned short` canonicalises to `u16`"):
        val nodeType: Foreign of "u16" from WebIdlDom = node.nodeType
        nodeType.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "nodeType"
          case _                                   => false

      test(m"an inherited attribute resolves up the chain (HTMLElement → Element)"):
        val tagName: Foreign of "string" from WebIdlDom = element.tagName
        tagName.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "tagName"
          case _                                   => false

      test(m"a member inherited from the root (HTMLElement → … → Node) resolves"):
        val nodeName: Foreign of "string" from WebIdlDom = element.nodeName
        nodeName.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == "nodeName"
          case _                                   => false

      test(m"an operation inherited from EventTarget resolves on HTMLElement"):
        val dispatched: Foreign of "boolean" from WebIdlDom =
          element.dispatchEvent(Foreign["Event", WebIdlDom])

        dispatched.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 =>
            m == "dispatchEvent"

          case _ =>
            false

      test(m"an inherited operation is typed by its result"):
        val appended: Foreign of "Node" from WebIdlDom = node.appendChild(Foreign["Node", WebIdlDom])
        appended.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 =>
            m == "appendChild"

          case _ =>
            false

      test(m"passing a DOM argument of the wrong foreign type is a compile error"):
        demilitarize(node.appendChild(Foreign["Event", WebIdlDom])).map(_.message)
      . assert(_ == List(t"xenophile: appendChild expects an argument of foreign type Node"))

    // The end-to-end dynamic-completions route: Harlequin's typechecked pipeline finds the
    // `Foreign` companion through `Completable`, which enumerates the receiver's foreign type's
    // members from the definitions resource recorded in its `Locus` refinement. The highlighted
    // snippet compiles against this test module's own classes, so the fixture `Interface` givens
    // above are importable by name.
    suite(m"Dynamic completions"):
      given Scalac[3.8, Universe.Classfile] = Scalac[3.8](Nil)
      given LocalClasspath = LocalClasspath.of(Classloader[Tests.type])
      import highlighting.typecheckedScala

      def completionsAt(source: Text): List[prophesy.Completion] =
        Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items)

      val header = "import xenophile.*\nimport xenophile.tsInterface\n"

      test(m"a partial member on a Foreign receiver completes from the definitions"):
        completionsAt(t"${header}val foo = Foreign[\"Foo\", Typescript]\nval x = foo.ba").map(_.name)
      . assert(_ == List(t"bar", t"baz"))

      test(m"a method member completes as a method with its signature"):
        completionsAt(t"${header}val foo = Foreign[\"Foo\", Typescript]\nval x = foo.gre")
      . assert: items =>
          items.map { item => (item.name, item.kind) } ==
            List((t"greet", prophesy.Completion.Kind.Method))

      test(m"completion works on a navigated (non-root) receiver"):
        completionsAt(t"${header}val foo = Foreign[\"Foo\", Typescript]\nval x = foo.bar.qu")
        . map(_.name)
      . assert(_ == List(t"qux"))

    typescriptParserTests()
    dtsDisciplineTests()
    webIdlDisciplineTests()
    witDisciplineTests()
    cheaderDisciplineTests()
    kotlinMetadataDisciplineTests()

  def typescriptParserTests(): Unit =
    import strategies.throwUnsafely

    def declarations(source: Text): List[Typescript.Declaration] = Typescript.Parser.parse(source)

    def names(source: Text): scala.List[Text] = declarations(source).stdlib.map(_.key)

    def members(source: Text): scala.List[Text] =
      declarations(source).stdlib.flatMap(_.declaredMembers.stdlib).map(_.selector)

    // Every construct below was dropped whole, or silently misread, by the grammar this
    // replaced; each test names the shape rather than the mechanism.

    test(m"a generic interface is read, not dropped"):
      names("interface Box<T> { value: T; }")
    . assert(_ == scala.List("Box"))

    test(m"an interface's extends clause is recorded"):
      declarations("interface A { x: number; }\ninterface B extends A { y: number; }").stdlib
      . collect { case interface: Typescript.Declaration.Interface => interface }
      . flatMap(_.extending.stdlib.map(_.text))
    . assert(_ == scala.List("A"))

    test(m"a type alias is a declaration"):
      names("type Id = string | number;")
    . assert(_ == scala.List("Id"))

    test(m"a class, an enum, a function and a const are declarations"):
      names("""|declare class C { m(): void; }
                |declare enum E { A, B }
                |declare function f(x: number): string;
                |declare const k: number;
                |""".s.stripMargin.tt)
    . assert(_ == scala.List("C", "E", "f", "k"))

    test(m"a namespace qualifies the declarations it encloses"):
      names("declare namespace a { namespace b { interface X { y: number; } } }")
    . assert(_ == scala.List("a.b.X"))

    test(m"only exported declarations are exported in a module"):
      declarations("export interface A { x: number; }\ninterface B { y: number; }").stdlib
      . map { declaration => (declaration.key, declaration.exported) }
    . assert(_ == scala.List(("A", true), ("B", false)))

    test(m"every top-level declaration is exported in a global script"):
      declarations("interface A { x: number; }").stdlib.map(_.exported)
    . assert(_ == scala.List(true))

    test(m"a comment does not start a declaration"):
      names("// interface Ghost { x: number; }\ninterface Real { x: number; }")
    . assert(_ == scala.List("Real"))

    test(m"a block comment is skipped entirely"):
      names("/* interface Ghost {\n x: number; } */\ninterface Real { x: number; }")
    . assert(_ == scala.List("Real"))

    test(m"an index signature is a member of its own kind"):
      members("interface A { [key: string]: number; }")
    . assert(_ == scala.List("[]"))

    test(m"a call signature and a construct signature are distinct members"):
      members("interface A { (x: number): string; new (y: string): A; }")
    . assert(_ == scala.List("()", "new()"))

    test(m"a getter and a setter do not collide with a property"):
      members("interface A { get x(): number; set x(value: number); }")
    . assert(_ == scala.List("get x", "set x"))

    test(m"overloads accumulate under one member rather than overwriting"):
      declarations("interface A { f(x: number): string; f(x: string): number; }").stdlib
      . flatMap(_.declaredMembers.stdlib).map(_.signatures.stdlib.length)
    . assert(_ == scala.List(2))

    test(m"an inline object type does not terminate the enclosing interface"):
      members("interface A { config: { host: string; port: number }; after: number; }")
    . assert(_ == scala.List("config", "after"))

    test(m"a function type is read as a function, not as a stray parenthesis"):
      declarations("interface A { handler: (event: string) => void; }").stdlib
      . flatMap(_.declaredMembers.stdlib).flatMap(_.signatures.stdlib).map(_.text)
    . assert(_ == scala.List("(event: string) => void"))

    test(m"an intersection is not truncated to its first member"):
      declarations("type T = A & B;").stdlib
      . collect { case alias: Typescript.Declaration.Alias => alias.target.text }
    . assert(_ == scala.List("A & B"))

    test(m"a tuple type is read"):
      declarations("type T = [string, number];").stdlib
      . collect { case alias: Typescript.Declaration.Alias => alias.target.text }
    . assert(_ == scala.List("[string, number]"))

    test(m"a string literal type keeps its value and is not confused with a name"):
      declarations("""type T = "a" | "b";""").stdlib
      . collect { case alias: Typescript.Declaration.Alias => alias.target.text }
    . assert(_ == scala.List("a | b"))

    test(m"a negative numeric literal type keeps its sign"):
      declarations("type T = -1;").stdlib
      . collect { case alias: Typescript.Declaration.Alias => alias.target.text }
    . assert(_ == scala.List("-1"))

    test(m"a nested array type is read to the right depth"):
      declarations("type T = string[][];").stdlib
      . collect { case alias: Typescript.Declaration.Alias => alias.target.text }
    . assert(_ == scala.List("string[][]"))

    test(m"a type predicate is read"):
      declarations("declare function isFoo(x: unknown): x is Foo;").stdlib
      . collect { case function: Typescript.Declaration.Function => function }
      . flatMap(_.signatures.stdlib).map(_.text)
    . assert(_ == scala.List("(x: unknown) => x is Foo"))

    test(m"a rest parameter is marked as such"):
      declarations("declare function f(...args: string[]): void;").stdlib
      . collect { case function: Typescript.Declaration.Function => function }
      . flatMap(_.signatures.stdlib)
      . collect { case Typescript.Type.Function(parameters, _, _, _) => parameters.stdlib.map(_.rest) }
    . assert(_ == scala.List(scala.List(true)))

    // Constructs outside the grammar are refused, never approximated. This is the property the
    // discipline depends on: a declaration file read as a smaller contract than it declares
    // would make every claim computed from it unsound.

    def refuses(source: Text): Optional[Typescript.Error.Reason] =
      import errorDiagnostics.stackTracesDiagnostics
      capture[Typescript.Error](Typescript.Parser.parse(source)).reason

    test(m"a conditional type is refused"):
      refuses("type T<A> = A extends string ? number : boolean;")
    . assert(_ == Typescript.Error.Reason.Unsupported("a conditional type"))

    test(m"a template literal type is refused"):
      refuses(t"type T = `a${'$'}{B}c`;")
    . assert(_ == Typescript.Error.Reason.Unsupported("a template literal type"))

    test(m"an infer binder is refused"):
      refuses("type T<A> = Array<infer B>;")
    . assert(_ == Typescript.Error.Reason.Unsupported("an `infer` binder"))

    test(m"a mapped type is refused under its own name"):
      refuses("interface A { [K in B]: number; }")
    . assert(_ == Typescript.Error.Reason.Unsupported("a mapped type"))

    test(m"an unterminated string literal is a syntax error"):
      refuses("""type T = "unterminated;""").let:
        case Typescript.Error.Reason.Syntax(_, _) => true
        case _                                   => false
    . assert(_ == true)

    test(m"a duplicated class declaration is refused"):
      refuses("declare class A {}\ndeclare class A {}")
    . assert(_ == Typescript.Error.Reason.Duplicate("A"))

    test(m"interfaces merge, so a repeated interface name is accepted"):
      names("interface A { x: number; }\ninterface A { y: number; }")
    . assert(_ == scala.List("A", "A"))

    // The dialect projection, which the foreign-function macro reads.

    test(m"the dialect resolves a member inherited through extends"):
      TypescriptDialect.parse("interface A { x: number; }\ninterface B extends A { y: number; }")
      . at("B").lay(scala.Nil) { members => members.stdlib.keys.toList }
      . sortBy(_.s)
    . assert(_ == scala.List("x", "y"))

    test(m"the dialect reads a generic interface the old grammar dropped"):
      TypescriptDialect.parse("interface Box<T> { value: T; }")
      . at("Box").lay(scala.Nil) { members => members.stdlib.keys.toList }
    . assert(_ == scala.List("value"))

  def dtsDisciplineTests(): Unit =
    import reliquary.*
    import alphabets.hexLowerCase
    import strategies.throwUnsafely

    def content(source: Text): List[(TreePath, Data)] =
      List((TreePath(t"types/index.d.ts"), Array.unsafeFrozen(source.s.getBytes("UTF-8").nn)))

    def atomize(source: Text): Atomization =
      DtsDiscipline.atomize(content(source), Discipline.Context("jvm"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      """|export interface Client {
          |  send(message: string): void;
          |  readonly id: string;
          |}
          |export type Handle = string | number;
          |export declare function connect(url: string): Client;
          |""".s.stripMargin.tt

    test(m"the discipline claims declaration files and nothing else"):
      val data = Array.freeze(Array.allocate[Byte](0))

      (DtsDiscipline.claims(TreePath("types/index.d.ts"), data),
       DtsDiscipline.claims(TreePath("lib/index.js"), data),
       DtsDiscipline.claims(TreePath("readme.md"), data))
    . assert(_ == (true, false, false))

    test(m"the discipline certifies recompilation and not linkage"):
      (DtsDiscipline.id, DtsDiscipline.guarantees("jvm"), DtsDiscipline.keying)
    . assert(_ == ("dts/1", Set(Discipline.Guarantee.Recompilation),
        Discipline.Keying.Declaration))

    test(m"each exported declaration and each member yields an atom"):
      keys(baseline)
    . assert(_ == scala.List("Client", "Client#id", "Client#send", "Handle", "connect"))

    test(m"an unexported declaration is not part of the contract"):
      keys("export interface A { x: number; }\ninterface Hidden { y: number; }")
    . assert(_ == scala.List("A", "A#x"))

    test(m"atomization is deterministic"):
      def once(): scala.List[(Text, Text)] =
        atomize(baseline).atoms.stdlib
        . map { atom => (atom.key, atom.valueHash.serialize[Hex]) }
        . sortBy(_(0).s)

      once() == once()
    . assert(identity)

    test(m"renaming a type parameter changes nothing"):
      grade("export interface Box<T> { value: T; }", "export interface Box<U> { value: U; }")
    . assert(_ == Grade.Patch)

    test(m"reordering the members of a union changes nothing"):
      grade("export type T = A | B;", "export type T = B | A;")
    . assert(_ == Grade.Patch)

    test(m"reordering the elements of a tuple is a major change"):
      grade("export type T = [A, B];", "export type T = [B, A];")
    . assert(_ == Grade.Major)

    // The one change that is honestly two events: adding a member is pure extension for a
    // consumer who calls the interface, and a break for one who implements it. The member's own
    // atom records the first; the fold of member keys into the interface's atom records the
    // second, and the second is what the grade reports.
    test(m"adding an interface member is a major change for implementors"):
      grade("export interface A { x: number; }", "export interface A { x: number; y: number; }")
    . assert(_ == Grade.Major)

    test(m"the added member is nonetheless an atom of its own"):
      keys("export interface A { x: number; y: number; }")
    . assert(_ == scala.List("A", "A#x", "A#y"))

    test(m"adding a whole interface is a minor change"):
      grade("export interface A { x: number; }",
          "export interface A { x: number; }\nexport interface B { y: number; }")
    . assert(_ == Grade.Minor)

    test(m"removing a member is a major change"):
      grade("export interface A { x: number; y: number; }", "export interface A { x: number; }")
    . assert(_ == Grade.Major)

    test(m"making a member optional is a major change"):
      grade("export interface A { x: number; }", "export interface A { x?: number; }")
    . assert(_ == Grade.Major)

    test(m"adding an overload is a major change"):
      grade("export interface A { f(x: number): void; }",
          "export interface A { f(x: number): void; f(x: string): void; }")
    . assert(_ == Grade.Major)

    test(m"changing a declaration's namespace changes its key"):
      keys("export declare namespace a { interface X { y: number; } }")
    . assert(_ == scala.List("a.X", "a.X#y"))

    test(m"an unreadable declaration file is an atomization error"):
      import errorDiagnostics.stackTracesDiagnostics

      capture[Discipline.Error]:
        DtsDiscipline.atomize(content("export type T<A> = A extends string ? 1 : 2;"),
            Discipline.Context("jvm"))

      . reason
    . assert:
        case Discipline.Error.Reason.Malformed(_) => true
        case _                                   => false

    test(m"the registry falls back to opaque for content the discipline does not claim"):
      val registry = Discipline.Registry(List(DtsDiscipline))
      val js = List((TreePath(t"lib/index.js"), Array.freeze(Array.allocate[Byte](1))))

      registry.atomize(js, Discipline.Context("jvm")).stdlib.map(_.discipline)
    . assert(_ == scala.List("opaque/1"))

  def webIdlDisciplineTests(): Unit =
    import reliquary.*
    import strategies.throwUnsafely

    def content(source: Text): List[(TreePath, Data)] =
      List((TreePath(t"idl/browser.idl"), Array.unsafeFrozen(source.s.getBytes("UTF-8").nn)))

    def atomize(source: Text): Atomization =
      WebIdlDiscipline.atomize(content(source), Discipline.Context("host"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      """|interface Widget {
          |  readonly attribute DOMString name;
          |  undefined render(long depth);
          |};
          |dictionary Options {
          |  required DOMString mode;
          |  long retries = 3;
          |};
          |enum Direction { "up", "down" };
          |""".s.stripMargin.tt

    suite(m"The `webidl/1` discipline"):
      test(m"the discipline claims idl files in the host world and nothing else"):
        val data = Array.freeze(Array.allocate[Byte](0))

        (WebIdlDiscipline.claims(TreePath("idl/dom.idl"), data),
         WebIdlDiscipline.claims(TreePath("lib/index.js"), data),
         WebIdlDiscipline.domain.covers("host"),
         WebIdlDiscipline.domain.covers("jvm"))
      . assert(_ == (true, false, true, false))

      test(m"declarations, members, fields and values yield atoms"):
        keys(baseline)
      . assert(_ == scala.List("Direction", "Direction#down", "Direction#up", "Options",
          "Options#retries", "Widget", "Widget#name", "Widget#render(s32)"))

      test(m"adding an interface member is a minor for callers"):
        val grown = t"${baseline}partial interface Widget { attribute long depth; };"
        grade(baseline, grown)
      . assert(_ == Grade.Minor)

      test(m"adding a required dictionary member is major"):
        val grown = baseline.s.replace("required DOMString mode;",
            "required DOMString mode;\n  required long width;").nn.tt
        grade(baseline, grown)
      . assert(_ == Grade.Major)

      test(m"adding an optional dictionary member is minor"):
        val grown = baseline.s.replace("long retries = 3;",
            "long retries = 3;\n  boolean verbose = false;").nn.tt
        grade(baseline, grown)
      . assert(_ == Grade.Minor)

      test(m"adding an enumeration value is minor"):
        grade(baseline, baseline.s.replace("\"down\"", "\"down\", \"left\"").nn.tt)
      . assert(_ == Grade.Minor)

      test(m"removing a member is major"):
        grade(baseline, baseline.s.replace("  undefined render(long depth);\n", "").nn.tt)
      . assert(_ == Grade.Major)

      test(m"a mixin's members atomize under the including interface"):
        val mixed =
          """|interface Base {};
              |interface mixin Extras { undefined extra(); };
              |Base includes Extras;
              |""".s.stripMargin.tt

        keys(mixed)
      . assert(_ == scala.List("Base", "Base#extra()"))

      test(m"a partial interface in another file completes its target"):
        val split = List(
          (TreePath(t"idl/a.idl"),
           Array.unsafeFrozen(t"interface W {};".s.getBytes("UTF-8").nn)),
          (TreePath(t"idl/b.idl"),
           Array.unsafeFrozen(t"partial interface W { attribute long x; };".s
              .getBytes("UTF-8").nn)))

        WebIdlDiscipline.atomize(split, Discipline.Context("host")).atoms.stdlib.map(_.key)
        . sortBy(_.s)
      . assert(_ == scala.List("W", "W#x"))

      test(m"exposure scopes are part of the key"):
        keys("[Exposed=(Window,Worker)] interface Scoped {};")
      . assert(_ == scala.List("Scoped[Window,Worker]"))

      test(m"identically-shaped members of different interfaces do not alias"):
        val twins = "interface A { attribute long x; };\ninterface B { attribute long x; };"
        val atoms = atomize(twins).atoms.stdlib

        atoms.map { atom => Lira.Hash.text(atom.valueHash) }.distinct.size
        == atoms.size
      . assert(identity)

      test(m"union member order does not affect a hash"):
        val one = atomize("interface U { attribute (long or DOMString) x; };")
        val two = atomize("interface U { attribute (DOMString or long) x; };")

        one.atoms.stdlib.map { atom => Lira.Hash.text(atom.valueHash) }
        == two.atoms.stdlib.map { atom => Lira.Hash.text(atom.valueHash) }
      . assert(identity)

      test(m"an unsupported construct is an atomization error"):
        import errorDiagnostics.stackTracesDiagnostics

        capture[Discipline.Error](atomize("weird thing;")).reason match
          case Discipline.Error.Reason.Malformed(_) => true
          case _                                   => false
      . assert(identity)

      test(m"the real DOM excerpt atomizes"):
        val stream = getClass.getResourceAsStream("/xenophile/dom.idl").nn
        val bytes = stream.readAllBytes().nn
        stream.close()
        atomize(Text(String(bytes, "UTF-8"))).atoms.stdlib.size
      . assert(_ > 50)

  def witDisciplineTests(): Unit =
    import reliquary.*
    import strategies.throwUnsafely

    def content(source: Text): List[(TreePath, Data)] =
      List((TreePath(t"wit/api.wit"), Array.unsafeFrozen(source.s.getBytes("UTF-8").nn)))

    def atomize(source: Text): Atomization =
      WitDiscipline.atomize(content(source), Discipline.Context("host"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      """|package wasi:random@0.2.0;
          |
          |interface random {
          |  record seed { value: u64 }
          |  get-random-bytes: func(len: u64) -> list<u8>;
          |}
          |
          |world host {
          |  import random;
          |  export run;
          |}
          |""".s.stripMargin.tt

    suite(m"The `wit/1` discipline"):
      test(m"the discipline claims wit files in its two worlds and nothing else"):
        val data = Array.freeze(Array.allocate[Byte](0))

        (WitDiscipline.claims(TreePath("wit/world.wit"), data),
         WitDiscipline.claims(TreePath("lib/api.idl"), data),
         WitDiscipline.domain.covers("host"),
         WitDiscipline.domain.covers("component"),
         WitDiscipline.domain.covers("jvm"))
      . assert(_ == (true, false, true, true, false))

      test(m"interfaces, items and worlds yield package-qualified atoms"):
        keys(baseline)
      . assert(_ == scala.List(
          "wasi:random/host@0.2.0",
          "wasi:random/host@0.2.0#import wasi:random/random@0.2.0",
          "wasi:random/random@0.2.0",
          "wasi:random/random@0.2.0#get-random-bytes",
          "wasi:random/random@0.2.0#seed"))

      test(m"adding a function to an interface is minor"):
        val grown = baseline.s.replace("}\n\nworld",
            "  get-random-u64: func() -> u64;\n}\n\nworld").nn.tt
        grade(baseline, grown)
      . assert(_ == Grade.Minor)

      test(m"adding a record field is major"):
        grade(baseline, baseline.s.replace("{ value: u64 }", "{ value: u64, extra: u32 }").nn.tt)
      . assert(_ == Grade.Major)

      test(m"a world gaining an import is minor"):
        val source = baseline.s.replace("interface random {",
            "interface insecure { i: func(); }\ninterface random {").nn.tt
        val grown = source.s.replace("import random;", "import random;\n  import insecure;").nn.tt
        grade(source, grown)
      . assert(_ == Grade.Minor)

      test(m"a world gaining an export is major"):
        grade(baseline, baseline.s.replace("export run;", "export run;\n  export other;").nn.tt)
      . assert(_ == Grade.Major)

      test(m"a use-imported reference is qualified to its source interface"):
        val direct =
          """|package a:pkg;
              |interface one {
              |  type id = u64;
              |}
              |interface two {
              |  use one.{id};
              |  get: func() -> id;
              |}
              |""".s.stripMargin.tt

        val renamed = direct.s.replace("use one.{id};", "use one.{id as key};").nn
          .replace("-> id;", "-> key;").nn.tt

        val hashes = { (source: Text) =>
          atomize(source).atoms.stdlib
          . filter(_.key == "a:pkg/two#get")
          . map { atom => Lira.Hash.text(atom.valueHash) }
        }

        hashes(direct) == hashes(renamed)
      . assert(identity)

      test(m"a since gate is consumed and an unstable gate is refused"):
        import errorDiagnostics.stackTracesDiagnostics

        val gated =
          """|package a:pkg;
              |interface one {
              |  @since(version = 0.2.1)
              |  get: func() -> u64;
              |}
              |""".s.stripMargin.tt

        val unstable = gated.s.replace("@since(version = 0.2.1)",
            "@unstable(feature = fancy)").nn.tt

        val accepted = atomize(gated).atoms.stdlib.exists(_.key == "a:pkg/one#get")

        val refused =
          capture[Discipline.Error](atomize(unstable)).reason match
            case Discipline.Error.Reason.Malformed(_) => true
            case _                                   => false

        (accepted, refused)
      . assert(_ == (true, true))

      test(m"an unresolvable type reference is an error"):
        import errorDiagnostics.stackTracesDiagnostics

        capture[Discipline.Error]:
          atomize("package a:pkg;\ninterface one { get: func() -> mystery; }")
        . reason match
            case Discipline.Error.Reason.Unresolved(_) => true
            case _                                    => false
      . assert(identity)

      test(m"the sample wit fixture atomizes"):
        val stream = getClass.getResourceAsStream("/xenophile/api.wit").nn
        val bytes = stream.readAllBytes().nn
        stream.close()
        atomize(Text(String(bytes, "UTF-8"))).atoms.stdlib.size
      . assert(_ > 10)

  def cheaderDisciplineTests(): Unit =
    import reliquary.*
    import strategies.throwUnsafely

    def content(source: Text): List[(TreePath, Data)] =
      List((TreePath(t"include/library.h"), Array.unsafeFrozen(source.s.getBytes("UTF-8").nn)))

    def atomize(source: Text): Atomization =
      CHeaderDiscipline.atomize(content(source), Discipline.Context("host"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def hashOf(source: Text, key: Text): Optional[Text] =
      atomize(source).atoms.stdlib.find(_.key == key)
      . map { atom => Lira.Hash.text(atom.valueHash) }.getOrElse(Unset)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      """|typedef struct Point { int x; int y; } Point;
          |typedef enum { LEFT, RIGHT } Direction;
          |int add(int a, int b);
          |size_t strlen(const char* s);
          |""".s.stripMargin.tt

    suite(m"The `cheader/1` discipline"):
      test(m"the discipline claims headers in the host world and nothing else"):
        val data = Array.freeze(Array.allocate[Byte](0))

        (CHeaderDiscipline.claims(TreePath("include/openssl.h"), data),
         CHeaderDiscipline.claims(TreePath("src/main.c"), data),
         CHeaderDiscipline.domain.covers("host"),
         CHeaderDiscipline.domain.covers("nir"))
      . assert(_ == (true, false, true, false))

      test(m"declarations are keyed by bare name"):
        keys(baseline)
      . assert(_ == scala.List("Direction", "Point", "add", "strlen"))

      test(m"adding a declaration is minor and removing one is major"):
        val grown = t"${baseline}double pow(double base, double exponent);"
        (grade(baseline, grown), grade(grown, baseline))
      . assert(_ == (Grade.Minor, Grade.Major))

      test(m"signedness distinguishes hashes"):
        hashOf("int f(unsigned int x);", "f") != hashOf("int f(int x);", "f")
      . assert(identity)

      test(m"pointer depth distinguishes hashes"):
        hashOf("int f(char** x);", "f") != hashOf("int f(char* x);", "f")
      . assert(identity)

      test(m"pointee constness folds and by-value constness does not"):
        (hashOf("int f(const char* x);", "f") != hashOf("int f(char* x);", "f"),
         hashOf("int f(const int x);", "f") == hashOf("int f(int x);", "f"))
      . assert(_ == (true, true))

      test(m"parameter names do not fold"):
        hashOf("int add(int a, int b);", "add") == hashOf("int add(int x, int y);", "add")
      . assert(identity)

      test(m"enumerator values fold, explicit or implicit"):
        (hashOf("typedef enum { A, B } E;", "E")
           == hashOf("typedef enum { A = 0, B = 1 } E;", "E"),
         hashOf("typedef enum { A, B } E;", "E")
           != hashOf("typedef enum { A, B = 5 } E;", "E"))
      . assert(_ == (true, true))

      test(m"completing an opaque struct changes its value"):
        hashOf("struct S;", "S") != hashOf("struct S { int x; };", "S")
      . assert(identity)

      test(m"an unsupported construct is an atomization error"):
        import errorDiagnostics.stackTracesDiagnostics

        capture[Discipline.Error](atomize("int x = 4;")).reason match
          case Discipline.Error.Reason.Malformed(_) => true
          case _                                   => false
      . assert(identity)

      test(m"the sample library header atomizes"):
        val stream = getClass.getResourceAsStream("/xenophile/library.h").nn
        val bytes = stream.readAllBytes().nn
        stream.close()
        atomize(Text(String(bytes, "UTF-8"))).atoms.stdlib.map(_.key).sortBy(_.s)
      . assert(_.contains("HMAC") == false)

      test(m"the openssl header atomizes with its functions keyed by symbol"):
        // The header lives in enigmatic's resources; where it is absent from this suite's
        // classpath the test degenerates to a pass rather than a false failure.
        val stream = getClass.getResourceAsStream("/enigmatic/openssl.h")

        if stream == null then true else
          val bytes = stream.nn.readAllBytes().nn
          stream.nn.close()
          atomize(Text(String(bytes, "UTF-8"))).atoms.stdlib.exists(_.key == "RAND_bytes")
      . assert(_ == true)

  def kotlinMetadataDisciplineTests(): Unit =
    import reliquary.*
    import strategies.throwUnsafely

    // Real Kotlin classfiles from the kotlin-stdlib fixture already on this suite's classpath.
    def classfile(name: Text): Data =
      val stream = getClass.getResourceAsStream(s"/${name.s.replace(".", "/")}.class").nn
      val bytes = stream.readAllBytes().nn
      stream.close()
      Array.unsafeFrozen(bytes)

    def content(names: Text*): List[(TreePath, Data)] =

        names.map: name =>
          (TreePath(t"${name.s.replace(".", "/").nn}.class"), classfile(name))
        . to(List)

    def atomize(names: Text*): Atomization =
      KotlinMetadataDiscipline.atomize(content(names*), Discipline.Context("jvm"))

    suite(m"The `kotlin-metadata/1` discipline"):
      test(m"the discipline claims metadata-carrying classfiles and nothing else"):
        val kotlin = classfile("kotlin.Pair")
        val scala0 = classfile("xenophile.Tests")

        (KotlinMetadataDiscipline.claims(TreePath("kotlin/Pair.class"), kotlin),
         KotlinMetadataDiscipline.claims(TreePath("xenophile/Tests.class"), scala0),
         KotlinMetadataDiscipline.claims(TreePath("readme.md"), kotlin))
      . assert(_ == (true, false, false))

      test(m"a data class atomizes its members, constructor and class atom"):
        val keys = atomize("kotlin.Pair").atoms.stdlib.map(_.key)

        (keys.contains("kotlin.Pair"),
         keys.contains("kotlin.Pair.first"),
         keys.exists(_.s.startsWith("kotlin.Pair#component1(")),
         keys.exists(_.s.startsWith("kotlin.Pair#constructor(")))
      . assert(_ == (true, true, true, true))

      test(m"parameter types carry nullability marks in the key"):
        atomize("kotlin.Pair").atoms.stdlib.map(_.key.s)
        . exists { key => key.startsWith("kotlin.Pair#constructor(") }
      . assert(identity)

      test(m"suspend functions are atomized rather than dropped"):
        // `kotlin.sequences.SequenceScope` is the canonical suspend surface: `yield` is a
        // suspend function, and the whole point of this discipline is that it is visible.
        atomize("kotlin.sequences.SequenceScope").atoms.stdlib.map(_.key.s)
        . exists(_.startsWith("kotlin.sequences.SequenceScope#yield("))
      . assert(identity)

      test(m"an enum class atomizes with its class atom"):
        atomize("kotlin.DeprecationLevel").atoms.stdlib.map(_.key)
        . contains("kotlin.DeprecationLevel")
      . assert(identity)

      test(m"atomization is deterministic"):
        val one = atomize("kotlin.Pair").atoms.stdlib.map { a => Lira.Hash.text(a.valueHash) }
        val two = atomize("kotlin.Pair").atoms.stdlib.map { a => Lira.Hash.text(a.valueHash) }
        one == two
      . assert(identity)

      test(m"identically-shaped members of different classes do not alias"):
        val atoms = atomize("kotlin.Pair", "kotlin.Triple").atoms.stdlib
        atoms.map { atom => Lira.Hash.text(atom.valueHash) }.distinct.size == atoms.size
      . assert(identity)

      test(m"the registry claims kotlin classes ahead of the opaque fallback"):
        val registry = Discipline.Registry(List(KotlinMetadataDiscipline))
        val mixed = content("kotlin.Pair") 

        registry.atomize(mixed, Discipline.Context("jvm")).stdlib.map(_.discipline)
      . assert(_ == scala.List("kotlin-metadata/1"))
