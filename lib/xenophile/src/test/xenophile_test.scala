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

import ambience.systems.javaSystem

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
        . invoke[Int]
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
          . getProgressionLastElement(t"one", 10, 2)
      . assert(_.nonEmpty)

    suite(m"Kotlin facades"):
      val pair = make[kotlin.Pair[Text, Text]](t"a", t"b")

      test(m"a Kotlin class constructs through its facade"):
        pair.k.toString.tt
      . assert(_ == t"(a, b)")

      test(m"a property substitutes the facade's type arguments"):
        val first: Text = pair.first
        first
      . assert(_ == t"a")

      test(m"the second component reads likewise"):
        val second: Text = pair.second
        second
      . assert(_ == t"b")

      val regex = make[kotlin.text.Regex](t"[0-9]+")

      test(m"an instance method accepts Text for a CharSequence parameter"):
        val matches: Boolean = regex.matches(t"123")
        matches
      . assert(_ == true)

      test(m"a non-matching input reports false"):
        val matches: Boolean = regex.matches(t"abc")
        matches
      . assert(_ == false)

      test(m"a nullable result is an Optional, absent on no match"):
        regex.find(t"abc", 0).absent
      . assert(_ == true)

      test(m"a nullable result is present on a match, as a facade"):
        regex.find(t"a1b2", 0).let { result => result.value: Text }
      . assert(_ == t"1")

      test(m"a Kotlin String property reads as Text"):
        val pattern: Text = regex.pattern
        pattern
      . assert(_ == t"[0-9]+")

      test(m"an unknown property is rejected"):
        demilitarize:
          make[kotlin.Pair[Text, Text]](t"a", t"b").third
      . assert(_.nonEmpty)

      test(m"a wrongly-typed constructor argument is rejected"):
        demilitarize:
          make[kotlin.text.Regex](42)
      . assert(_.nonEmpty)

      test(m"a companion object's members are reachable"):
        val escaped: Text = companion[kotlin.text.Regex].escape(t"a.b")
        escaped
      . assert(_ == t"\\Qa.b\\E")

      test(m"an unknown member's error suggests near misses in Kotlin syntax"):
        demilitarize:
          make[kotlin.text.Regex](t"x").matchez(t"y")
        . map(_.message)
      . assert(_.exists(_.contains("did you mean")))

      test(m"a class without a companion object is rejected"):
        demilitarize:
          companion[kotlin.Pair[Text, Text]]
      . assert(_.nonEmpty)

      test(m"a Scala lambda satisfies a Kotlin function-type parameter"):
        val replaced: Text = regex.replace(t"a1b2",
            (m: Facade over kotlin.text.MatchResult) => t"<${m.value}>")

        replaced
      . assert(_ == t"a<1>b<2>")

      test(m"a lambda's facade parameter navigates the Kotlin type inside"):
        val upper: Text = regex.replace(t"3x4", (m: Facade over kotlin.text.MatchResult) =>
            t"${m.value}${m.value}")

        upper
      . assert(_ == t"33x44")

      test(m"a var property accepts assignment through its setter"):
        val parameter = make[kotlin.metadata.KmValueParameter](t"x")
        parameter.name = t"y"
        val name: Text = parameter.name
        name
      . assert(_ == t"y")

      test(m"assignment to a val property is rejected"):
        demilitarize:
          make[kotlin.Pair[Text, Text]](t"a", t"b").first = t"z"
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
        regex.find(t"a7b").let(_.value)
      . assert(_ == t"7")

      test(m"operator get is reachable as apply"):
        regex.find(t"a1b").let(_.groups(0)).let(_.value)
      . assert(_ == t"1")

      // Note `unwrap.toString`: `toString`/`equals`/`hashCode` are real members of the facade
      // wrapper itself, so `Dynamic` cannot intercept them.
      test(m"a plain Java class resolves through the reflection fallback"):
        make[java.lang.StringBuilder](t"ab").reverse().k.toString.tt
      . assert(_ == t"ba")

      test(m"an enum entry is reachable by name, and usable as an argument"):
        val relaxed = make[kotlin.text.Regex]
          ( t"[a-z]+", xenophile.enumEntry[kotlin.text.RegexOption]("IGNORE_CASE") )

        val matches: Boolean = relaxed.matches(t"ABC")
        matches
      . assert(_ == true)

      test(m"an unknown enum entry lists the real ones"):
        demilitarize:
          xenophile.enumEntry[kotlin.text.RegexOption]("IGNORE_CAES")
        . map(_.message)
      . assert(_.exists(_.contains("IGNORE_CASE")))

      test(m"a data class destructures into a typed tuple"):
        make[kotlin.Pair[Text, Text]](t"a", t"b").tuple
      . assert(_ == (t"a", t"b"))

      test(m"a Kotlin list result copies out as a Scala List of Text"):
        val parts: List[Text] = regex.split(t"a1b2").scala
        parts
      . assert(_ == List(t"a", t"b", t""))

      test(m"a named argument selects its declared parameter"):
        regex.find(input = t"a5b").let(_.value)
      . assert(_ == t"5")

      test(m"named arguments reorder to their declared positions"):
        regex.find(startIndex = 2, input = t"1a2b").let(_.value)
      . assert(_ == t"2")

      test(m"an unknown parameter name lists the declared ones"):
        demilitarize:
          regex.find(inpit = t"a5b")
        . map(_.message)
      . assert(_.exists(_.contains("input")))

      test(m"a Scala List argument satisfies a Java collection parameter"):
        val wrapped = make[java.util.ArrayList[Text]](List(t"a", t"b"))
        val size: Int = wrapped.size()
        size
      . assert(_ == 2)

      test(m"surplus arguments collect into a vararg tail"):
        make[java.util.Formatter]().format(t"[%s:%s]", t"x", t"y").k.toString.tt
      . assert(_ == t"[x:y]")

      test(m"a value-class member is rejected with a clear diagnostic"):
        demilitarize:
          companion[kotlin.time.Duration].parse(t"1s")
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
      . assert(_ == t"a")

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
      . assert(_ == t"42")

      test(m"a Java bean getter reads as a short property name"):
        val entry: Text = make[java.util.zip.ZipEntry](t"file.txt").name   // getName() -> .name
        entry
      . assert(_ == t"file.txt")

      test(m"a Java bean setter writes via assignment"):
        val entry = make[java.util.zip.ZipEntry](t"e")
        entry.comment = t"hello"            // setComment(String) via `.comment = …`
        val comment: Text = entry.comment   // getComment() -> .comment
        comment
      . assert(_ == t"hello")

      test(m"a Scala array bridges to a Java array parameter (element conversion)"):
        // The `E[]` constructor wants `CharSequence[]`; a Scala `Array[Text]` bridges to it.
        val strings: scala.Array[Text] = scala.Array(t"a", t"bb")
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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args) if args.length == 1 => m == t"greet"
          case _                                                                      => false

      test(m"a Foreign argument of the declared parameter type is accepted"):
        val linked: Foreign of "Foo" from Typescript = foo.link(foo.bar)
        linked.expr
      . assert:
          case Foreign.Expression.Apply(_, args__) if args__.length == 1 =>
            args__.head match
              case Foreign.Expression.Select(_, b, _) => b == t"bar"
              case _                                  => false
          case _                                                                      => false

    suite(m"Conversion of Scala values to Foreign"):
      test(m"a Scala value converts into a `Foreign` literal"):
        val text: Foreign of "string" from Typescript = t"hello"
        text.expr
      . assert:
          case Foreign.Expression.Literal(_) => true
          case _                             => false

      test(m"a Scala argument is converted to a `Foreign` literal upon application"):
        foo.greet(t"hi").expr
      . assert:
          case Foreign.Expression.Apply(_, args__) if args__.length == 1 =>
            args__.head match
              case Foreign.Expression.Literal(_) => true
              case _                             => false
          case _                                                                 => false

      test(m"an Optional value converts to a `Foreign` literal (optional instance)"):
        val opt: Foreign of ("string" | "undefined") from Typescript = t"hi": Optional[Text]
        opt.expr
      . assert:
          case Foreign.Expression.Literal(_) => true
          case _                             => false

    suite(m"Foreign type composition"):
      test(m"an array field is read as `Array<T>`"):
        val tags: Foreign of ("Array" over "string") from Typescript = foo.tags
        tags.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Foo"), t"tags", t"Foo"))

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
          case Foreign.Expression.Select(_, m, _) => m == t"nickname"
          case _                                   => false

      test(m"a union field has a bare-union foreign type"):
        val id: Foreign of ("string" | "number") from Typescript = foo.id
        id.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"id"
          case _                                   => false

      test(m"a generic field has an `over` foreign type"):
        val lookup: Foreign of ("Map" over ("number", "string")) from Typescript = foo.lookup
        lookup.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"lookup"
          case _                                   => false

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

      test(m"FFM: call libc strlen through a parsed C header"):
        val arena = java.lang.foreign.Arena.global().nn
        val libc = ForeignLibrary.system(t"long strlen(const char* s);")
        val text = arena.allocateFrom("hello, world").nn
        libc.handle(t"strlen").invokeWithArguments(text).nn.asInstanceOf[Long]
      . assert(_ == 12L)

      // The bare `invoke`, on a module that also depends on the Wasm, JS, Kotlin and Scala Native
      // backends: it resolves to Panama because the `Native` ecosystem names both C backends and
      // only `PanamaInvoke` is on this classpath. Before the backends shared one `invoke`, this
      // could not be written here at all.
      test(m"FFM: `invoke` materializes a C call as a Panama downcall"):
        library.abs(-5).invoke[Int]
      . assert(_ == 5)

      test(m"a C struct field has the field's foreign type"):
        val point: Foreign of "Point" from Native = Foreign["Point", Native]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Point"), t"x", t"Point"))

      test(m"applying a C function builds an `Apply` node typed by its result"):
        val absolute: Foreign of "int" from Native = library.abs(5)
        absolute.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == t"abs"
          case _                                                                      => false

      test(m"a function returning `const char*` has the C-string foreign type"):
        val version: Foreign of "string" from Native = library.version()
        version.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == t"version"
          case _                                                                  => false

      test(m"a `union` field has the field's foreign type"):
        val number: Foreign of "Number" from Native = Foreign["Number", Native]
        number.f.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Number"), t"f", t"Number"))

      test(m"a `typedef` alias resolves to its underlying foreign type"):
        val counter: Foreign of "int" from Native = library.increment(1)
        counter.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == t"increment"
          case _                                                                      => false

      test(m"a fixed-width `int32_t` is canonicalised to `int`"):
        val value: Foreign of "int" from Native = library.identity(42)
        value.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == t"identity"
          case _                                                                      => false

      test(m"passing a C argument of the wrong foreign type is a compile error"):
        demilitarize(library.abs(t"five")).map(_.message)
      . assert(_ == List(t"xenophile: abs expects an argument of foreign type int"))

    suite(m"Wit (WebAssembly Interface Types)"):
      val api: Foreign of "api" from Wit = Foreign["api", Wit]

      test(m"a WIT record field keeps its faithful (Hypotenuse-backed) type"):
        val point: Foreign of "point" from Wit = Foreign["point", Wit]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"point"), t"x", t"point"))

      // `greet` is declared after a `resource { … }` in the interface, so this also checks that the
      // resource's braces are skipped and the functions following it are still parsed.
      test(m"a function declared after a `resource` is typed by its result"):
        val greeting: Foreign of "string" from Wit = api.greet(t"hi")
        greeting.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == t"greet"
          case _                                                                      => false

      test(m"an `enum` is the unsigned discriminant sized to its cases"):
        val shade: Foreign of "u8" from Wit = api.shade()
        shade.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == t"shade"
          case _                                                                  => false

      test(m"a `flags` type is a Hypotenuse bit-vector sized to its members"):
        val caps: Foreign of "b8" from Wit = api.caps()
        caps.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == t"caps"
          case _                                                                  => false

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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == t"lookup"
          case _                                                                      => false

      test(m"passing a WIT argument of the wrong foreign type is a compile error"):
        demilitarize(api.add(t"two", t"three")).map(_.message)
      . assert(_ == List(t"xenophile: add expects an argument of foreign type s32"))

      test(m"an interface function is qualified with its package's module id"):
        val wit = t"package wasi:random@0.2.0; interface random { get-random-u64: func() -> u64; }"
        WitDialect.parse(wit).stdlib(t"random").stdlib(t"get-random-u64").module.or(t"")
      . assert(_ == t"wasi:random/random@0.2.0")

    suite(m"Wit worlds"):
      val source =
        t"""package test:demo@1.0.0;
            world service {
              import wasi:io/streams@0.2.0;
              import wasi:clocks/monotonic-clock@0.2.0;
              export wasi:http/incoming-handler@0.2.0;
            }"""

      test(m"a world's imports are read in order, as Component Model ids"):
        WitDialect.worlds(source).stdlib(t"service").imports
      . assert(_ == List(t"wasi:io/streams@0.2.0", t"wasi:clocks/monotonic-clock@0.2.0"))

      test(m"a world's exports are read separately from its imports"):
        WitDialect.worlds(source).stdlib(t"service").exports
      . assert(_ == List(t"wasi:http/incoming-handler@0.2.0"))

      test(m"a bare interface name is qualified with the package id"):
        val wit = t"package test:demo@1.0.0; world w { import helper; }"
        WitDialect.worlds(wit).stdlib(t"w").imports
      . assert(_ == List(t"test:demo/helper@1.0.0"))

      test(m"an inline function import references no interface"):
        val wit = t"package test:demo@1.0.0; world w { import log: func(message: string); }"
        WitDialect.worlds(wit).stdlib(t"w").imports
      . assert(_ == List())

      test(m"an inline interface export references no interface"):
        val wit = t"package test:demo@1.0.0; world w { export handler: interface { go: func(); } }"
        WitDialect.worlds(wit).stdlib(t"w").exports
      . assert(_ == List())

      test(m"a world does not capture items from an interface beside it"):
        val wit = t"package test:demo@1.0.0; interface i { go: func(); } world w { import wasi:io/streams@0.2.0; }"
        WitDialect.worlds(wit).stdlib(t"w").imports
      . assert(_ == List(t"wasi:io/streams@0.2.0"))

      test(m"every world in a source is read"):
        val wit = t"package test:demo@1.0.0; world a { export x:y/z@1.0.0; } world b { import p:q/r@1.0.0; }"
        WitDialect.worlds(wit).stdlib.keySet.toList.sorted
      . assert(_ == List(t"a", t"b"))

    suite(m"WebIDL (synthetic sample)"):
      val shape: Foreign of "Shape" from WebIdl = Foreign["Shape", WebIdl]
      val circle: Foreign of "Circle" from WebIdl = Foreign["Circle", WebIdl]

      test(m"an attribute is read as a field of its declared foreign type"):
        val name: Foreign of "string" from WebIdl = shape.name
        name.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Shape"), t"name", t"Shape"))

      test(m"`octet` canonicalises to the Hypotenuse-backed `u8`"):
        val sides: Foreign of "u8" from WebIdl = shape.sides
        sides.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"sides"
          case _                                   => false

      test(m"`unsigned long` canonicalises to `u32`"):
        val area: Foreign of "u32" from WebIdl = shape.area
        area.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"area"
          case _                                   => false

      test(m"a `sequence<T>` operation has an `over` foreign type"):
        val labels: Foreign of ("sequence" over "string") from WebIdl = shape.labels()
        labels.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), Nil) => m == t"labels"
          case _                                                                  => false

      test(m"a nullable `T?` result is a union with `null`"):
        val described: Foreign of ("string" | "null") from WebIdl = shape.describe(t"the ")
        described.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 => m == t"describe"
          case _                                                                      => false

      test(m"an `enum` reference resolves to `string`"):
        val style: Foreign of "string" from WebIdl = shape.style
        style.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"style"
          case _                                   => false

      test(m"a `typedef` to a union resolves transitively"):
        val id: Foreign of ("string" | "s32") from WebIdl = shape.id
        id.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"id"
          case _                                   => false

      test(m"a `partial interface` member is merged into the interface"):
        val order: Foreign of "s32" from WebIdl = shape.order
        order.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"order"
          case _                                   => false

      test(m"an inherited attribute resolves on the derived interface"):
        val area: Foreign of "u32" from WebIdl = circle.area
        area.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"area"
          case _                                   => false

      test(m"a mixin member applied with `includes` resolves"):
        val visible: Foreign of "boolean" from WebIdl = circle.visible
        visible.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"visible"
          case _                                   => false

      test(m"a `dictionary` field is read as a field of its foreign type"):
        val options: Foreign of "ShapeOptions" from WebIdl = Foreign["ShapeOptions", WebIdl]
        val color: Foreign of "string" from WebIdl = options.color
        color.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"color"
          case _                                   => false

      test(m"passing a WebIDL argument of the wrong foreign type is a compile error"):
        demilitarize(shape.scale(t"large")).map(_.message)
      . assert(_ == List(t"xenophile: scale expects an argument of foreign type f64"))

    suite(m"WebIDL (real DOM from webref)"):
      val node: Foreign of "Node" from WebIdlDom = Foreign["Node", WebIdlDom]
      val element: Foreign of "HTMLElement" from WebIdlDom = Foreign["HTMLElement", WebIdlDom]

      test(m"a DOM attribute is read as a field of its foreign type"):
        val nodeName: Foreign of "string" from WebIdlDom = node.nodeName
        nodeName.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Node"), t"nodeName", t"Node"))

      test(m"`unsigned short` canonicalises to `u16`"):
        val nodeType: Foreign of "u16" from WebIdlDom = node.nodeType
        nodeType.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"nodeType"
          case _                                   => false

      test(m"an inherited attribute resolves up the chain (HTMLElement → Element)"):
        val tagName: Foreign of "string" from WebIdlDom = element.tagName
        tagName.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"tagName"
          case _                                   => false

      test(m"a member inherited from the root (HTMLElement → … → Node) resolves"):
        val nodeName: Foreign of "string" from WebIdlDom = element.nodeName
        nodeName.expr
      . assert:
          case Foreign.Expression.Select(_, m, _) => m == t"nodeName"
          case _                                   => false

      test(m"an operation inherited from EventTarget resolves on HTMLElement"):
        val dispatched: Foreign of "boolean" from WebIdlDom =
          element.dispatchEvent(Foreign["Event", WebIdlDom])

        dispatched.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 =>
            m == t"dispatchEvent"

          case _ =>
            false

      test(m"an inherited operation is typed by its result"):
        val appended: Foreign of "Node" from WebIdlDom = node.appendChild(Foreign["Node", WebIdlDom])
        appended.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), args__) if args__.length == 1 =>
            m == t"appendChild"

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
      given LocalClasspath = unsafely(System.properties.java.`class`.path().as[LocalClasspath])
      import highlighting.typecheckedScala

      def completionsAt(source: Text): List[prophesy.Completion] =
        Scala.highlight(source, caret = source.length.z).completions.lay(Nil)(_.items)

      val header = t"import xenophile.*\nimport xenophile.tsInterface\n"

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

  def typescriptParserTests(): Unit =
    import strategies.throwUnsafely

    def declarations(source: Text): List[TypescriptDeclaration] = TypescriptParser.parse(source)

    def names(source: Text): scala.List[Text] = declarations(source).stdlib.map(_.key)

    def members(source: Text): scala.List[Text] =
      declarations(source).stdlib.flatMap(_.declaredMembers.stdlib).map(_.selector)

    // Every construct below was dropped whole, or silently misread, by the grammar this
    // replaced; each test names the shape rather than the mechanism.

    test(m"a generic interface is read, not dropped"):
      names(t"interface Box<T> { value: T; }")
    . assert(_ == scala.List(t"Box"))

    test(m"an interface's extends clause is recorded"):
      declarations(t"interface A { x: number; }\ninterface B extends A { y: number; }").stdlib
      . collect { case interface: TypescriptDeclaration.Interface => interface }
      . flatMap(_.extending.stdlib.map(_.text))
    . assert(_ == scala.List(t"A"))

    test(m"a type alias is a declaration"):
      names(t"type Id = string | number;")
    . assert(_ == scala.List(t"Id"))

    test(m"a class, an enum, a function and a const are declarations"):
      names(t"""|declare class C { m(): void; }
                |declare enum E { A, B }
                |declare function f(x: number): string;
                |declare const k: number;
                |""".s.stripMargin.tt)
    . assert(_ == scala.List(t"C", t"E", t"f", t"k"))

    test(m"a namespace qualifies the declarations it encloses"):
      names(t"declare namespace a { namespace b { interface X { y: number; } } }")
    . assert(_ == scala.List(t"a.b.X"))

    test(m"only exported declarations are exported in a module"):
      declarations(t"export interface A { x: number; }\ninterface B { y: number; }").stdlib
      . map { declaration => (declaration.key, declaration.exported) }
    . assert(_ == scala.List((t"A", true), (t"B", false)))

    test(m"every top-level declaration is exported in a global script"):
      declarations(t"interface A { x: number; }").stdlib.map(_.exported)
    . assert(_ == scala.List(true))

    test(m"a comment does not start a declaration"):
      names(t"// interface Ghost { x: number; }\ninterface Real { x: number; }")
    . assert(_ == scala.List(t"Real"))

    test(m"a block comment is skipped entirely"):
      names(t"/* interface Ghost {\n x: number; } */\ninterface Real { x: number; }")
    . assert(_ == scala.List(t"Real"))

    test(m"an index signature is a member of its own kind"):
      members(t"interface A { [key: string]: number; }")
    . assert(_ == scala.List(t"[]"))

    test(m"a call signature and a construct signature are distinct members"):
      members(t"interface A { (x: number): string; new (y: string): A; }")
    . assert(_ == scala.List(t"()", t"new()"))

    test(m"a getter and a setter do not collide with a property"):
      members(t"interface A { get x(): number; set x(value: number); }")
    . assert(_ == scala.List(t"get x", t"set x"))

    test(m"overloads accumulate under one member rather than overwriting"):
      declarations(t"interface A { f(x: number): string; f(x: string): number; }").stdlib
      . flatMap(_.declaredMembers.stdlib).map(_.signatures.stdlib.length)
    . assert(_ == scala.List(2))

    test(m"an inline object type does not terminate the enclosing interface"):
      members(t"interface A { config: { host: string; port: number }; after: number; }")
    . assert(_ == scala.List(t"config", t"after"))

    test(m"a function type is read as a function, not as a stray parenthesis"):
      declarations(t"interface A { handler: (event: string) => void; }").stdlib
      . flatMap(_.declaredMembers.stdlib).flatMap(_.signatures.stdlib).map(_.text)
    . assert(_ == scala.List(t"(event: string) => void"))

    test(m"an intersection is not truncated to its first member"):
      declarations(t"type T = A & B;").stdlib
      . collect { case alias: TypescriptDeclaration.Alias => alias.target.text }
    . assert(_ == scala.List(t"A & B"))

    test(m"a tuple type is read"):
      declarations(t"type T = [string, number];").stdlib
      . collect { case alias: TypescriptDeclaration.Alias => alias.target.text }
    . assert(_ == scala.List(t"[string, number]"))

    test(m"a string literal type keeps its value and is not confused with a name"):
      declarations(t"""type T = "a" | "b";""").stdlib
      . collect { case alias: TypescriptDeclaration.Alias => alias.target.text }
    . assert(_ == scala.List(t"a | b"))

    test(m"a negative numeric literal type keeps its sign"):
      declarations(t"type T = -1;").stdlib
      . collect { case alias: TypescriptDeclaration.Alias => alias.target.text }
    . assert(_ == scala.List(t"-1"))

    test(m"a nested array type is read to the right depth"):
      declarations(t"type T = string[][];").stdlib
      . collect { case alias: TypescriptDeclaration.Alias => alias.target.text }
    . assert(_ == scala.List(t"string[][]"))

    test(m"a type predicate is read"):
      declarations(t"declare function isFoo(x: unknown): x is Foo;").stdlib
      . collect { case function: TypescriptDeclaration.Function => function }
      . flatMap(_.signatures.stdlib).map(_.text)
    . assert(_ == scala.List(t"(x: unknown) => x is Foo"))

    test(m"a rest parameter is marked as such"):
      declarations(t"declare function f(...args: string[]): void;").stdlib
      . collect { case function: TypescriptDeclaration.Function => function }
      . flatMap(_.signatures.stdlib)
      . collect { case TypescriptType.Function(parameters, _, _, _) => parameters.stdlib.map(_.rest) }
    . assert(_ == scala.List(scala.List(true)))

    // Constructs outside the grammar are refused, never approximated. This is the property the
    // discipline depends on: a declaration file read as a smaller contract than it declares
    // would make every claim computed from it unsound.

    def refuses(source: Text): Optional[TypescriptError.Reason] =
      import errorDiagnostics.stackTracesDiagnostics
      capture[TypescriptError](TypescriptParser.parse(source)).reason

    test(m"a conditional type is refused"):
      refuses(t"type T<A> = A extends string ? number : boolean;")
    . assert(_ == TypescriptError.Reason.Unsupported(t"a conditional type"))

    test(m"a template literal type is refused"):
      refuses(t"type T = `a${'$'}{B}c`;")
    . assert(_ == TypescriptError.Reason.Unsupported(t"a template literal type"))

    test(m"an infer binder is refused"):
      refuses(t"type T<A> = Array<infer B>;")
    . assert(_ == TypescriptError.Reason.Unsupported(t"an `infer` binder"))

    test(m"a mapped type is refused under its own name"):
      refuses(t"interface A { [K in B]: number; }")
    . assert(_ == TypescriptError.Reason.Unsupported(t"a mapped type"))

    test(m"an unterminated string literal is a syntax error"):
      refuses(t"""type T = "unterminated;""").let:
        case TypescriptError.Reason.Syntax(_, _) => true
        case _                                   => false
    . assert(_ == true)

    test(m"a duplicated class declaration is refused"):
      refuses(t"declare class A {}\ndeclare class A {}")
    . assert(_ == TypescriptError.Reason.Duplicate(t"A"))

    test(m"interfaces merge, so a repeated interface name is accepted"):
      names(t"interface A { x: number; }\ninterface A { y: number; }")
    . assert(_ == scala.List(t"A", t"A"))

    // The dialect projection, which the foreign-function macro reads.

    test(m"the dialect resolves a member inherited through extends"):
      TypescriptDialect.parse(t"interface A { x: number; }\ninterface B extends A { y: number; }")
      . at(t"B").lay(scala.Nil) { members => members.stdlib.keys.toList }
      . sortBy(_.s)
    . assert(_ == scala.List(t"x", t"y"))

    test(m"the dialect reads a generic interface the old grammar dropped"):
      TypescriptDialect.parse(t"interface Box<T> { value: T; }")
      . at(t"Box").lay(scala.Nil) { members => members.stdlib.keys.toList }
    . assert(_ == scala.List(t"value"))

  def dtsDisciplineTests(): Unit =
    import reliquary.*
    import alphabets.hexLowerCase
    import strategies.throwUnsafely

    def content(source: Text): List[(TreePath, Data)] =
      List((TreePath(t"types/index.d.ts"), Array.unsafeFrozen(source.s.getBytes("UTF-8").nn)))

    def atomize(source: Text): Atomization =
      DtsDiscipline.atomize(content(source), Discipline.Context(t"jvm"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      t"""|export interface Client {
          |  send(message: string): void;
          |  readonly id: string;
          |}
          |export type Handle = string | number;
          |export declare function connect(url: string): Client;
          |""".s.stripMargin.tt

    test(m"the discipline claims declaration files and nothing else"):
      val data = Array.freeze(Array[Byte](0))

      (DtsDiscipline.claims(TreePath(t"types/index.d.ts"), data),
       DtsDiscipline.claims(TreePath(t"lib/index.js"), data),
       DtsDiscipline.claims(TreePath(t"readme.md"), data))
    . assert(_ == (true, false, false))

    test(m"the discipline certifies recompilation and not linkage"):
      (DtsDiscipline.id, DtsDiscipline.guarantees(t"jvm"), DtsDiscipline.keying)
    . assert(_ == (t"dts/1", Set(Discipline.Guarantee.Recompilation),
        Discipline.Keying.Declaration))

    test(m"each exported declaration and each member yields an atom"):
      keys(baseline)
    . assert(_ == scala.List(t"Client", t"Client#id", t"Client#send", t"Handle", t"connect"))

    test(m"an unexported declaration is not part of the contract"):
      keys(t"export interface A { x: number; }\ninterface Hidden { y: number; }")
    . assert(_ == scala.List(t"A", t"A#x"))

    test(m"atomization is deterministic"):
      def once(): scala.List[(Text, Text)] =
        atomize(baseline).atoms.stdlib
        . map { atom => (atom.key, atom.valueHash.serialize[Hex]) }
        . sortBy(_(0).s)

      once() == once()
    . assert(identity)

    test(m"renaming a type parameter changes nothing"):
      grade(t"export interface Box<T> { value: T; }", t"export interface Box<U> { value: U; }")
    . assert(_ == Grade.Patch)

    test(m"reordering the members of a union changes nothing"):
      grade(t"export type T = A | B;", t"export type T = B | A;")
    . assert(_ == Grade.Patch)

    test(m"reordering the elements of a tuple is a major change"):
      grade(t"export type T = [A, B];", t"export type T = [B, A];")
    . assert(_ == Grade.Major)

    // The one change that is honestly two events: adding a member is pure extension for a
    // consumer who calls the interface, and a break for one who implements it. The member's own
    // atom records the first; the fold of member keys into the interface's atom records the
    // second, and the second is what the grade reports.
    test(m"adding an interface member is a major change for implementors"):
      grade(t"export interface A { x: number; }", t"export interface A { x: number; y: number; }")
    . assert(_ == Grade.Major)

    test(m"the added member is nonetheless an atom of its own"):
      keys(t"export interface A { x: number; y: number; }")
    . assert(_ == scala.List(t"A", t"A#x", t"A#y"))

    test(m"adding a whole interface is a minor change"):
      grade(t"export interface A { x: number; }",
          t"export interface A { x: number; }\nexport interface B { y: number; }")
    . assert(_ == Grade.Minor)

    test(m"removing a member is a major change"):
      grade(t"export interface A { x: number; y: number; }", t"export interface A { x: number; }")
    . assert(_ == Grade.Major)

    test(m"making a member optional is a major change"):
      grade(t"export interface A { x: number; }", t"export interface A { x?: number; }")
    . assert(_ == Grade.Major)

    test(m"adding an overload is a major change"):
      grade(t"export interface A { f(x: number): void; }",
          t"export interface A { f(x: number): void; f(x: string): void; }")
    . assert(_ == Grade.Major)

    test(m"changing a declaration's namespace changes its key"):
      keys(t"export declare namespace a { interface X { y: number; } }")
    . assert(_ == scala.List(t"a.X", t"a.X#y"))

    test(m"an unreadable declaration file is an atomization error"):
      import errorDiagnostics.stackTracesDiagnostics

      capture[DisciplineError]:
        DtsDiscipline.atomize(content(t"export type T<A> = A extends string ? 1 : 2;"),
            Discipline.Context(t"jvm"))

      . reason
    . assert:
        case DisciplineError.Reason.Malformed(_) => true
        case _                                   => false

    test(m"the registry falls back to opaque for content the discipline does not claim"):
      val registry = Discipline.Registry(List(DtsDiscipline))
      val js = List((TreePath(t"lib/index.js"), Array.freeze(Array[Byte](1))))

      registry.atomize(js, Discipline.Context(t"jvm")).stdlib.map(_.discipline)
    . assert(_ == scala.List(t"opaque/1"))

  def webIdlDisciplineTests(): Unit =
    import reliquary.*
    import strategies.throwUnsafely

    def content(source: Text): List[(TreePath, Data)] =
      List((TreePath(t"idl/browser.idl"), Array.unsafeFrozen(source.s.getBytes("UTF-8").nn)))

    def atomize(source: Text): Atomization =
      WebIdlDiscipline.atomize(content(source), Discipline.Context(t"host"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      t"""|interface Widget {
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
        val data = Array.freeze(Array[Byte](0))

        (WebIdlDiscipline.claims(TreePath(t"idl/dom.idl"), data),
         WebIdlDiscipline.claims(TreePath(t"lib/index.js"), data),
         WebIdlDiscipline.domain.covers(t"host"),
         WebIdlDiscipline.domain.covers(t"jvm"))
      . assert(_ == (true, false, true, false))

      test(m"declarations, members, fields and values yield atoms"):
        keys(baseline)
      . assert(_ == scala.List(t"Direction", t"Direction#down", t"Direction#up", t"Options",
          t"Options#retries", t"Widget", t"Widget#name", t"Widget#render(s32)"))

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
          t"""|interface Base {};
              |interface mixin Extras { undefined extra(); };
              |Base includes Extras;
              |""".s.stripMargin.tt

        keys(mixed)
      . assert(_ == scala.List(t"Base", t"Base#extra()"))

      test(m"a partial interface in another file completes its target"):
        val split = List(
          (TreePath(t"idl/a.idl"),
           Array.unsafeFrozen(t"interface W {};".s.getBytes("UTF-8").nn)),
          (TreePath(t"idl/b.idl"),
           Array.unsafeFrozen(t"partial interface W { attribute long x; };".s
              .getBytes("UTF-8").nn)))

        WebIdlDiscipline.atomize(split, Discipline.Context(t"host")).atoms.stdlib.map(_.key)
        . sortBy(_.s)
      . assert(_ == scala.List(t"W", t"W#x"))

      test(m"exposure scopes are part of the key"):
        keys(t"[Exposed=(Window,Worker)] interface Scoped {};")
      . assert(_ == scala.List(t"Scoped[Window,Worker]"))

      test(m"identically-shaped members of different interfaces do not alias"):
        val twins = t"interface A { attribute long x; };\ninterface B { attribute long x; };"
        val atoms = atomize(twins).atoms.stdlib

        atoms.map { atom => LiraHash.text(atom.valueHash) }.distinct.size
        == atoms.size
      . assert(identity)

      test(m"union member order does not affect a hash"):
        val one = atomize(t"interface U { attribute (long or DOMString) x; };")
        val two = atomize(t"interface U { attribute (DOMString or long) x; };")

        one.atoms.stdlib.map { atom => LiraHash.text(atom.valueHash) }
        == two.atoms.stdlib.map { atom => LiraHash.text(atom.valueHash) }
      . assert(identity)

      test(m"an unsupported construct is an atomization error"):
        import errorDiagnostics.stackTracesDiagnostics

        capture[DisciplineError](atomize(t"weird thing;")).reason match
          case DisciplineError.Reason.Malformed(_) => true
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
      WitDiscipline.atomize(content(source), Discipline.Context(t"host"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      t"""|package wasi:random@0.2.0;
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
        val data = Array.freeze(Array[Byte](0))

        (WitDiscipline.claims(TreePath(t"wit/world.wit"), data),
         WitDiscipline.claims(TreePath(t"lib/api.idl"), data),
         WitDiscipline.domain.covers(t"host"),
         WitDiscipline.domain.covers(t"component"),
         WitDiscipline.domain.covers(t"jvm"))
      . assert(_ == (true, false, true, true, false))

      test(m"interfaces, items and worlds yield package-qualified atoms"):
        keys(baseline)
      . assert(_ == scala.List(
          t"wasi:random/host@0.2.0",
          t"wasi:random/host@0.2.0#import wasi:random/random@0.2.0",
          t"wasi:random/random@0.2.0",
          t"wasi:random/random@0.2.0#get-random-bytes",
          t"wasi:random/random@0.2.0#seed"))

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
          t"""|package a:pkg;
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
          . filter(_.key == t"a:pkg/two#get")
          . map { atom => LiraHash.text(atom.valueHash) }
        }

        hashes(direct) == hashes(renamed)
      . assert(identity)

      test(m"a since gate is consumed and an unstable gate is refused"):
        import errorDiagnostics.stackTracesDiagnostics

        val gated =
          t"""|package a:pkg;
              |interface one {
              |  @since(version = 0.2.1)
              |  get: func() -> u64;
              |}
              |""".s.stripMargin.tt

        val unstable = gated.s.replace("@since(version = 0.2.1)",
            "@unstable(feature = fancy)").nn.tt

        val accepted = atomize(gated).atoms.stdlib.exists(_.key == t"a:pkg/one#get")

        val refused =
          capture[DisciplineError](atomize(unstable)).reason match
            case DisciplineError.Reason.Malformed(_) => true
            case _                                   => false

        (accepted, refused)
      . assert(_ == (true, true))

      test(m"an unresolvable type reference is an error"):
        import errorDiagnostics.stackTracesDiagnostics

        capture[DisciplineError]:
          atomize(t"package a:pkg;\ninterface one { get: func() -> mystery; }")
        . reason match
            case DisciplineError.Reason.Unresolved(_) => true
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
      CHeaderDiscipline.atomize(content(source), Discipline.Context(t"host"))

    def keys(source: Text): scala.List[Text] =
      atomize(source).atoms.stdlib.map(_.key).sortBy(_.s)

    def hashOf(source: Text, key: Text): Optional[Text] =
      atomize(source).atoms.stdlib.find(_.key == key)
      . map { atom => LiraHash.text(atom.valueHash) }.getOrElse(Unset)

    def grade(before: Text, after: Text): Grade =
      Grade.between(List(atomize(before)), List(atomize(after)))

    val baseline: Text =
      t"""|typedef struct Point { int x; int y; } Point;
          |typedef enum { LEFT, RIGHT } Direction;
          |int add(int a, int b);
          |size_t strlen(const char* s);
          |""".s.stripMargin.tt

    suite(m"The `cheader/1` discipline"):
      test(m"the discipline claims headers in the host world and nothing else"):
        val data = Array.freeze(Array[Byte](0))

        (CHeaderDiscipline.claims(TreePath(t"include/openssl.h"), data),
         CHeaderDiscipline.claims(TreePath(t"src/main.c"), data),
         CHeaderDiscipline.domain.covers(t"host"),
         CHeaderDiscipline.domain.covers(t"nir"))
      . assert(_ == (true, false, true, false))

      test(m"declarations are keyed by bare name"):
        keys(baseline)
      . assert(_ == scala.List(t"Direction", t"Point", t"add", t"strlen"))

      test(m"adding a declaration is minor and removing one is major"):
        val grown = t"${baseline}double pow(double base, double exponent);"
        (grade(baseline, grown), grade(grown, baseline))
      . assert(_ == (Grade.Minor, Grade.Major))

      test(m"signedness distinguishes hashes"):
        hashOf(t"int f(unsigned int x);", t"f") != hashOf(t"int f(int x);", t"f")
      . assert(identity)

      test(m"pointer depth distinguishes hashes"):
        hashOf(t"int f(char** x);", t"f") != hashOf(t"int f(char* x);", t"f")
      . assert(identity)

      test(m"pointee constness folds and by-value constness does not"):
        (hashOf(t"int f(const char* x);", t"f") != hashOf(t"int f(char* x);", t"f"),
         hashOf(t"int f(const int x);", t"f") == hashOf(t"int f(int x);", t"f"))
      . assert(_ == (true, true))

      test(m"parameter names do not fold"):
        hashOf(t"int add(int a, int b);", t"add") == hashOf(t"int add(int x, int y);", t"add")
      . assert(identity)

      test(m"enumerator values fold, explicit or implicit"):
        (hashOf(t"typedef enum { A, B } E;", t"E")
           == hashOf(t"typedef enum { A = 0, B = 1 } E;", t"E"),
         hashOf(t"typedef enum { A, B } E;", t"E")
           != hashOf(t"typedef enum { A, B = 5 } E;", t"E"))
      . assert(_ == (true, true))

      test(m"completing an opaque struct changes its value"):
        hashOf(t"struct S;", t"S") != hashOf(t"struct S { int x; };", t"S")
      . assert(identity)

      test(m"an unsupported construct is an atomization error"):
        import errorDiagnostics.stackTracesDiagnostics

        capture[DisciplineError](atomize(t"int x = 4;")).reason match
          case DisciplineError.Reason.Malformed(_) => true
          case _                                   => false
      . assert(identity)

      test(m"the sample library header atomizes"):
        val stream = getClass.getResourceAsStream("/xenophile/library.h").nn
        val bytes = stream.readAllBytes().nn
        stream.close()
        atomize(Text(String(bytes, "UTF-8"))).atoms.stdlib.map(_.key).sortBy(_.s)
      . assert(_.contains(t"HMAC") == false)

      test(m"the openssl header atomizes with its functions keyed by symbol"):
        // The header lives in enigmatic's resources; where it is absent from this suite's
        // classpath the test degenerates to a pass rather than a false failure.
        val stream = getClass.getResourceAsStream("/enigmatic/openssl.h")

        if stream == null then true else
          val bytes = stream.nn.readAllBytes().nn
          stream.nn.close()
          atomize(Text(String(bytes, "UTF-8"))).atoms.stdlib.exists(_.key == t"RAND_bytes")
      . assert(_ == true)
