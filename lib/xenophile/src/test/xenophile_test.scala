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
      import kotlinInvocation.invoke

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
      val pair = Kotlin.make[kotlin.Pair[Text, Text]](t"a", t"b")

      test(m"a Kotlin class constructs through its facade"):
        pair.unwrap.toString.tt
      . assert(_ == t"(a, b)")

      test(m"a property substitutes the facade's type arguments"):
        val first: Text = pair.first
        first
      . assert(_ == t"a")

      test(m"the second component reads likewise"):
        val second: Text = pair.second
        second
      . assert(_ == t"b")

      val regex = Kotlin.make[kotlin.text.Regex](t"[0-9]+")

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
          Kotlin.make[kotlin.Pair[Text, Text]](t"a", t"b").third
      . assert(_.nonEmpty)

      test(m"a wrongly-typed constructor argument is rejected"):
        demilitarize:
          Kotlin.make[kotlin.text.Regex](42)
      . assert(_.nonEmpty)

      test(m"a non-Kotlin class cannot be made into a facade"):
        demilitarize:
          Kotlin.make[java.lang.StringBuilder](t"x")
      . assert(_.nonEmpty)

      test(m"a companion object's members are reachable"):
        val escaped: Text = Kotlin.companion[kotlin.text.Regex].escape(t"a.b")
        escaped
      . assert(_ == t"\\Qa.b\\E")

      test(m"an unknown member's error suggests near misses in Kotlin syntax"):
        demilitarize:
          Kotlin.make[kotlin.text.Regex](t"x").matchez(t"y")
        . map(_.message)
      . assert(_.exists(_.contains("did you mean")))

      test(m"a class without a companion object is rejected"):
        demilitarize:
          Kotlin.companion[kotlin.Pair[Text, Text]]
      . assert(_.nonEmpty)

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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"greet"
          case _                                                                      => false

      test(m"a Foreign argument of the declared parameter type is accepted"):
        val linked: Foreign of "Foo" from Typescript = foo.link(foo.bar)
        linked.expr
      . assert:
          case Foreign.Expression.Apply(_, List(Foreign.Expression.Select(_, b, _))) => b == t"bar"
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
          case Foreign.Expression.Apply(_, List(Foreign.Expression.Literal(_))) => true
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

      test(m"a C struct field has the field's foreign type"):
        val point: Foreign of "Point" from Native = Foreign["Point", Native]
        point.x.expr
      . assert(_ == Foreign.Expression.Select(Foreign.Expression.Reference(t"Point"), t"x", t"Point"))

      test(m"applying a C function builds an `Apply` node typed by its result"):
        val absolute: Foreign of "int" from Native = library.abs(5)
        absolute.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"abs"
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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"increment"
          case _                                                                      => false

      test(m"a fixed-width `int32_t` is canonicalised to `int`"):
        val value: Foreign of "int" from Native = library.identity(42)
        value.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"identity"
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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"greet"
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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"lookup"
          case _                                                                      => false

      test(m"passing a WIT argument of the wrong foreign type is a compile error"):
        demilitarize(api.add(t"two", t"three")).map(_.message)
      . assert(_ == List(t"xenophile: add expects an argument of foreign type s32"))

      test(m"an interface function is qualified with its package's module id"):
        val wit = t"package wasi:random@0.2.0; interface random { get-random-u64: func() -> u64; }"
        WitDialect.parse(wit)(t"random")(t"get-random-u64").module.or(t"")
      . assert(_ == t"wasi:random/random@0.2.0")

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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) => m == t"describe"
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
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) =>
            m == t"dispatchEvent"

          case _ =>
            false

      test(m"an inherited operation is typed by its result"):
        val appended: Foreign of "Node" from WebIdlDom = node.appendChild(Foreign["Node", WebIdlDom])
        appended.expr
      . assert:
          case Foreign.Expression.Apply(Foreign.Expression.Select(_, m, _), List(_)) =>
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
