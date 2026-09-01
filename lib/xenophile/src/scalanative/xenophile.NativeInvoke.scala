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

import scala.collection.immutable as sci
import scala.collection.immutable.{List, Nil, ::}

import scala.quoted.*

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

// The declared arity is compared against the argument count, so `List#size` is genuinely
// required; the list is a single prototype's parameters.
import denominative.dysasymptotics.linearSize
import denominative.size

// The terminal materializer for the native (C) ecosystem when the *target platform is Scala
// Native*, and the counterpart of `ForeignLibrary` (which lowers the same navigation to a JVM
// Panama downcall). It turns a fully-applied `Foreign` navigation into a real Scala Native call:
// the C symbol is resolved at runtime with `dlsym`, wrapped as a `CFuncPtr`, and invoked.
//
// As with `WasmInvoke`/`JsInvoke`, every Scala Native entry point is looked up in the *downstream*
// classpath by name (`Symbol.requiredMethod`/`requiredClass`), so this module compiles for the JVM
// (where the macro itself runs) without any dependency on the Native runtime; the call it emits is
// only ever expanded — and linked — at a downstream `.native` call site.
//
// Materializes a call of any arity whose parameters and result are C primitives (`int`, `long`,
// `short`, `char`, `double`, `float`, `bool`, `void`) or C strings (`char*` ↔ `Text`): each Scala
// argument, recovered from the `Foreign.converter` `Conversion` the navigation applied, is passed
// to a `CFuncPtr` of the matching arity, with `Text` arguments marshalled to a `CString` in a
// `Zone` and a `Text` result read back with `fromCString`. General pointers/structs are next.
object NativeInvoke extends Materializer:
  def materialize[result: Type](self: Expr[Foreign])(using quotes: Quotes): Expr[result] =
    import quotes.reflect.*

    // The native runtime entry points, resolved from the downstream `nativelib`/`posixlib` so this
    // module needs no dependency on them (the macro runs on the JVM; the call links downstream).
    // `dlopen` is retained only for its `CString` parameter type (read off below); symbol
    // resolution itself goes through `xenophile.ForeignLibrary.resolve` (its native twin), which
    // searches the libraries loaded at runtime by `register` before the process-default lookup.
    val dlfcn = Symbol.requiredModule("scala.scalanative.posix.dlfcn")
    val dlopen = dlfcn.methodMember("dlopen").head
    val resolve = Symbol.requiredMethod("xenophile.ForeignLibrary.resolve")
    val cfuncPtr = Symbol.requiredModule("scala.scalanative.unsafe.CFuncPtr")
    val fromPtr = cfuncPtr.methodMember("fromPtr").head
    val cquote = Symbol.requiredClass("scala.scalanative.unsafe.CQuote")
    val cMethod = cquote.methodMember("c").head

    // The receiver carries the source language (`Origin`) in its refined type; the C function it
    // was reached through is recovered from the `Foreign.Expression` the navigation built.
    val (_, origin) = Xenophile.receiver(self)

    val (owner, function, _, argumentTerms) = Xenophile.navigation(self)

    // Validate the call against the parsed C header and read its parameter and result types.
    val allDefinitions = Xenophile.definitions(origin, Xenophile.locusOf(origin))

    val members = allDefinitions(owner).or:
      halt(m"xenophile: the foreign type $owner is not defined")

    val prototype = members(function).or:
      halt(m"xenophile: the foreign type $owner has no member $function")

    val parameterTypes: proscenium.List[Foreign.Type] = prototype.parameters.or(proscenium.Nil)

    if argumentTerms.length != parameterTypes.size then
      halt(m"xenophile: wrong number of arguments for $owner.$function")

    // Each C type maps to the Scala type of identical ABI: a primitive (`int`→`Int`; Scala Native's
    // `CInt` etc. are aliases), `CString` (`char*`) for a `Text`, or `Ptr[Byte]` for any other
    // pointer (`T*`/`void*`/opaque handle, whose Scala value is the raw-address `Address`). `cType`
    // returns the `CFuncPtr` slot type and the marshalling `Kind` it needs. The `CString` type is
    // read off `dlopen`'s own `CString` parameter, so no Native type is named here.
    val cstringType = dlopen.paramSymss.head.head.info
    val ptrByteType = Symbol.requiredClass("scala.scalanative.unsafe.Ptr").typeRef
      .appliedTo(TypeRepr.of[Byte])

    enum Kind:
      case Plain, Str, Address

    def cType(tpe: Foreign.Type): (TypeRepr, Kind) = tpe match
      case Foreign.Type.Named(t"int")    => (TypeRepr.of[Int], Kind.Plain)
      case Foreign.Type.Named(t"long")   => (TypeRepr.of[Long], Kind.Plain)
      case Foreign.Type.Named(t"short")  => (TypeRepr.of[Short], Kind.Plain)
      case Foreign.Type.Named(t"char")   => (TypeRepr.of[Byte], Kind.Plain)
      case Foreign.Type.Named(t"double") => (TypeRepr.of[Double], Kind.Plain)
      case Foreign.Type.Named(t"float")  => (TypeRepr.of[Float], Kind.Plain)
      case Foreign.Type.Named(t"bool")   => (TypeRepr.of[Boolean], Kind.Plain)
      case Foreign.Type.Named(t"void")   => (TypeRepr.of[Unit], Kind.Plain)
      case Foreign.Type.Named(t"string") => (cstringType, Kind.Str)

      case Foreign.Type.Applied(t"ptr", _) =>
        (ptrByteType, Kind.Address)

      case _ =>
        halt(m"xenophile: $owner.$function uses a struct type, unsupported on native")

    // A stdlib view: the pairs carry `quotes.reflect.TypeRepr^` capabilities, which capture
    // checking will not let flow into the opaque collections' traversal evidence.
    val paramInfo = parameterTypes.stdlib.map(cType)
    val (resultCtype, resultKind) = cType(prototype.result)
    val hasStringArg = paramInfo.exists(_._2 == Kind.Str)
    val arity = paramInfo.length

    // The `Ptr`↔`Long` bridges for `Address` arguments and results: `fromRawPtr`/`toRawPtr` (from
    // the `scala.scalanative.runtime` package object, whose members dotty exposes on the package
    // symbol) and the `Intrinsics` casts.
    val runtimePackage = Symbol.requiredPackage("scala.scalanative.runtime")
    val fromRawPtr = runtimePackage.methodMember("fromRawPtr").head
    val toRawPtr = runtimePackage.methodMember("toRawPtr").head
    val intrinsics = Symbol.requiredModule("scala.scalanative.runtime.Intrinsics")
    val castLongToRawPtr = intrinsics.methodMember("castLongToRawPtr").head
    val castRawPtrToLong = intrinsics.methodMember("castRawPtrToLong").head

    // `CFuncPtr<arity>[param…, result]`.
    val cfuncPtrN = Symbol.requiredClass(s"scala.scalanative.unsafe.CFuncPtr$arity")
    // `TypeRepr.appliedTo` takes a stdlib `List`: the quotes API is the boundary.
    val funcType = cfuncPtrN.typeRef.appliedTo(paramInfo.map(_._1) :+ resultCtype)

    // `new CQuote(StringContext("function")).c()` — the interned C string of the symbol name. The
    // `StringContext` is built with a quote (it is plain stdlib, so its varargs are spread by the
    // quote compiler correctly) and only the Scala Native `CQuote`/`c` around it are name-resolved.
    val context = '{_root_.scala.StringContext(${Expr(function.s)})}.asTerm
    val quoted = Apply(Select(New(TypeTree.ref(cquote)), cquote.primaryConstructor), List(context))
    val symbolName = Apply(Select(quoted, cMethod), Nil)

    // `ForeignLibrary.resolve(c"function")` — the symbol pointer from the registered libraries (or
    // the default lookup), panicking if unbound rather than returning NULL. This replaces a bare
    // `dlsym(dlopen(null, RTLD_NOW), …)`, whose default-only lookup found a symbol just when some
    // *other* reachable code had statically linked its library (`@link`), and crashed otherwise.
    val symbol = Apply(Ref(resolve), List(symbolName))

    // `CFuncPtr.fromPtr[CFuncPtr0[result]](symbol)` — summon the required `Tag` implicit (present
    // downstream, where the Native runtime is on the classpath) and pass it explicitly.
    val applied = Apply(TypeApply(Ref(fromPtr), List(Inferred(funcType))), List(symbol))

    val tagType = fromPtr.paramSymss.last.head.info.substituteTypes(
      List(fromPtr.paramSymss.head.head), List(funcType))

    val tag = Implicits.search(tagType) match
      case success: ImplicitSearchSuccess =>
        success.tree

      case _ =>
        halt(m"xenophile: could not resolve the Scala Native function-pointer tag")

    val funcPtr = Apply(applied, List(tag))

    // The `Text`/`CString` marshallers, from the same `scala.scalanative.unsafe` package object as
    // `CQuote`. `toCString(str)(zone)` needs a `Zone` (the `(String)` overload is picked out by its
    // shape); `fromCString(cstr, charset)` reads one back — the default charset built with a quote,
    // since `java.*` resolves on both platforms.
    val unsafe = cquote.owner

    val toCStrings = unsafe.methodMember("toCString").filter: method =>
      method.paramSymss match
        case List(List(param), List(_)) => param.info =:= TypeRepr.of[String]
        case _                          => false

    val toCString = toCStrings.head
    val fromCString = unsafe.methodMember("fromCString").head
    val charset = '{_root_.java.nio.charset.Charset.defaultCharset().nn}.asTerm

    // The invocation: each argument is unwrapped from the navigation's `Literal` (the cast unboxes
    // it from `Any`); a `string` one is marshalled to a `CString` in `zone`, and a `pointer` one
    // (a raw-address `Long`, since `Address` is opaquely a `Long`) to a `Ptr[Byte]` with
    // `fromRawPtr(castLongToRawPtr(_))`. A `string` result is read back with `fromCString`, and a
    // pointer result lowered to its raw address with `castRawPtrToLong(toRawPtr(_))`.
    def invocation(zone: Optional[Term]): Term =
      val callArgs = argumentTerms.to(List).zip(paramInfo).map: (term, info) =>
        val (tpe, kind) = info
        val value = Xenophile.convertedValue(term)

        kind match
          case Kind.Str =>
            val string = Select.unique(value, "asInstanceOf").appliedToType(TypeRepr.of[String])
            val place = zone.or(halt(m"xenophile: no zone for a string argument"))
            Apply(Apply(Ref(toCString), List(string)), List(place))

          case Kind.Address =>
            val address = Select.unique(value, "asInstanceOf").appliedToType(TypeRepr.of[Long])
            val raw = Apply(Ref(castLongToRawPtr), List(address))
            Apply(TypeApply(Ref(fromRawPtr), List(Inferred(TypeRepr.of[Byte]))), List(raw))

          case Kind.Plain =>
            Select.unique(value, "asInstanceOf").appliedToType(tpe)

      val raw = Apply(Select(funcPtr, cfuncPtrN.methodMember("apply").head), callArgs)

      resultKind match
        case Kind.Str => Apply(Ref(fromCString), List(raw, charset))

        case Kind.Address =>
          val rawPtr = Apply(TypeApply(Ref(toRawPtr), List(Inferred(TypeRepr.of[Byte]))), List(raw))
          Apply(Ref(castRawPtrToLong), List(rawPtr))

        case Kind.Plain => raw

    val innerType = resultKind match
      case Kind.Str     => TypeRepr.of[String]
      case Kind.Address => TypeRepr.of[Long]
      case Kind.Plain   => resultCtype

    // A string argument's `CString` lives in a `Zone`, so wrap the call in `Zone.acquire { zone =>
    // … }` when (and only when) there is one; otherwise emit the call directly.
    val call =
      if !hasStringArg then invocation(Unset)
      else
        val zoneType = Symbol.requiredClass("scala.scalanative.unsafe.Zone").typeRef
        val zoneModule = Symbol.requiredModule("scala.scalanative.unsafe.Zone")
        val acquire = zoneModule.methodMember("acquire").head
        val method = MethodType(List("zone"))(_ => List(zoneType), _ => innerType)

        val lambda = Lambda(Symbol.spliceOwner, method,
          (_, params) => invocation(params.head.asInstanceOf[Term]))

        Apply(TypeApply(Ref(acquire), List(Inferred(innerType))), List(lambda))

    // Coerce the result (a Scala primitive, or the `String` from `fromCString`) to `result` — for a
    // `string` result, the `String` to the `Text` it opaquely is.
    Select.unique(call, "asInstanceOf").appliedToType(TypeRepr.of[result]).asExprOf[result]
