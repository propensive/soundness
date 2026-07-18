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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.quoted.*

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

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
// Like `JsInvoke`, this v1 materializes nullary calls returning a C primitive (`interface.fn()`);
// argument marshalling (through `Interoperable`, with `Text`↔`CString` conversions) and non-scalar
// results are the follow-up, tracked alongside the `Js`/`Wasm` forms'.
object NativeInvoke:
  def invoke[result: Type](self: Expr[Foreign])(using quotes: Quotes): Expr[result] =
    import quotes.reflect.*

    // The native runtime entry points, resolved from the downstream `nativelib`/`posixlib` so this
    // module needs no dependency on them (the macro runs on the JVM; the call links downstream).
    val dlfcn = Symbol.requiredModule("scala.scalanative.posix.dlfcn")
    val dlopen = dlfcn.methodMember("dlopen").head
    val dlsym = dlfcn.methodMember("dlsym").head
    val rtldNow = dlfcn.methodMember("RTLD_NOW").head
    val cfuncPtr = Symbol.requiredModule("scala.scalanative.unsafe.CFuncPtr")
    val fromPtr = cfuncPtr.methodMember("fromPtr").head
    val cfuncPtr0 = Symbol.requiredClass("scala.scalanative.unsafe.CFuncPtr0")
    val cquote = Symbol.requiredClass("scala.scalanative.unsafe.CQuote")
    val cMethod = cquote.methodMember("c").head

    // The receiver carries the source language (`Origin`) in its refined type; the C function it
    // was reached through is recovered from the `Foreign.Expression` the navigation built.
    val (_, origin) = Xenophile.receiver(self)

    // The navigation expands to `Foreign.make(<AST>).asInstanceOf[…]`, nested in `Inlined`/`Typed`
    // layers `underlyingArgument` cannot fold away; recover the AST by term-level stripping, as in
    // `WasmInvoke`/`JsInvoke`.
    def strip(term: Term): Term = term match
      case Inlined(_, _, body)                        => strip(body)
      case Typed(expr, _)                             => strip(expr)
      case Block(Nil, expr)                           => strip(expr)
      case TypeApply(Select(expr, "asInstanceOf"), _) => strip(expr)
      case _                                          => term

    def stringOf(term: Term): Text = strip(term).absolve match
      case Literal(StringConstant(string)) => string.tt

    def literal(term: Term): Text = strip(term).absolve match
      case Apply(Ident("tt"), List(argument)) => stringOf(argument)
      case other                              => stringOf(other)

    def notCall: Nothing =
      halt(m"xenophile: `invoke` expects a foreign function invocation, `interface.function()`")

    // Peel to `Foreign.make(<AST>)`; take its argument — the navigation expression AST.
    val expression = strip(self.asTerm.underlyingArgument).absolve match
      case Apply(Select(_, "make"), List(argument)) => strip(argument)
      case _                                        => notCall

    // The number of arguments the navigation applied (only nullary calls are materialized in v1).
    def argumentCount(term: Term): Int = strip(term) match
      case Apply(_, List(varargs)) => strip(varargs).absolve match
        case Repeated(elements, _) => elements.length
        case _                     => 0

      case _ =>
        0

    // Either an applied call — `Expression.Apply(select, args)` — or the bare selection of a
    // zero-parameter function (`Expression.Select`), preferred inside `inline` definitions.
    val (selectNode, argCount) = expression match
      case Apply(Select(_, "apply"), List(node, args)) => (strip(node), argumentCount(args))
      case _                                           => (expression, 0)

    val (owner, function) = selectNode.absolve match
      case Apply(Select(_, "apply"), List(_, member, owner)) => (literal(owner), literal(member))
      case _                                                 => notCall

    if argCount > 0 then
      halt(m"xenophile: this v1 of the Scala Native materializer supports only nullary calls")

    // Validate the call against the parsed C header and read its result type.
    val allDefinitions = Xenophile.definitions(origin, Xenophile.locusOf(origin))

    val members = allDefinitions.at(owner).or:
      halt(m"xenophile: the foreign type $owner is not defined")

    val prototype = members.at(function).or:
      halt(m"xenophile: the foreign type $owner has no member $function")

    if prototype.parameters.or(Nil).nonEmpty then
      halt(m"xenophile: $owner.$function takes arguments; only nullary calls materialize so far")

    // The C result type maps to the Scala primitive of identical ABI (Scala Native's `CInt`/`CLong`
    // etc. are aliases of `Int`/`Long`); the `CFuncPtr0`'s return type is that primitive directly.
    val resultType: TypeRepr = prototype.result match
      case Foreign.Type.Named(t"int")    => TypeRepr.of[Int]
      case Foreign.Type.Named(t"long")   => TypeRepr.of[Long]
      case Foreign.Type.Named(t"short")  => TypeRepr.of[Short]
      case Foreign.Type.Named(t"char")   => TypeRepr.of[Byte]
      case Foreign.Type.Named(t"double") => TypeRepr.of[Double]
      case Foreign.Type.Named(t"float")  => TypeRepr.of[Float]
      case Foreign.Type.Named(t"bool")   => TypeRepr.of[Boolean]
      case Foreign.Type.Named(t"void")   => TypeRepr.of[Unit]

      case _ =>
        halt(m"xenophile: $owner.$function returns a non-scalar type not yet supported on native")

    // `CFuncPtr0[result]`.
    val funcType = cfuncPtr0.typeRef.appliedTo(List(resultType))

    // `new CQuote(StringContext("function")).c()` — the interned C string of the symbol name.
    val parts = Typed(
      Repeated(List(Literal(StringConstant(function.s))), TypeTree.of[String]),
      TypeTree.of[Seq[String]])

    val context =
      Apply(Select.unique(Ref(Symbol.requiredModule("scala.StringContext")), "apply"), List(parts))

    val quoted = Apply(Select(New(TypeTree.ref(cquote)), cquote.primaryConstructor), List(context))
    val symbolName = Apply(Select(quoted, cMethod), Nil)

    // `dlopen(null, RTLD_NOW)` — a handle over the main program and everything already loaded (the
    // C symbols the linked binary provides), then `dlsym(handle, c"function")`.
    val handle = Apply(Ref(dlopen), List(Literal(NullConstant()), Ref(rtldNow)))
    val symbol = Apply(Ref(dlsym), List(handle, symbolName))

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

    // Invoke the zero-argument function pointer.
    Apply(Select(funcPtr, cfuncPtr0.methodMember("apply").head), Nil).asExprOf[result]
