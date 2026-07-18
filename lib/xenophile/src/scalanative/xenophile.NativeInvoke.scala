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
// Materializes a call of any arity whose parameters and result are C primitives (`int`, `long`,
// `short`, `char`, `double`, `float`, `bool`, `void`) or C strings (`char*` ↔ `Text`): each Scala
// argument, recovered from the `Foreign.converter` `Conversion` the navigation applied, is passed
// to a `CFuncPtr` of the matching arity, with `Text` arguments marshalled to a `CString` in a
// `Zone` and a `Text` result read back with `fromCString`. General pointers/structs are next.
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

    // The argument operands the navigation applied (`Expression.Apply`'s argument list); an empty
    // list for the bare selection of a zero-parameter function (`Expression.Select`, preferred
    // inside `inline` definitions).
    def argumentList(term: Term): List[Term] = strip(term) match
      case Apply(_, List(varargs)) => strip(varargs).absolve match
        case Repeated(elements, _) => elements.map(strip)
        case _                     => Nil

      case _ =>
        Nil

    val (selectNode, argumentTerms) = expression match
      case Apply(Select(_, "apply"), List(node, args)) => (strip(node), argumentList(args))
      case _                                           => (expression, Nil)

    val (owner, function) = selectNode.absolve match
      case Apply(Select(_, "apply"), List(_, member, owner)) => (literal(owner), literal(member))
      case _                                                 => notCall

    // The Scala value wrapped by the `Foreign.converter` `Conversion` in an argument operand — the
    // argument of the conversion's `apply` (`argTree` emits `arg.expr`, where `arg` is that
    // converted value). Recovered by traversal, as in `WasmInvoke`.
    def convertedValue(term: Term): Term =
      var found: Optional[Term] = Unset

      val traverser = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case Apply(Select(qualifier, "apply"), List(value))
          if qualifier.tpe <:< TypeRepr.of[Conversion[Nothing, Foreign]] =>
            if found.absent then found = value

          case _ =>
            traverseTreeChildren(tree)(owner)

      traverser.traverseTree(term)(Symbol.spliceOwner)

      found.or:
        halt(m"xenophile: a foreign argument must be a Scala value with an `Interoperable`")

    // Validate the call against the parsed C header and read its parameter and result types.
    val allDefinitions = Xenophile.definitions(origin, Xenophile.locusOf(origin))

    val members = allDefinitions.at(owner).or:
      halt(m"xenophile: the foreign type $owner is not defined")

    val prototype = members.at(function).or:
      halt(m"xenophile: the foreign type $owner has no member $function")

    val parameterTypes = prototype.parameters.or(Nil)

    if argumentTerms.length != parameterTypes.length then
      halt(m"xenophile: wrong number of arguments for $owner.$function")

    // Each C type maps to the Scala type of identical ABI: a primitive (`int`→`Int`; Scala Native's
    // `CInt` etc. are aliases), or `CString` (`char*`) for a `Text`. `cType` returns the `CFuncPtr`
    // slot type and whether it is a string (needing `Text`/`CString` marshalling). The `CString`
    // type is read off `dlopen`'s own `CString` parameter, so no Native type is named here.
    val cstringType = dlopen.paramSymss.head.head.info

    def cType(tpe: Foreign.Type): (TypeRepr, Boolean) = tpe match
      case Foreign.Type.Named(t"int")    => (TypeRepr.of[Int], false)
      case Foreign.Type.Named(t"long")   => (TypeRepr.of[Long], false)
      case Foreign.Type.Named(t"short")  => (TypeRepr.of[Short], false)
      case Foreign.Type.Named(t"char")   => (TypeRepr.of[Byte], false)
      case Foreign.Type.Named(t"double") => (TypeRepr.of[Double], false)
      case Foreign.Type.Named(t"float")  => (TypeRepr.of[Float], false)
      case Foreign.Type.Named(t"bool")   => (TypeRepr.of[Boolean], false)
      case Foreign.Type.Named(t"void")   => (TypeRepr.of[Unit], false)
      case Foreign.Type.Named(t"string") => (cstringType, true)

      case _ =>
        halt(m"xenophile: $owner.$function uses a pointer or struct type, unsupported on native")

    val paramInfo = parameterTypes.map(cType)
    val (resultCtype, resultIsString) = cType(prototype.result)
    val hasStringArg = paramInfo.exists(_._2)
    val arity = paramInfo.length

    // `CFuncPtr<arity>[param…, result]`.
    val cfuncPtrN = Symbol.requiredClass(s"scala.scalanative.unsafe.CFuncPtr$arity")
    val funcType = cfuncPtrN.typeRef.appliedTo(paramInfo.map(_._1) :+ resultCtype)

    // `new CQuote(StringContext("function")).c()` — the interned C string of the symbol name. The
    // `StringContext` is built with a quote (it is plain stdlib, so its varargs are spread by the
    // quote compiler correctly) and only the Scala Native `CQuote`/`c` around it are name-resolved.
    val context = '{_root_.scala.StringContext(${Expr(function.s)})}.asTerm
    val quoted = Apply(Select(New(TypeTree.ref(cquote)), cquote.primaryConstructor), List(context))
    val symbolName = Apply(Select(quoted, cMethod), Nil)

    // `dlopen(null, RTLD_NOW)` — a handle over the main program and everything already loaded (the
    // C symbols the linked binary provides), then `dlsym(handle, c"function")`. The null filename
    // is cast to dlopen's own `CString` parameter type, since Scala Native's pointer type is not
    // implicitly nullable.
    val nullFilename =
      Select.unique(Literal(NullConstant()), "asInstanceOf").appliedToType(cstringType)

    val handle = Apply(Ref(dlopen), List(nullFilename, Ref(rtldNow)))
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
    // it from `Any`), and a `string` one is marshalled to a `CString` in `zone`; a `string` result
    // is read back with `fromCString`.
    def invocation(zone: Optional[Term]): Term =
      val callArgs = argumentTerms.zip(paramInfo).map: (term, info) =>
        val (tpe, isString) = info
        val value = convertedValue(term)

        if isString then
          val string = Select.unique(value, "asInstanceOf").appliedToType(TypeRepr.of[String])
          val place = zone.or(halt(m"xenophile: no zone for a string argument"))
          Apply(Apply(Ref(toCString), List(string)), List(place))
        else
          Select.unique(value, "asInstanceOf").appliedToType(tpe)

      val raw = Apply(Select(funcPtr, cfuncPtrN.methodMember("apply").head), callArgs)

      if resultIsString then Apply(Ref(fromCString), List(raw, charset)) else raw

    val innerType = if resultIsString then TypeRepr.of[String] else resultCtype

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
