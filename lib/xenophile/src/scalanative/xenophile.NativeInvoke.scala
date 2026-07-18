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
// `short`, `char`, `double`, `float`, `bool`, `void`): each Scala argument, recovered from the
// `Foreign.converter` `Conversion` the navigation applied, is passed straight to a `CFuncPtr` of
// the matching arity. Pointer/`Text`↔`CString` marshalling (which needs a `Zone`) is the next step.
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

    // Each C type maps to the Scala primitive of identical ABI (Scala Native's `CInt`/`CLong` etc.
    // are aliases of `Int`/`Long`); a `CFuncPtr`'s parameter and return types are those primitives.
    def scalaType(tpe: Foreign.Type): TypeRepr = tpe match
      case Foreign.Type.Named(t"int")    => TypeRepr.of[Int]
      case Foreign.Type.Named(t"long")   => TypeRepr.of[Long]
      case Foreign.Type.Named(t"short")  => TypeRepr.of[Short]
      case Foreign.Type.Named(t"char")   => TypeRepr.of[Byte]
      case Foreign.Type.Named(t"double") => TypeRepr.of[Double]
      case Foreign.Type.Named(t"float")  => TypeRepr.of[Float]
      case Foreign.Type.Named(t"bool")   => TypeRepr.of[Boolean]
      case Foreign.Type.Named(t"void")   => TypeRepr.of[Unit]

      case _ =>
        halt(m"xenophile: a C type in $owner.$function is not yet supported on native")

    val resultType = scalaType(prototype.result)
    val paramTypes = parameterTypes.map(scalaType)
    val arity = paramTypes.length

    // The marshalled call arguments: for a C primitive the wrapped Scala value is already of the
    // right type, ascribed to the `CFuncPtr` parameter type (which unboxes it from the `Any` the
    // navigation's `Literal` stored it as).
    val callArgs = argumentTerms.zip(paramTypes).map: (term, tpe) =>
      Select.unique(convertedValue(term), "asInstanceOf").appliedToType(tpe)

    // `CFuncPtr<arity>[param…, result]`.
    val cfuncPtrN = Symbol.requiredClass(s"scala.scalanative.unsafe.CFuncPtr$arity")
    val funcType = cfuncPtrN.typeRef.appliedTo(paramTypes :+ resultType)

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
    val cstringType = dlopen.paramSymss.head.head.info

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

    // Invoke the function pointer with the marshalled arguments.
    Apply(Select(funcPtr, cfuncPtrN.methodMember("apply").head), callArgs).asExprOf[result]
