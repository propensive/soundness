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

import java.lang.foreign.*

import scala.quoted.*

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

// The terminal materializer for the native (C) ecosystem when the *target platform is the JVM*:
// the typed, macro counterpart of driving `ForeignLibrary` by hand, and the JVM analogue of
// `NativeInvoke`. It turns a fully-applied `Foreign` navigation into a Panama downcall: the
// `FunctionDescriptor` is built at compile time from the parsed C header, the symbol is resolved
// at runtime from the process-wide lookup (`ForeignLibrary.downcall`, which searches the default
// lookup and every `ForeignLibrary.register`ed library — the JVM's stand-in for the flat symbol
// namespace a statically-linked native binary gets from `dlopen(null)`), and the call is made
// through a cached `MethodHandle`.
//
// Marshalling matches `NativeInvoke`: C primitives pass through (boxed, as `invokeWithArguments`
// expects), a `string` argument (`Text`) is copied into a confined per-call `Arena`, and a
// `pointer` argument (any other `T*`) travels as its raw address via `MemorySegment.ofAddress`.
// A `string` result is read back out of native memory; a pointer result becomes a `Pointer`.
object PanamaInvoke:
  def invoke[result: Type](self: Expr[Foreign])(using quotes: Quotes): Expr[result] =
    import quotes.reflect.*

    // The receiver carries the source language (`Origin`) in its refined type; the C function it
    // was reached through is recovered from the `Foreign.Expression` the navigation built —
    // term-level stripping, as in `NativeInvoke`/`WasmInvoke`/`JsInvoke`.
    val (_, origin) = Xenophile.receiver(self)

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

    val expression = strip(self.asTerm.underlyingArgument).absolve match
      case Apply(Select(_, "make"), List(argument)) => strip(argument)
      case _                                        => notCall

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

    // The Scala value wrapped by the `Foreign.converter` `Conversion` in an argument operand,
    // recovered by traversal, as in `NativeInvoke`.
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

    // The FFM layout of each C type, spliced directly (this module compiles on the JVM, so the
    // Panama API is quotable): the same mapping as `ForeignLibrary.layout`, resolved at compile
    // time from the header's own types.
    def layoutFor(tpe: Foreign.Type): Expr[MemoryLayout] = tpe match
      case Foreign.Type.Named(t"int")    => '{ValueLayout.JAVA_INT.nn}
      case Foreign.Type.Named(t"long")   => '{ValueLayout.JAVA_LONG.nn}
      case Foreign.Type.Named(t"short")  => '{ValueLayout.JAVA_SHORT.nn}
      case Foreign.Type.Named(t"char")   => '{ValueLayout.JAVA_BYTE.nn}
      case Foreign.Type.Named(t"double") => '{ValueLayout.JAVA_DOUBLE.nn}
      case Foreign.Type.Named(t"float")  => '{ValueLayout.JAVA_FLOAT.nn}
      case Foreign.Type.Named(t"bool")   => '{ValueLayout.JAVA_BOOLEAN.nn}
      case _                             => '{ValueLayout.ADDRESS.nn}

    val layouts = Varargs(parameterTypes.map(layoutFor))

    val descriptor: Expr[FunctionDescriptor] = prototype.result match
      case Foreign.Type.Named(t"void") =>
        '{FunctionDescriptor.ofVoid($layouts*).nn}

      case result =>
        '{FunctionDescriptor.of(${layoutFor(result)}, $layouts*).nn}

    def isString(tpe: Foreign.Type): Boolean = tpe match
      case Foreign.Type.Named(t"string") => true
      case _                             => false

    val hasStringArg = parameterTypes.exists(isString)

    // Marshals each argument for `invokeWithArguments`: primitives are simply boxed; a `string`
    // is copied into `arena` (absent unless some parameter needs it); any other pointer travels
    // as a zero-length segment of its raw address.
    def marshalled(arena: Optional[Expr[Arena]]): List[Expr[AnyRef]] =
      argumentTerms.zip(parameterTypes).map: (term, paramType) =>
        val value = convertedValue(term).asExpr

        paramType match
          case Foreign.Type.Named(t"string") =>
            val place = arena.or(halt(m"xenophile: no arena for a string argument"))
            '{$place.allocateFrom($value.asInstanceOf[String]).nn}

          case Foreign.Type.Named(t"int" | t"long" | t"short" | t"char" | t"double" | t"float"
                                   | t"bool") =>
            '{$value.asInstanceOf[AnyRef]}

          case _ =>
            '{MemorySegment.ofAddress($value.asInstanceOf[Long]).nn}

    def call(arena: Optional[Expr[Arena]]): Expr[Any] =
      val arguments = Expr.ofList(marshalled(arena))

      ' {
          ForeignLibrary
          . downcall(${Expr(function.s)}.tt, $descriptor)
          . invokeWithArguments($arguments*)
        }

    val invocation: Expr[Any] =
      if !hasStringArg then call(Unset)
      else
        ' {
            val arena: Arena = Arena.ofConfined().nn
            try ${call('arena)} finally arena.close()
          }

    // Unmarshals the (boxed) result by the header's result type: a `string` is read back from
    // native memory as `Text`, a pointer becomes its raw address (a `Pointer`), a primitive is
    // simply unboxed by the cast, and `void` yields `Unit`.
    prototype.result match
      case Foreign.Type.Named(t"void") =>
        '{$invocation; ()}.asInstanceOf[Expr[result]]

      case Foreign.Type.Named(t"string") =>
        '{ForeignLibrary.text($invocation.asInstanceOf[MemorySegment]).asInstanceOf[result]}

      case Foreign.Type.Named(t"int" | t"long" | t"short" | t"char" | t"double" | t"float"
                               | t"bool") =>
        '{$invocation.asInstanceOf[result]}

      case _ =>
        '{Pointer($invocation.asInstanceOf[MemorySegment].address).asInstanceOf[result]}
