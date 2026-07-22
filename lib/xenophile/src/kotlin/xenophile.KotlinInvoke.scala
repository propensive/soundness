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

import scala.quoted.*

import anticipation.*
import fulminate.*
import vacuous.*

// The terminal materializer for the Kotlin ecosystem: turns a fully-applied `Foreign`
// navigation into a direct, statically-typed JVM call — no runtime reflection. The JVM owner,
// method name and descriptor come from `KotlinDialect`'s side-table (same module, so no
// smuggling through the ecosystem-neutral `Prototype`); the method symbol is resolved from the
// downstream compile classpath, so this module needs no dependency on the Kotlin libraries it
// calls into. This v1 materializes calls to top-level (static) functions; instance receivers
// and properties follow.
object KotlinInvoke:
  def invoke[result: Type](self: Expr[Foreign])(using quotes: Quotes): Expr[result] =
    import quotes.reflect.*

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
      halt(m"xenophile: `invoke` expects a Kotlin function invocation, `facade.function(…)`")

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
      case _                                           => notCall

    val (owner, function) = selectNode.absolve match
      case Apply(Select(_, "apply"), List(_, member, owner)) => (literal(owner), literal(member))
      case _                                                 => notCall

    // The Scala value wrapped by the `Foreign.converter` `Conversion` in an argument operand,
    // recovered by traversal, as in `PanamaInvoke`.
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
        halt(m"xenophile: a Kotlin argument must be a Scala value with an `Interoperable`")

    val argumentValues = argumentTerms.map(convertedValue)

    // The parameter segment of a JVM method descriptor, split into one entry per parameter.
    def descriptorParameters(descriptor: Text): List[Text] =
      val inner = descriptor.s.substring(1, descriptor.s.indexOf(')')).nn
      var index = 0
      var result: List[Text] = Nil

      while index < inner.length do
        val start = index
        while inner(index) == '[' do index += 1
        if inner(index) == 'L' then index = inner.indexOf(';', index) + 1 else index += 1
        result = inner.substring(start, index).nn.tt :: result

      result.reverse

    // Whether an argument's Scala type can satisfy a descriptor entry: primitives must match
    // exactly (so `Int` arguments select the `(III)I` overload, not `(JJJ)J`); reference
    // parameters accept any non-primitive argument, since the emission casts.
    def satisfies(argument: quotes.reflect.TypeRepr, parameter: Text): Boolean =
      parameter.s match
        case "I" => argument <:< TypeRepr.of[Int]
        case "J" => argument <:< TypeRepr.of[Long]
        case "Z" => argument <:< TypeRepr.of[Boolean]
        case "D" => argument <:< TypeRepr.of[Double]
        case "F" => argument <:< TypeRepr.of[Float]
        case "S" => argument <:< TypeRepr.of[Short]
        case "B" => argument <:< TypeRepr.of[Byte]
        case "C" => argument <:< TypeRepr.of[Char]
        case _   => !(argument <:< TypeRepr.of[AnyVal])

    val members = KotlinDialect.members(owner, function).filter: member =>
      val shapes = argumentValues.map(_.tpe.widen).zip(descriptorParameters(member.descriptor))
      member.arity == argumentTerms.length && shapes.forall(satisfies)

    val member = members match
      case List(member) => member
      case Nil          => halt(m"xenophile: $owner has no $function matching these arguments")

      case _ =>
        halt(m"xenophile: $owner.$function is overloaded; overloads are not yet supported")

    if !member.static
    then halt(m"xenophile: $owner.$function is an instance member, which is not yet supported")

    // A Java static is a member of the class's companion module in the reflection model. The
    // owner is the *declaring* class: for a multi-file facade this is the part class, which
    // kotlin-stdlib makes package-private (parts-inheritance), so emission may be inaccessible
    // even though the JVM would resolve the call through the public facade; that limitation
    // stands until a facade-owned emission strategy exists.
    val module = Symbol.requiredClass(member.owner.s).companionModule
    val arity = argumentTerms.length

    val method =
      module.methodMember(member.jvmName.s).filter(_.paramSymss.flatten.length == arity) match
        case method :: _ => method
        case Nil         => halt(m"xenophile: no JVM method ${member.jvmName} on ${member.owner}")

    val parameterTypes = Ref(module).tpe.memberType(method) match
      case MethodType(_, types, _) => types
      case _                       => halt(m"xenophile: ${member.jvmName} is not a plain method")

    // Arguments erase to their JVM shapes: an argument whose Scala type does not already
    // conform to the parameter (e.g. an opaque `Text` against `String`) is cast, which is free
    // at runtime.
    val arguments = argumentValues.zip(parameterTypes).map: (term, tpe) =>
      if term.tpe <:< tpe then term
      else TypeApply(Select.unique(term, "asInstanceOf"), List(Inferred(tpe)))

    val call = Apply(Select(Ref(module), method), arguments)

    TypeApply(Select.unique(call, "asInstanceOf"), List(TypeTree.of[result])).asExprOf[result]

// The `invoke` terminal for Kotlin navigations. Deliberately housed in its own object rather
// than exported loose: other ecosystems' materializers (Wasm, JS) export `invoke` extensions
// too, so a named import — `import kotlinInvocation.invoke` — disambiguates where they coexist.
object kotlinInvocation:
  extension (foreign: Foreign)
    transparent inline def invoke[result]: result = ${KotlinInvoke.invoke[result]('foreign)}
