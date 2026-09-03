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
import scala.collection.immutable.Seq


import anticipation.*
import fulminate.*
import gigantism.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*
import denominative.*
import denominative.dysasymptotics.linearSize

object Xenophile:

  // Resolves the grammar for a source language by reading the `Grammar` type member of its
  // `Ecosystem` — the singleton type of a `Dialect` object — and loading that object reflectively
  // at compile time. This keeps `core` free of any dependency on the ecosystems (sibling modules).
  private def dialectFor(using quotes: Quotes)(origin: quotes.reflect.TypeRepr): Dialect =
    import quotes.reflect.*

    val grammar = origin.typeSymbol.typeMember("Grammar")

    if !grammar.exists then halt(m"xenophile: the source language does not define a `Grammar`")

    val dialectType = origin.memberType(grammar) match
      case TypeBounds(_, hi) => hi
      case other             => other

    // The symbol's full name is `$`-suffixed for a module, but dots separate owners, so a *nested*
    // grammar object reads `Outer$.Inner$` where the JVM binary name is `Outer$Inner$`; the
    // singleton is the static `MODULE$` field. `Class.forName/1` uses the macro's own classloader.
    val className = dialectType.typeSymbol.fullName.replaceAll("\\$\\.", "\\$").nn

    try Class.forName(className).nn.getField("MODULE$").nn.get(null).nn.asInstanceOf[Dialect]
    catch case _: Throwable =>
      halt(m"xenophile: could not load the grammar for the foreign source language")

  // Resolves the emission backend for a source language by reading the `Emission` member of its
  // `Ecosystem` — the fully-qualified names of `Materializer` objects — and loading the first that
  // is on the classpath. The reflective step is `dialectFor`'s, and for the same reason: `core`
  // must reach a backend it cannot depend on. Only the name differs, because a *type* would make
  // the ecosystem depend on the backend, and `xenophile.wasm` already depends on `xenophile.wit`.
  //
  // "The first that is on the classpath" is what makes the choice the build's. The C ecosystem
  // names two — Panama for the JVM, `CFuncPtr` for Scala Native — and no real classpath carries
  // both: `enigmatic.openssl` compiles one unchanged source both ways purely by swapping
  // `xenophile.native` for `xenophile.scalanative` in its `nativeModuleDeps`.
  private def materializerFor(using quotes: Quotes)(origin: quotes.reflect.TypeRepr): Materializer =
    import quotes.reflect.*

    val emission = origin.typeSymbol.typeMember("Emission")

    if !emission.exists
    then halt(m"xenophile: the source language names no materializer (it defines no `Emission`)")

    // A union of candidates flattens to a list; a single candidate is a list of one.
    def candidates(repr: TypeRepr): Seq[Text] = repr.dealias match
      case OrType(left, right)                => candidates(left) ++ candidates(right)
      case ConstantType(StringConstant(name)) => Seq(name.tt)

      case _ =>
        halt(m"xenophile: the source language's `Emission` is not a materializer's name")

    val names = origin.memberType(emission) match
      case TypeBounds(_, hi) => candidates(hi)
      case other             => candidates(other)

    // The singleton of a named object is the static `MODULE$` field of its `$`-suffixed module
    // class. A candidate that is simply absent is skipped — that is the other platform's backend,
    // which this build did not depend on.
    def load(name: Text): Optional[Materializer] =
      try
        val module = Class.forName(name.s+"$").nn
        module.getField("MODULE$").nn.get(null).nn.asInstanceOf[Materializer]
      catch case _: Throwable => Unset

    var found: Optional[Materializer] = Unset

    names.foreach: name =>
      if found.absent then found = load(name)

    found.or:
      halt(m"xenophile: no materializer for this foreign source language is on the classpath")

  // The single terminal for every ecosystem: let the backend the build selected emit the call.
  def invoke[result: Type](self: Expr[Foreign]): Macro[result] =
    val (_, origin) = receiver(self)
    materializerFor(origin).materialize[result](self)

  // Peels a navigation apart into the foreign type it selected from, the member it reached, the
  // receiver's own tree (which WIT resource methods thread as a handle) and the operand trees.
  //
  // Until the five backends were unified this was copied verbatim into each of them, and three
  // divergences had already crept in: Kotlin rejected the bare selection the others accepted, and
  // Wasm lacked the others' plain string-literal fallback. This takes the lenient reading of each.
  private[xenophile] def navigation(using quotes: Quotes)(self: Expr[Foreign])
  :   (Text, Text, quotes.reflect.Term, Seq[quotes.reflect.Term]) =

    import quotes.reflect.*

    // The navigation expands to `Foreign.make(<AST>).asInstanceOf[…]`, and reaching a materializer
    // through further `inline` definitions (an inline given publishing a deferred import, say)
    // nests it in `Inlined`/`Typed` layers that `underlyingArgument` cannot fold away. So the AST
    // is recovered by term-level stripping rather than by quote-pattern matching.
    def strip(term: Term): Term = term match
      case Inlined(_, _, body)                        => strip(body)
      case Typed(expr, _)                             => strip(expr)
      case Block(Nil, expr)                           => strip(expr)
      case TypeApply(Select(expr, "asInstanceOf"), _) => strip(expr)
      case _                                          => term

    def stringOf(term: Term): Text = strip(term).absolve match
      case Literal(StringConstant(string)) => string.tt

    // `.tt` desugars to `tt("…")`; recover the string from the operand, or from a bare literal.
    def literal(term: Term): Text = strip(term).absolve match
      case Apply(Ident("tt"), Seq(argument)) => stringOf(argument)
      case other                             => stringOf(other)

    def notCall: Nothing =
      halt(m"xenophile: `call` expects a foreign function invocation, `interface.function(…)`")

    val expression = strip(self.asTerm.underlyingArgument).absolve match
      case Apply(Select(_, "make"), Seq(argument)) => strip(argument)
      case _                                       => notCall

    // The elements of the `Expr.ofList`-built argument list of an `Expression.Apply`.
    def argumentList(term: Term): Seq[Term] = strip(term) match
      case Apply(_, Seq(varargs)) => strip(varargs) match
        case Repeated(elements, _) => elements.map(strip)
        case _                     => Seq()

      case _ =>
        Seq()

    // Either an applied call — `Expression.Apply(select, arguments)`, whose companion `apply`
    // takes two arguments — or the bare selection of a zero-parameter function
    // (`Expression.Select`, whose companion `apply` takes three): `interface.function.call[R]()`.
    // The latter is preferred inside `inline` definitions, where re-inlining an empty-varargs
    // application trips path-dependent type avoidance.
    val (selectNode, argumentTerms) = expression match
      case Apply(Select(_, "apply"), Seq(node, args)) => (strip(node), argumentList(args))
      case _                                          => (expression, Seq())

    selectNode.absolve match
      case Apply(Select(_, "apply"), Seq(receiver, member, owner)) =>
        (literal(owner), literal(member), strip(receiver), argumentTerms)

      case _ =>
        notCall

  // The Scala value wrapped by the `Foreign.converter` `Conversion` in a navigation operand: a
  // converted argument, or (for WIT) a resource-handle receiver.
  private[xenophile] def convertedValue(using quotes: Quotes)(term: quotes.reflect.Term)
  :   quotes.reflect.Term =

    import quotes.reflect.*
    var found: Optional[Term] = Unset

    val traverser = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
        case Apply(Select(qualifier, "apply"), Seq(value))
        if qualifier.tpe <:< TypeRepr.of[Conversion[Nothing, Foreign]] =>
          if found.absent then found = value

        case _ =>
          traverseTreeChildren(tree)(owner)

    traverser.traverseTree(term)(Symbol.spliceOwner)

    found.or:
      halt(m"xenophile: a foreign operand must be a Scala value with an `Interoperable` instance")

  // Reads the definitions resource at `path` and parses it with the grammar for `origin`.
  // Parsed definitions are cached by resource path for the lifetime of a compilation run, so that
  // navigating a chain like `foo.bar.qux` parses each resource only once instead of per access.
  private val parsed: scala.collection.mutable.HashMap[Text, Map[Text, Map[Text, Prototype]]] =
    scala.collection.mutable.HashMap()

  def definitions(using quotes: Quotes)(origin: quotes.reflect.TypeRepr, path: Text)
  :   Map[Text, Map[Text, Prototype]] =

    parsed.synchronized:
      parsed.at(path).or:
        val stream = Optional(getClass.getResourceAsStream(path.s)).or:
          halt(m"xenophile: could not read foreign definitions at $path on the classpath")

        val content = scala.io.Source.fromInputStream(stream).mkString.tt
        val result = dialectFor(origin).parse(content)
        parsed(path) = result

        result

  // Collects every `type X = …` member from a (possibly nested) refinement type into a map.
  private[xenophile] def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   scala.collection.immutable.Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => scala.collection.immutable.Map()

  // Reads the `Topic` (foreign type) and `Origin` (source language) from a `Foreign` receiver. The
  // topic is returned as a type, since it may be compound (a union, say) rather than a single name.
  private[xenophile] def receiver(using quotes: Quotes)(self: Expr[Foreign])
  :   (quotes.reflect.TypeRepr, quotes.reflect.TypeRepr) =

    import quotes.reflect.*

    val members = refinements(self.asTerm.tpe.widen).to(Map)

    val topic = members(t"Topic").or:
      halt(m"xenophile: the receiver is not a foreign type (it has no `Topic`)")

    val origin = members(t"Origin").or:
      halt(m"xenophile: the receiver does not record its source language (it has no `Origin`)")

    (topic, origin)

  // The single foreign type name of a topic, for navigation; compound topics (e.g. unions) have no
  // members to select, so they are rejected here.
  private[xenophile] def topicName(using quotes: Quotes)(topic: quotes.reflect.TypeRepr): Text =
    import quotes.reflect.*

    topic.absolve match
      case ConstantType(StringConstant(name)) => name.tt

      case _ =>
        halt(m"xenophile: a compound foreign type (such as a union) has no members to select")

  // The receiver's recorded definitions path: the `Locus` member of its refinement, if the
  // navigation began from a root that had an `Interface` in scope to record it.
  private[xenophile] def receiverLocus(using quotes: Quotes)(self: Expr[Foreign])
  :   Optional[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    refinements(self.asTerm.tpe.widen).to(Map)(t"Locus")

  // Summons the `Interface` given for a source language and reads its definitions path (`Locus`)
  // as the singleton path type, or `Unset` when no such `Interface` (or no path) is in scope.
  private[xenophile] def summonedLocus(using quotes: Quotes)(origin: quotes.reflect.TypeRepr)
  :   Optional[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    val interfaceType = Refinement(TypeRepr.of[Interface], "Form", TypeBounds(origin, origin))

    interfaceType.asType.absolve match
      case '[interface] => Expr.summon[interface] match
        case None => Unset

        case Some(found) =>
          val members = (refinements(found.asTerm.tpe) ++ refinements(found.asTerm.tpe.widen)).to(Map)
          members(t"Locus")

  // The definitions path carried by a `Locus` singleton type.
  private[xenophile] def locusText(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): Text =
    import quotes.reflect.*

    repr.absolve match
      case ConstantType(StringConstant(path)) => path.tt

      case _ =>
        halt(m"xenophile: the definitions path is not a string literal type")

  // The definitions path for a source language, from the summoned `Interface`; halts if none is
  // available.
  private[xenophile] def locusOf(using quotes: Quotes)(origin: quotes.reflect.TypeRepr): Text =
    locusText:
      summonedLocus(origin).or:
        halt(m"xenophile: no `Interface` with a definitions path is in scope")

  // Builds the type-level representation of a foreign type: a string-singleton for a named type, a
  // bare union for `Union`, and `constructor over (arguments…)` (the prepositional `over`, i.e.
  // `constructor { type Transport = (arguments…) }`) for a generic application.
  private def reprOf(using quotes: Quotes)(foreign: Foreign.Type): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    foreign match
      case Foreign.Type.Named(name) =>
        ConstantType(StringConstant(name.s))

      case Foreign.Type.Union(members) =>
        val reprs: List[TypeRepr] = members.map(reprOf)

        // `reduce` is total only on a `Populated` receiver; a parsed union always has members,
        // so the empty branch is unreachable.
        reprs.occupied.lay(TypeRepr.of[Nothing]): nonEmpty =>
          nonEmpty.reduce: (a, b) =>
            a.asType.absolve match
              case '[x] => b.asType.absolve match
                case '[y] => TypeRepr.of[x | y]

      case Foreign.Type.Applied(constructor, arguments) =>
        val ctor = ConstantType(StringConstant(constructor.s))

        // A single argument is left bare; multiple arguments are wrapped in a tuple.
        val argument = arguments.map(reprOf) match
          case List(single) =>
            single

          case reprs =>
            // A right fold, as a left fold over the reversed list: `fold` is left-associative.
            reprs.reverse.fold(TypeRepr.of[EmptyTuple]): (tail, head) =>
              head.asType.absolve match
                case '[head] => tail.asType.absolve match
                  case '[type tail <: Tuple; tail] => TypeRepr.of[head *: tail]

        Refinement(ctor, "Transport", TypeBounds(argument, argument))

  // Builds the refined type `Foreign of <topic> from <origin>`, recording the definitions path
  // as a third `Locus` member when it is known — so later navigation steps (and completion
  // engines, which cannot summon the `Interface`) can read it back from the type alone.
  private def foreignType(using quotes: Quotes)
    ( kind:   Foreign.Type,
      origin: quotes.reflect.TypeRepr,
      locus:  Optional[quotes.reflect.TypeRepr] )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val topicType = reprOf(kind)

    val base =
      Refinement
        ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(topicType, topicType)),
          "Origin",
          TypeBounds(origin, origin) )

    locus.lay(base): locusRepr => Refinement(base, "Locus", TypeBounds(locusRepr, locusRepr))

  // Builds the `Expression` for a single method argument. Every argument arrives as a `Foreign`
  // (either already, or converted from a Scala value at the call site by the `converter`
  // `Conversion`), so we need only check its foreign type (`Topic`) against the parameter type.
  private def argTree(using quotes: Quotes)
    ( arg: Expr[Foreign], paramType: Foreign.Type, method: Text )
  :   Expr[Foreign.Expression] =

    import quotes.reflect.*

    val paramTopic = reprOf(paramType)
    val argRepr = arg.asTerm.tpe.widen

    val argTopic = refinements(argRepr).to(Map)(t"Topic").or:
      halt(m"xenophile: the foreign type of an argument to $method is not known")

    // The `ok` arm topic of a `result<ok, err>` parameter, if it is one — so a value of that arm's
    // foreign type satisfies the parameter (the terminal materializer wraps it as an `Ok`), used
    // for `wasi:http`'s `response-outparam.set(result<outgoing-response, error-code>)`.
    val okArm: Optional[TypeRepr] = paramType match
      case Foreign.Type.Applied(constructor, ok :: _) if constructor.s == "result" => reprOf(ok)
      case _                                                                       => Unset

    // A raw C `Address` argument (topic `pointer`) satisfies *any* pointer-typed parameter
    // (`ptr<T>`, of which `char*`'s `string` special case is not one) — the C dialect checks
    // pointerness, not pointee identity, exactly as C itself does.
    val pointerOk: Boolean = paramType match
      case Foreign.Type.Applied(constructor, _) if constructor.s == "ptr" =>
        argTopic <:< reprOf(Foreign.Type.Named(t"pointer"))

      case _ =>
        false

    // Subsumption, not equality: a `string` (or a bare `none`) argument satisfies an
    // `option<string>` (`string|none`) parameter, and an `ok`-arm value a `result<…>` parameter.
    if argTopic <:< paramTopic || okArm.lay(false)(argTopic <:< _) || pointerOk then '{$arg.expr}
    else halt(m"xenophile: $method expects an argument of foreign type ${paramType.text}")

  // The member prototypes for `topic`: resolved on demand by a self-resolving dialect (which
  // records no `Locus`), or read from the definitions resource at the receiver's — or the
  // summoned `Interface`'s — locus.
  private[xenophile] def prototypes(using quotes: Quotes)
    ( self: Expr[Foreign], originRepr: quotes.reflect.TypeRepr, topic: Text )
  :   (Map[Text, Prototype], Optional[quotes.reflect.TypeRepr]) =

    val dialect = dialectFor(originRepr)

    if dialect.resolves then
      val members = dialect.resolve(topic).or:
        halt(m"xenophile: the foreign type $topic is not defined on the compile classpath")

      (members, Unset)
    else
      val locusRepr = receiverLocus(self).or:
        summonedLocus(originRepr).or:
          halt(m"xenophile: no `Interface` with a definitions path is in scope")

      val members = definitions(originRepr, locusText(locusRepr))(topic).or:
        halt(m"xenophile: the foreign type $topic is not defined")

      (members, locusRepr)

  def select(self: Expr[Foreign], field: Expr[String]): Macro[Foreign] =
    val fieldName = field.valueOrAbort.tt
    val (topicRepr, originRepr) = receiver(self)
    val topic = topicName(topicRepr)
    val (typeMembers, locusRepr) = prototypes(self, originRepr, topic)

    val signature = typeMembers(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    // A method with parameters cannot be bare-selected, but a zero-parameter method can: the
    // selection is typed by its result, so a terminal materializer (e.g. WIT `call`) can treat it
    // as a nullary call — avoiding an empty-varargs application, which trips path-dependent type
    // avoidance when the navigation is re-inlined from an enclosing `inline` definition.
    signature.parameters.let: parameters =>
      if !parameters.nil
      then halt(m"xenophile: $fieldName is a method of $topic and must be called with arguments")

    foreignType(signature.result, originRepr, locusRepr).asType.absolve match
      case '[type result <: Foreign; result] =>
        val member = Expr(fieldName.s)
        val owner = Expr(topic.s)
        val tree = '{Foreign.Expression.Select($self.expr, $member.tt, $owner.tt)}
        '{Foreign.make($tree).asInstanceOf[result]}

  // The array constructors whose element type indexing yields: WebIDL/`sequence`, `FrozenArray`,
  // TypeScript `Array`/`ReadonlyArray`, and WIT `list`. All are encoded by `reprOf` as
  // `<constructor> { type Transport = <element> }` (the prepositional `over`).
  private val arrayConstructors: Set[Text] =
    Set(t"sequence", t"FrozenArray", t"Array", t"ReadonlyArray", t"list")

  // Indexes into an array-typed foreign value: checks the receiver's `Topic` is one of the
  // `arrayConstructors` applied to a single element type, and yields a `Foreign` of that element.
  def index(self: Expr[Foreign], idx: Expr[Int]): Macro[Foreign] =
    import quotes.reflect.*

    val (topicRepr, originRepr) = receiver(self)
    val locus = receiverLocus(self)

    val element = topicRepr.dealias match
      case Refinement(parent, "Transport", TypeBounds(_, element)) => parent.dealias match
        case ConstantType(StringConstant(constructor))
        if arrayConstructors.has(constructor.tt) =>
          element

        case _ =>
          halt(m"xenophile: this foreign type is not an indexable array type")

      case _ =>
        halt(m"xenophile: this foreign type is not an indexable array type")

    val base =
      Refinement
        ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(element, element)),
          "Origin",
          TypeBounds(originRepr, originRepr) )

    val resultType = locus.lay(base): locusRepr =>
      Refinement(base, "Locus", TypeBounds(locusRepr, locusRepr))

    resultType.asType.absolve match
      case '[type result <: Foreign; result] =>
        val tree = '{Foreign.Expression.Index($self.expr, Foreign.Expression.Literal($idx))}
        '{Foreign.make($tree).asInstanceOf[result]}

  def applied(self: Expr[Foreign], field: Expr[String], arguments: Expr[Seq[Foreign]])
  :   Macro[Foreign] =

    val fieldName = field.valueOrAbort.tt
    val (topicRepr, originRepr) = receiver(self)
    val topic = topicName(topicRepr)
    val (typeMembers, locusRepr) = prototypes(self, originRepr, topic)

    val signature = typeMembers(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    val parameters = signature.parameters.or:
      halt(m"xenophile: $fieldName is not a method of $topic")

    val args = arguments match
      case Lifts.Varargs(exprs) => exprs

      case _ =>
        halt(m"xenophile: the arguments to $fieldName must be passed directly")

    if args.size != parameters.size then
      halt(m"xenophile: $fieldName expects ${parameters.size} arguments, not ${args.size}")

    val argTrees: List[Expr[Foreign.Expression]] = args.zip(parameters).map: (arg, paramType) =>
      argTree(arg, paramType, fieldName)

    val member = Expr(fieldName.s)
    val owner = Expr(topic.s)
    val target = '{Foreign.Expression.Select($self.expr, $member.tt, $owner.tt)}
    // `Expr.ofList` takes a stdlib `Seq`: the quotes API is the boundary.
    val tree = '{Foreign.Expression.Apply($target, ${Expr.ofList(argTrees.stdlib)})}

    foreignType(signature.result, originRepr, locusRepr).asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make($tree).asInstanceOf[result]}

  // Builds an `Interface { type Form = form; type Locus = <locus> }` value from the definitions
  // path's singleton type, shared by both `apply` overloads.
  private def interfaceOf[form: Type](using quotes: Quotes)(locusRepr: quotes.reflect.TypeRepr)
  :   Expr[Interface] =

    import quotes.reflect.*
    val formRepr = TypeRepr.of[form]

    val resultType =
      Refinement
        ( Refinement(TypeRepr.of[Interface], "Form", TypeBounds(formRepr, formRepr)),
          "Locus",
          TypeBounds(locusRepr, locusRepr) )

    resultType.asType.absolve match
      case '[type result <: Interface; result] =>
        '{(new Interface {}).asInstanceOf[result]}

  // The `Locative` overload: read the `Locus` singleton path type from the argument (a hellenism
  // `Resource`, or any other value carrying a `Locus`).
  def interface[form: Type](resource: Expr[Locative]): Macro[Interface] =
    import quotes.reflect.*

    val members = (refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)).to(Map)

    val locusRepr = members(t"Locus").or:
      halt(m"xenophile: the resource does not carry a singleton path type (it has no `Locus`)")

    interfaceOf[form](locusRepr)

  // The `String` overload: the path is given directly as a string literal, needing no hellenism.
  def interfaceFromPath[form: Type](path: Expr[String]): Macro[Interface] =
    import quotes.reflect.*

    val locus = path.value.getOrElse:
      halt(m"xenophile: the definitions path must be a string literal")

    interfaceOf[form](ConstantType(StringConstant(locus)))

  def root[name <: Label: Type, origin: Type]: Macro[Foreign] =
    import quotes.reflect.*

    val nameRepr = TypeRepr.of[name]

    val name = nameRepr.absolve match
      case ConstantType(StringConstant(string)) => string

      case _ =>
        halt(m"xenophile: the foreign type name must be a string literal type")

    // Record the definitions path in the root's type when an `Interface` is in scope here; a
    // root without one still navigates (each step summons the `Interface` afresh), but offers
    // no dynamic completions.
    val locus = summonedLocus(TypeRepr.of[origin])

    foreignType(Foreign.Type.Named(name.tt), TypeRepr.of[origin], locus).asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make(Foreign.Expression.Reference(${Expr(name)}.tt)).asInstanceOf[result]}
