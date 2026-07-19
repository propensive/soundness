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
import gigantism.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Xenophile:

  // Resolves the grammar for a source language by reading the `Grammar` type member of its
  // `Ecosystem` — the singleton type of a `Dialect` object — and loading that object reflectively
  // at compile time. This keeps `core` free of any dependency on the ecosystems (sibling modules).
  private def dialectFor(using quotes: Quotes)(origin: quotes.reflect.TypeRepr): Dialect =
    import quotes.reflect.*

    val grammar = origin.typeSymbol.typeMember("Grammar")

    if !grammar.exists
    then halt(m"xenophile: the source language does not define a `Grammar`")

    val dialectType = origin.memberType(grammar) match
      case TypeBounds(_, hi) => hi
      case other             => other

    // The symbol's full name is the JVM binary name of the module class (already `$`-suffixed); its
    // singleton is the static `MODULE$` field. `Class.forName/1` uses the macro's own classloader.
    val className = dialectType.typeSymbol.fullName

    try Class.forName(className).nn.getField("MODULE$").nn.get(null).nn.asInstanceOf[Dialect]
    catch case _: Throwable =>
      halt(m"xenophile: could not load the grammar for the foreign source language")

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
  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

  // Reads the `Topic` (foreign type) and `Origin` (source language) from a `Foreign` receiver. The
  // topic is returned as a type, since it may be compound (a union, say) rather than a single name.
  private[xenophile] def receiver(using quotes: Quotes)(self: Expr[Foreign])
  :   (quotes.reflect.TypeRepr, quotes.reflect.TypeRepr) =

    import quotes.reflect.*

    val members = refinements(self.asTerm.tpe.widen)

    val topic = members.at(t"Topic").or:
      halt(m"xenophile: the receiver is not a foreign type (it has no `Topic`)")

    val origin = members.at(t"Origin").or:
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

  // Summons the `Interface` given for a source language and reads its definitions path (`Locus`).
  private[xenophile] def locusOf(using quotes: Quotes)(origin: quotes.reflect.TypeRepr): Text =
    import quotes.reflect.*

    val interfaceType = Refinement(TypeRepr.of[Interface], "Form", TypeBounds(origin, origin))

    val interfaceTerm = interfaceType.asType.absolve match
      case '[interface] =>
        val found = Expr.summon[interface].getOrElse:
          halt(m"xenophile: no `Interface` is available for the foreign source language")

        found.asTerm

    val members = refinements(interfaceTerm.tpe) ++ refinements(interfaceTerm.tpe.widen)

    val locusRepr = members.at(t"Locus").or:
      halt(m"xenophile: the `Interface` does not specify a definitions path (it has no `Locus`)")

    locusRepr.absolve match
      case ConstantType(StringConstant(path)) => path.tt

      case _ =>
        halt(m"xenophile: the definitions path is not a string literal type")

  // Builds the type-level representation of a foreign type: a string-singleton for a named type, a
  // bare union for `Union`, and `constructor over (arguments…)` (the prepositional `over`, i.e.
  // `constructor { type Transport = (arguments…) }`) for a generic application.
  private def reprOf(using quotes: Quotes)(foreign: Foreign.Type): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    foreign match
      case Foreign.Type.Named(name) =>
        ConstantType(StringConstant(name.s))

      case Foreign.Type.Union(members) =>
        members.map(reprOf).reduce: (a, b) =>
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
            reprs.foldRight(TypeRepr.of[EmptyTuple]): (head, tail) =>
              head.asType.absolve match
                case '[head] => tail.asType.absolve match
                  case '[type tail <: Tuple; tail] => TypeRepr.of[head *: tail]

        Refinement(ctor, "Transport", TypeBounds(argument, argument))

  // Builds the refined type `Foreign of <topic> from <origin>`.
  private def foreignType(using quotes: Quotes)(kind: Foreign.Type, origin: quotes.reflect.TypeRepr)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val topicType = reprOf(kind)

    Refinement
      ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(topicType, topicType)),
        "Origin",
        TypeBounds(origin, origin) )

  // Builds the `Expression` for a single method argument. Every argument arrives as a `Foreign`
  // (either already, or converted from a Scala value at the call site by the `converter`
  // `Conversion`), so we need only check its foreign type (`Topic`) against the parameter type.
  private def argTree(using quotes: Quotes)
    ( arg: Expr[Foreign], paramType: Foreign.Type, method: Text )
  :   Expr[Foreign.Expression] =

    import quotes.reflect.*

    val paramTopic = reprOf(paramType)
    val argRepr = arg.asTerm.tpe.widen

    val argTopic = refinements(argRepr).at(t"Topic").or:
      halt(m"xenophile: the foreign type of an argument to $method is not known")

    // The `ok` arm topic of a `result<ok, err>` parameter, if it is one — so a value of that arm's
    // foreign type satisfies the parameter (the terminal materializer wraps it as an `Ok`), used
    // for `wasi:http`'s `response-outparam.set(result<outgoing-response, error-code>)`.
    val okArm: Optional[TypeRepr] = paramType match
      case Foreign.Type.Applied(constructor, ok :: _) if constructor.s == "result" => reprOf(ok)
      case _                                                                       => Unset

    // A raw C `Pointer` argument (topic `pointer`) satisfies *any* pointer-typed parameter
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

  def select(self: Expr[Foreign], field: Expr[String]): Macro[Foreign] =
    val fieldName = field.valueOrAbort.tt
    val (topicRepr, originRepr) = receiver(self)
    val topic = topicName(topicRepr)

    val typeMembers = definitions(originRepr, locusOf(originRepr)).at(topic).or:
      halt(m"xenophile: the foreign type $topic is not defined")

    val signature = typeMembers.at(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    // A method with parameters cannot be bare-selected, but a zero-parameter method can: the
    // selection is typed by its result, so a terminal materializer (e.g. WIT `invoke`) can treat it
    // as a nullary call — avoiding an empty-varargs application, which trips path-dependent type
    // avoidance when the navigation is re-inlined from an enclosing `inline` definition.
    signature.parameters.let: parameters =>
      if !parameters.isEmpty
      then halt(m"xenophile: $fieldName is a method of $topic and must be called with arguments")

    foreignType(signature.result, originRepr).asType.absolve match
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

    val element = topicRepr.dealias match
      case Refinement(parent, "Transport", TypeBounds(_, element)) => parent.dealias match
        case ConstantType(StringConstant(constructor))
        if arrayConstructors.contains(constructor.tt) =>
          element

        case _ =>
          halt(m"xenophile: this foreign type is not an indexable array type")

      case _ =>
        halt(m"xenophile: this foreign type is not an indexable array type")

    val resultType =
      Refinement
        ( Refinement(TypeRepr.of[Foreign], "Topic", TypeBounds(element, element)),
          "Origin",
          TypeBounds(originRepr, originRepr) )

    resultType.asType.absolve match
      case '[type result <: Foreign; result] =>
        val tree = '{Foreign.Expression.Index($self.expr, Foreign.Expression.Literal($idx))}
        '{Foreign.make($tree).asInstanceOf[result]}

  def applied(self: Expr[Foreign], field: Expr[String], arguments: Expr[Seq[Foreign]])
  :   Macro[Foreign] =

    val fieldName = field.valueOrAbort.tt
    val (topicRepr, originRepr) = receiver(self)
    val topic = topicName(topicRepr)

    val typeMembers = definitions(originRepr, locusOf(originRepr)).at(topic).or:
      halt(m"xenophile: the foreign type $topic is not defined")

    val signature = typeMembers.at(fieldName).or:
      halt(m"xenophile: the foreign type $topic has no member $fieldName")

    val parameters = signature.parameters.or:
      halt(m"xenophile: $fieldName is not a method of $topic")

    val args = arguments match
      case Varargs(exprs) => exprs.to(List)

      case _ =>
        halt(m"xenophile: the arguments to $fieldName must be passed directly")

    if args.length != parameters.length then
      halt(m"xenophile: $fieldName expects ${parameters.length} arguments, not ${args.length}")

    val argTrees: List[Expr[Foreign.Expression]] = args.zip(parameters).map: (arg, paramType) =>
      argTree(arg, paramType, fieldName)

    val member = Expr(fieldName.s)
    val owner = Expr(topic.s)
    val target = '{Foreign.Expression.Select($self.expr, $member.tt, $owner.tt)}
    val tree = '{Foreign.Expression.Apply($target, ${Expr.ofList(argTrees)})}

    foreignType(signature.result, originRepr).asType.absolve match
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

    val members = refinements(resource.asTerm.tpe) ++ refinements(resource.asTerm.tpe.widen)

    val locusRepr = members.at(t"Locus").or:
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

    foreignType(Foreign.Type.Named(name.tt), TypeRepr.of[origin]).asType.absolve match
      case '[type result <: Foreign; result] =>
        '{Foreign.make(Foreign.Expression.Reference(${Expr(name)}.tt)).asInstanceOf[result]}
