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
import gossamer.*
import rudiments.*
import vacuous.*

// The macro engine behind `Facade`: resolves member accesses against `KotlinDialect`'s
// metadata, and materializes each access immediately as a direct JVM call on the unwrapped
// underlying value. Generic precision comes free of charge from the compiler: the receiver is
// cast to its full applied Scala type (`Pair[Text, Text]`), so member types substitute without
// consulting the metadata — which instead contributes what Java signatures cannot say: which
// members exist as Kotlin sees them (properties versus methods), and nullability, which turns
// results into `Optional`s. `kotlin.String` results become `Text`; other Kotlin-typed results
// wrap as further facades.
object KotlinFacade:
  // The full applied Scala type of the facade's underlying value, from its `Transport`.
  private def transport(using quotes: Quotes)(self: Expr[Facade]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    Xenophile.refinements(self.asTerm.tpe.widen).at(t"Transport").or:
      halt(m"xenophile: the facade does not record its underlying type")

  private def classNameOf(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): Text =
    // A nested Java class's owner may surface as the module class (`Regex$.Companion`);
    // normalizing to plain dots leaves `KotlinDialect` to derive the `$`-separated binary name.
    repr.dealias.classSymbol.map(_.fullName.replace("$.", ".").nn.tt).getOrElse:
      halt(m"xenophile: ${repr.show} does not name a class")

  // Renders a foreign type in Kotlin syntax — simple names, `?` for nullability — for
  // diagnostics that quote the API as its author declared it.
  private[xenophile] def kotlinType(tpe: Foreign.Type): Text = tpe match
    case Foreign.Type.Union(members) =>
      members.filter(_ != Foreign.Type.Named(t"null")) match
        case List(inner) if members.length == 2 => t"${kotlinType(inner)}?"
        case _                                  => members.map(kotlinType).join(t" | ")

    case Foreign.Type.Named(name) =>
      simple(name)

    case Foreign.Type.Applied(name, arguments) =>
      t"${simple(name)}<${arguments.map(kotlinType).join(t", ")}>"

  private def simple(name: Text): Text =
    if name.s.startsWith("#") then t"T"
    else name.s.substring(name.s.lastIndexOf('.') + 1).nn.tt

  private[xenophile] def rendered(name: Text, prototype: Prototype): Text =
    prototype.parameters.lay(t"val $name: ${kotlinType(prototype.result)}"): parameters =>
      t"fun $name(${parameters.map(kotlinType).join(t", ")}): ${kotlinType(prototype.result)}"

  // Near misses among the type's members, rendered in Kotlin syntax, for `did you mean`.
  private def similar(className: Text, name: Text): List[Text] =
    KotlinDialect.resolve(className).lay(Nil): members =>
      members.to(List).filter: (memberName, _) =>
        val left = memberName.s.toLowerCase.nn
        val right = name.s.toLowerCase.nn
        left.startsWith(right.take(3)) || right.startsWith(left.take(3))

      . sortBy(_(0).s).take(4).map(rendered)

  // Kotlin value classes mangle their members' JVM names (`-impl` hashes); such members are
  // not yet callable, and say so rather than failing obscurely downstream.
  private def mangled(using Quotes)(className: Text, member: KotlinDialect.JvmMember): Unit =
    if member.jvmName.s.contains("-") then
      val jvmName = member.jvmName
      halt(m"xenophile: $jvmName of $className involves an unsupported Kotlin value class")

  private def missing(using Quotes)(className: Text, name: Text): Nothing =
    similar(className, name) match
      case Nil =>
        halt(m"xenophile: $className has no member $name")

      case candidates =>
        val listed = candidates.join(t"; ")
        halt(m"xenophile: $className has no member $name; did you mean: $listed")

  // Whether the Kotlin-level result type is nullable, and its non-null form.
  private def denull(km: Foreign.Type): (Foreign.Type, Boolean) = km match
    case Foreign.Type.Union(members) =>
      members.filter(_ != Foreign.Type.Named(t"null")) match
        case List(inner) => (inner, members.contains(Foreign.Type.Named(t"null")))
        case _           => (km, false)

    case _ =>
      (km, false)

  // Strips the explicit-nulls artifacts from a Java-typed result so the facade's types are
  // definite: the runtime null case is handled by the `Optional` wrapping, driven by metadata.
  private def solid(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    repr.widen match
      case OrType(left, right) if right =:= TypeRepr.of[Null] => solid(left)
      case FlexibleType(underlying)                           => solid(underlying)
      case AnnotatedType(underlying, _)                       => solid(underlying)
      case other                                              => other

  // Refines a raw call into the facade-typed result: `kotlin.String` reads as `Text`, a
  // nullable result wraps as an `Optional`, primitives and substituted type parameters pass
  // through, and any other Kotlin-typed result wraps as a further `Facade`.
  // Wraps a raw value as a facade of `repr`, carrying the same function-typed `Selectable`
  // refinement `make` produces — so a facade *returned from a method call* also lets bare lambdas
  // infer against its own functional-interface methods, not only a freshly-constructed one.
  private def refinedFacade(using quotes: Quotes)
    ( value: Expr[Any], repr: quotes.reflect.TypeRepr )
  :   Expr[Any] =

    import quotes.reflect.*

    val facade = repr.asType.absolve match
      case '[u] => '{Facade[u]($value.asInstanceOf[u])}.asTerm

    // Cast to the refinement at term level: binding it as a type (`'[refined]`) would let its
    // function members' capture sets escape the nested quote.
    TypeApply(Select.unique(facade, "asInstanceOf"), List(Inferred(functionRefinement(repr))))
    . asExprOf[Any]

  private def refined(using quotes: Quotes)(call: quotes.reflect.Term, km: Foreign.Type)
  :   Expr[Any] =

    val (inner, nullable) = denull(km)

    def passthrough(name: Text): Boolean =
      name.s.startsWith("#") || Set(
          t"kotlin.Int", t"kotlin.Long", t"kotlin.Short", t"kotlin.Byte", t"kotlin.Boolean",
          t"kotlin.Double", t"kotlin.Float", t"kotlin.Char", t"kotlin.Unit", t"kotlin.Any",
          t"kotlin.Nothing").contains(name)

    val underlying = solid(call.tpe)

    if nullable then inner match
      case Foreign.Type.Named(t"kotlin.String" | t"kotlin.CharSequence") =>
        '{Optional(${call.asExpr}.asInstanceOf[Text | Null])}

      // A nullable primitive arrives boxed; absence maps to `Unset` and presence unboxes.
      case Foreign.Type.Named(t"kotlin.Int") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Integer | Null]).let(_.intValue)}

      case Foreign.Type.Named(t"kotlin.Long") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Long | Null]).let(_.longValue)}

      case Foreign.Type.Named(t"kotlin.Short") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Short | Null]).let(_.shortValue)}

      case Foreign.Type.Named(t"kotlin.Byte") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Byte | Null]).let(_.byteValue)}

      case Foreign.Type.Named(t"kotlin.Boolean") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Boolean | Null]).let(_.booleanValue)}

      case Foreign.Type.Named(t"kotlin.Double") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Double | Null]).let(_.doubleValue)}

      case Foreign.Type.Named(t"kotlin.Float") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Float | Null]).let(_.floatValue)}

      case Foreign.Type.Named(t"kotlin.Char") =>
        '{Optional(${call.asExpr}.asInstanceOf[java.lang.Character | Null]).let(_.charValue)}

      // A substituted type parameter is already the user's own Scala type: no facade wrapping.
      case Foreign.Type.Named(name) if passthrough(name) => underlying.asType.absolve match
        case '[u] => '{Optional(${call.asExpr}.asInstanceOf[u | Null])}

      case _ => underlying.asType.absolve match
        case '[u] =>
          '{Optional(${call.asExpr}.asInstanceOf[u | Null]).let { value => Facade[u](value) }}

    else inner match
      case Foreign.Type.Named(t"kotlin.String" | t"kotlin.CharSequence") =>
        '{${call.asExpr}.asInstanceOf[Text]}

      // Cast to the definite (null-stripped) type: Kotlin declares the result non-nullable,
      // which the Java view of the signature cannot express.
      case Foreign.Type.Named(name) if passthrough(name) => underlying.asType.absolve match
        case '[u] => '{${call.asExpr}.asInstanceOf[u]}

      case _ =>
        refinedFacade(call.asExpr, underlying)

  // The receiver, unwrapped and cast to its full applied Scala type, so that member selection
  // substitutes the class's type parameters (`getFirst` on a `Pair[Text, Text]` types as
  // `Text`) without consulting the metadata.
  private def receiver(using quotes: Quotes)
    ( self: Expr[Facade], repr: quotes.reflect.TypeRepr )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    repr.asType.absolve match
      case '[u] => '{$self.raw.asInstanceOf[u]}.asTerm

  // Whether an argument can satisfy a JVM parameter type, given Kotlin's view of the boundary:
  // a conforming value always can; `Text` satisfies any parameter that accepts a `String` or
  // `CharSequence`; a `Facade` satisfies a parameter its underlying type conforms to.
  private def satisfies(using quotes: Quotes)
    ( argument: quotes.reflect.Term, parameter: quotes.reflect.TypeRepr )
  :   Boolean =

    import quotes.reflect.*

    val target = solid(parameter)
    val textual = argument.tpe.widen <:< TypeRepr.of[Text]
    val stringy = TypeRepr.of[String] <:< target || target =:= TypeRepr.of[CharSequence]

    val transported =
      Xenophile.refinements(argument.tpe.widen).at(t"Transport").lay(false)(_ <:< target)

    val direct = argument.tpe.widen <:< target || (textual && stringy) || transported

    val functional = samCompatible(argument, target) || proxyCompatible(argument, target)
    val bridged = functional || collectionCompatible(argument.tpe.widen, target)

    direct || bridged

  // Whether a Scala collection argument can satisfy a Java collection parameter through an
  // `asJava` view (constant-time, no copy).
  private def collectionCompatible(using quotes: Quotes)
    ( argument: quotes.reflect.TypeRepr, target: quotes.reflect.TypeRepr )
  :   Boolean =

    import quotes.reflect.*

    val mapLike = target <:< TypeRepr.of[java.util.Map[?, ?]] && argument <:< TypeRepr.of[Map[?, ?]]
    val setLike = target <:< TypeRepr.of[java.util.Set[?]] && argument <:< TypeRepr.of[Set[?]]

    val seqLike =
      target <:< TypeRepr.of[java.util.Collection[?]] && argument <:< TypeRepr.of[Seq[?]]

    mapLike || setLike || seqLike

  private def collectionAdapted(using quotes: Quotes)
    ( argument: quotes.reflect.Term, target: quotes.reflect.TypeRepr )
  :   Optional[quotes.reflect.Term] =

    import quotes.reflect.*

    val tpe = argument.tpe.widen

    if !collectionCompatible(tpe, solid(target)) then Unset else
      val view =
        if tpe <:< TypeRepr.of[Map[?, ?]] then
          '{scala.jdk.javaapi.CollectionConverters.asJava(${argument.asExprOf[Map[?, ?]]})}
        else if tpe <:< TypeRepr.of[Set[?]] then
          '{scala.jdk.javaapi.CollectionConverters.asJava(${argument.asExprOf[Set[?]]})}
        else
          '{scala.jdk.javaapi.CollectionConverters.asJava(${argument.asExprOf[Seq[?]]})}

      TypeApply(Select.unique(view.asTerm, "asInstanceOf"), List(Inferred(solid(target))))

  // The arity of a Kotlin function type, when the parameter is one.
  // A Java functional interface's single abstract method and its arity, when the parameter is
  // one (`Runnable`, `View.OnClickListener`, `java.util.function.*`, …). Kotlin's `FunctionN`
  // types are handled separately, since their adaptation implements a known interface directly.
  private def isJavaFunctionalInterface(using quotes: Quotes)(symbol: quotes.reflect.Symbol)
  :   Boolean =

    import quotes.reflect.*
    val java = symbol.flags.is(Flags.Trait) && symbol.flags.is(Flags.JavaDefined)
    java && !symbol.fullName.startsWith("kotlin.jvm.functions.")

  private def javaSam(using quotes: Quotes)(target: quotes.reflect.TypeRepr)
  :   Optional[(quotes.reflect.Symbol, Int)] =

    import quotes.reflect.*

    // A Java method's parameter arrives as a flexible/nullable type (`Predicate[…] | Null`)
    // under explicit nulls; strip that first, or the interface's class symbol is invisible.
    solid(target).classSymbol match
      case Some(symbol) if isJavaFunctionalInterface(symbol) =>
        // A functional interface has one abstract method — but, as Java's own rule allows, not
        // counting public `Object` methods it redeclares (`Comparator` redeclares `equals`).
        val objectMethods = Set("equals", "hashCode", "toString")

        val abstractMethods = symbol.methodMembers.filter: method =>
          val deferred = method.flags.is(Flags.Deferred) && !method.flags.is(Flags.Synthetic)
          deferred && !objectMethods.contains(method.name)

        abstractMethods match
          case List(method) => (method, method.paramSymss.flatten.length)
          case _            => Unset

      case _ =>
        Unset

  // Whether a *typed* Scala lambda can satisfy a Java functional-interface parameter. (An
  // untyped lambda never reaches the macro: `Dynamic` gives arguments no expected type, so a
  // lambda's parameter types must be written — `(_: View) => …` — though a `() => …` lambda is
  // always fully typed.)
  private def proxyCompatible(using quotes: Quotes)
    ( argument: quotes.reflect.Term, target: quotes.reflect.TypeRepr )
  :   Boolean =

    import quotes.reflect.*

    javaSam(target).lay(false): (_, arity) =>
      arity match
        case 0 => argument.tpe.widen <:< TypeRepr.of[() => Any]
        case 1 => argument.tpe.widen <:< TypeRepr.of[Nothing => Any]
        case 2 => argument.tpe.widen <:< TypeRepr.of[(Nothing, Nothing) => Any]
        case _ => false

  // Adapts a typed Scala lambda to a Java functional interface through a dynamic proxy: the
  // proxy answers `equals`/`hashCode`/`toString` itself and forwards the single abstract method
  // to the lambda, unwrapping any facade result. The `Proxy` machinery costs a reflective
  // dispatch per invocation — negligible for callbacks like click listeners.
  private def proxyAdapted(using quotes: Quotes)
    ( argument: quotes.reflect.Term, target: quotes.reflect.TypeRepr )
  :   Optional[quotes.reflect.Term] =

    import quotes.reflect.*

    javaSam(target).let: (_, arity) =>
      val interface = solid(target)

      interface.asType.absolve match
        case '[i] =>
          val handler: Expr[(Array[Object | Null] | Null) => Object | Null] = arity match
            case 0 =>
              val lambda = argument.asExprOf[() => Any]
              '{_ => KotlinRuntime.dispatch($lambda())}

            case 1 =>
              val lambda = '{${argument.asExpr}.asInstanceOf[Any => Any]}
              '{args => KotlinRuntime.dispatch($lambda(args.nn(0)))}

            case 2 =>
              val lambda = '{${argument.asExpr}.asInstanceOf[(Any, Any) => Any]}
              '{args => KotlinRuntime.dispatch($lambda(args.nn(0), args.nn(1)))}

            case _ =>
              halt(m"xenophile: functional interfaces above arity 2 are not yet supported")

          // `classOf[Interface]` reaches the interface's runtime `Class` from its type, avoiding
          // any reconstruction of the JVM binary name (a nested interface such as
          // `View.OnClickListener` mangles to `View$OnClickListener`).
          val samClass = Literal(ClassOfConstant(interface)).asExprOf[Class[i]]

          val proxy =
            ' {
                val loader = $samClass.getClassLoader
                val invocations = KotlinRuntime.forwarder($handler)

                java.lang.reflect.Proxy.newProxyInstance(loader, Array($samClass), invocations)
                . asInstanceOf[i]
              }

          proxy.asTerm

  // The Scala function type a Java functional-interface parameter maps to. Refining a method's
  // parameter to this — rather than leaving it `Any` under `Dynamic` — is what lets an untyped
  // lambda argument infer its parameter types: the refinement supplies the expected type.
  private def samFunctionType(using quotes: Quotes)(target: quotes.reflect.TypeRepr)
  :   Optional[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    // Resolve the interface's own wildcard type arguments to concrete types first
    // (`Predicate<? super Text>` → `Predicate[Text]`), so the single abstract method's
    // signature substitutes cleanly instead of leaving an unusable `Predicate[…]#T` projection.
    // A wildcard collapses to its lower bound when it has one (the parameter position of a
    // consumer-like interface), otherwise its upper bound.
    def resolved(bound: TypeRepr): TypeRepr = bound match
      case TypeBounds(low, high) =>
        if low =:= TypeRepr.of[Nothing] then solid(high) else solid(low)

      case other =>
        solid(other)

    val concrete = solid(target).dealias match
      case AppliedType(constructor, args) => constructor.appliedTo(args.map(resolved))
      case other                          => other

    javaSam(concrete).let: (method, arity) =>
      concrete.memberType(method).absolve match
        case MethodType(_, paramTypes, resultType) =>
          val args = paramTypes.map(solid(_)) :+ solid(resultType)
          defn.FunctionClass(arity).typeRef.appliedTo(args)

        case _ =>
          Unset

  // Refines `Facade over T` with those of T's methods that take a single functional-interface
  // parameter, typing that parameter as a Scala function. A `Selectable` structural member: the
  // refinement gives the call site a real function expected type (so a bare lambda infers),
  // while runtime dispatch stays with `applyDynamic`, which adapts the function to the interface.
  // Overloaded names are skipped (a refinement cannot express two members of one name).
  private def functionRefinement(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    val base = Refinement(TypeRepr.of[Facade], "Transport", TypeBounds(repr, repr))

    repr.classSymbol.fold(base): classSymbol =>
      // The refinement's declared result type must equal what `applyDynamic` returns at runtime,
      // or the two disagree. That holds for `Unit` and primitive/boolean results (which map to
      // themselves); a method returning a mapped reference type (a `Facade`, `Text`, `Optional`)
      // is left to the `Dynamic` path, where a bare lambda argument still cannot infer.
      def plainResult(result: TypeRepr): Boolean =
        val mapped = solid(result)
        mapped =:= TypeRepr.of[Unit] || mapped <:< TypeRepr.of[AnyVal]

      // A method worth refining: it has at least one functional-interface parameter (so a lambda
      // argument benefits) and a plain result.
      def candidate(method: Symbol): Boolean =
        val usable = !method.flags.is(Flags.Synthetic) && !method.flags.is(Flags.Private)

        usable && (repr.memberType(method) match
          case MethodType(_, parameters, result) =>
            parameters.exists(samFunctionType(_).present) && plainResult(result)

          case _ =>
            false)

      // One method per name (overrides and bridges duplicate a name in `methodMembers`); among a
      // name's candidates take the fewest-parameter one — the public convenience overload,
      // rather than an internal arity-padded variant (`removeIf(p, from, to)`).
      def arity(method: Symbol): Int = repr.memberType(method) match
        case MethodType(_, parameters, _) => parameters.length
        case _                            => Int.MaxValue

      val candidates =
        classSymbol.methodMembers.filter(candidate).groupBy(_.name).values
        . map(_.minBy(arity(_))).to(List)

      // A functional-interface parameter becomes a Scala function (so a lambda there infers);
      // every other parameter stays `Any`, exactly as the `Dynamic` path accepts it (preserving
      // the argument adaptations — `Text` for `String`, facade unwrapping — for non-lambda args).
      def parameterType(parameter: TypeRepr): TypeRepr =
        samFunctionType(parameter).or(TypeRepr.of[Any])

      candidates.foldLeft(base): (accumulated, method) =>
        repr.memberType(method).absolve match
          case MethodType(names, parameters, result) =>
            val signature =
              MethodType(names)(_ => parameters.map(parameterType), _ => solid(result))

            Refinement(accumulated, method.name, signature)

          case _ =>
            accumulated

  private def samArity(using quotes: Quotes)(target: quotes.reflect.TypeRepr): Optional[Int] =
    target.classSymbol.map(_.fullName).getOrElse("") match
      case "kotlin.jvm.functions.Function0" => 0
      case "kotlin.jvm.functions.Function1" => 1
      case "kotlin.jvm.functions.Function2" => 2
      case _                                => Unset

  private def samCompatible(using quotes: Quotes)
    ( argument: quotes.reflect.Term, target: quotes.reflect.TypeRepr )
  :   Boolean =

    import quotes.reflect.*

    samArity(target).lay(false):
      case 0 => argument.tpe.widen <:< TypeRepr.of[() => Any]
      case 1 => argument.tpe.widen <:< TypeRepr.of[Nothing => Any]
      case 2 => argument.tpe.widen <:< TypeRepr.of[(Nothing, Nothing) => Any]
      case _ => false

  // Adapts a Scala lambda to a Kotlin function-type parameter by implementing the
  // corresponding `kotlin.jvm.functions.FunctionN` interface around it. Inputs wrap as
  // `Facade`s when the lambda declares facade parameters, and a facade result unwraps, so the
  // lambda works in facade terms while Kotlin receives raw values.
  private def samAdapted(using quotes: Quotes)
    ( argument: quotes.reflect.Term, target: quotes.reflect.TypeRepr )
  :   Optional[quotes.reflect.Term] =

    import quotes.reflect.*

    // The lambda's own declared parameter types, to decide which inputs expect facades.
    val declared: List[TypeRepr] = argument.tpe.widen.dealias match
      case AppliedType(_, arguments) => arguments.dropRight(1)
      case _                         => Nil

    def facaded(index: Int): Boolean =
      declared.lift(index).map(_ <:< TypeRepr.of[Facade]).getOrElse(false)

    def inward[p: Type](index: Int, value: Expr[p]): Expr[Any] =
      if facaded(index) then '{Facade[p]($value)} else value

    def outward(value: Expr[Any]): Expr[Any] =
      ' {
          $value.absolve match
            case facade: Facade => facade.raw
            case other          => other
        }

    // Kotlin emits use-site variance (`? super P, ? extends R`), which reflects as bounds
    // rather than types; each is replaced by its meaningful end so the interface type is
    // concrete enough to implement.
    val concrete = solid(target).dealias match
      case AppliedType(constructor, arguments) =>
        val flattened = arguments.map:
          case TypeBounds(low, high) =>
            if low =:= TypeRepr.of[Nothing] then solid(high) else solid(low)

          case argument =>
            solid(argument)

        AppliedType(constructor, flattened)

      case other =>
        other

    samArity(target).let: arity =>
      concrete.asType.absolve match
        case '[kotlin.jvm.functions.Function0[r]] if arity == 0 =>
          val lambda = argument.asExprOf[() => Any]

          ' {
              new kotlin.jvm.functions.Function0[r]:
                def invoke(): r | Null = ${outward('{$lambda()})}.asInstanceOf[r]
            }

          . asTerm

        case '[kotlin.jvm.functions.Function1[p, r]] if arity == 1 =>
          val lambda = '{${argument.asExpr}.asInstanceOf[Any => Any]}

          ' {
              new kotlin.jvm.functions.Function1[p, r]:
                def invoke(input: p | Null): r | Null =
                  ${outward('{$lambda(${inward[p](0, '{input.asInstanceOf[p]})})})}
                  . asInstanceOf[r]
            }

          . asTerm

        case '[kotlin.jvm.functions.Function2[p1, p2, r]] if arity == 2 =>
          val lambda = '{${argument.asExpr}.asInstanceOf[(Any, Any) => Any]}

          ' {
              new kotlin.jvm.functions.Function2[p1, p2, r]:
                def invoke(input1: p1 | Null, input2: p2 | Null): r | Null =
                  val first = ${inward[p1](0, '{input1.asInstanceOf[p1]})}
                  val second = ${inward[p2](1, '{input2.asInstanceOf[p2]})}
                  ${outward('{$lambda(first, second)})}.asInstanceOf[r]
            }

          . asTerm

        case _ =>
          Unset

  // The argument term as passed to the JVM method: facades unwrap, and anything not already
  // conforming (an opaque `Text` against `String`) is cast, which is free at runtime.
  private def adapted(using quotes: Quotes)
    ( argument: quotes.reflect.Term, parameter: quotes.reflect.TypeRepr )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    val sam =
      samAdapted(argument, solid(parameter))
      . or(proxyAdapted(argument, solid(parameter)))
      . or(collectionAdapted(argument, parameter))

    val unwrapped = sam.or:
      if argument.tpe.widen <:< TypeRepr.of[Facade]
      then '{${argument.asExprOf[Facade]}.raw}.asTerm
      else argument

    if unwrapped.tpe <:< parameter then unwrapped
    else TypeApply(Select.unique(unwrapped, "asInstanceOf"), List(Inferred(solid(parameter))))

  private def parameterTypes(using quotes: Quotes)
    ( repr: quotes.reflect.TypeRepr, method: quotes.reflect.Symbol )
  :   Optional[List[quotes.reflect.TypeRepr]] =

    import quotes.reflect.*

    repr.memberType(method) match
      case MethodType(_, types, _) => types
      case _                       => Unset

  def select(self: Expr[Facade], name: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val field = name.valueOrAbort.tt
    val repr = transport(self)
    val className = classNameOf(repr)

    val member = KotlinDialect.members(className, field).filter(_.property) match
      case member :: _ => member
      case Nil         => missing(className, field)

    val prototype = KotlinDialect.memberPrototype(className, field).or:
      missing(className, field)

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))

    // A `const`/`@JvmField` property is a bare field — static on the companion module for an
    // `object`'s members, or an instance field otherwise — read directly.
    if member.field then
      val static = classSymbol.companionModule.fieldMember(member.jvmName.s)

      val term =
        if static.exists then Select(Ref(classSymbol.companionModule), static) else
          val instance = classSymbol.fieldMember(member.jvmName.s)

          if instance.exists then Select(receiver(self, repr), instance)
          else halt(m"xenophile: no JVM field ${member.jvmName} on $className")

      refined(term, prototype.result)

    else
      mangled(className, member)

      val method =
        classSymbol.methodMember(member.jvmName.s)
        . filter(parameterTypes(repr, _) == Optional(Nil)) match
          case method :: _ => method
          case Nil         => halt(m"xenophile: no JVM getter ${member.jvmName} on $className")

      refined(Apply(Select(receiver(self, repr), method), Nil), prototype.result)

  def applied(self: Expr[Facade], name: Expr[String], arguments: Expr[Seq[Any]])(using Quotes)
  :   Expr[Any] =

    import quotes.reflect.*

    val field = name.valueOrAbort.tt
    val repr = transport(self)
    val className = classNameOf(repr)

    val args = arguments match
      case Varargs(exprs) => exprs.to(List).map(_.asTerm)
      case _              => halt(m"xenophile: the arguments must be passed directly")

    val candidates = KotlinDialect.members(className, field).filter: member =>
      !member.property && member.arity == args.length

    val prototype = KotlinDialect.memberPrototype(className, field).or:
      missing(className, field)

    // With no member of this arity, a member whose omitted trailing parameters all declare
    // defaults is called through its synthetic `$default` bridge instead.
    if candidates.isEmpty then
      val defaulted = KotlinDialect.members(className, field).filter: member =>
        val trailing = member.defaults.drop(args.length).forall(identity)
        !member.property && member.arity > args.length && trailing

      defaulted match
        case member :: _ =>
          val provided = args.zipWithIndex.map(_.swap).to(Map)
          return bridgeCall(self, repr, className, member, provided, prototype)

        case Nil =>
          // `facade.property(x)` arrives as one Dynamic application; when the member is a
          // property, it reads as property access followed by Kotlin's `operator fun get`.
          if KotlinDialect.members(className, field).exists(_.property) then
            val target = select(self, name)

            if target.asTerm.tpe.widen <:< TypeRepr.of[Facade]
            then return applied(target.asExprOf[Facade], Expr("get"), arguments)

          val declared = rendered(field, prototype)
          val count = args.length
          halt(m"xenophile: $className declares $declared, which does not take $count arguments")

    invocation(self, repr, className, field, args, prototype)

  // Resolves an ordered-argument call against the Scala view of the class and emits it.
  private def invocation(using Quotes)
    ( self:      Expr[Facade],
      repr:      quotes.reflect.TypeRepr,
      className: Text,
      field:     Text,
      args:      List[quotes.reflect.Term],
      prototype: Prototype )
  :   Expr[Any] =

    import quotes.reflect.*

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))

    val candidates = KotlinDialect.members(className, field).filter: member =>
      val fits = member.arity == args.length
      val variadic = member.vararg && args.length >= member.arity - 1
      !member.property && (fits || variadic)

    candidates.foreach(mangled(className, _))

    // Whether a parameter is the repeated (vararg) tail in the Scala view.
    def repeated(parameter: TypeRepr): Optional[TypeRepr] = parameter match
      case AnnotatedType(AppliedType(_, List(element)), annotation)
      if annotation.tpe.typeSymbol == defn.RepeatedAnnot =>
        element

      case AppliedType(constructor, List(element))
      if constructor.typeSymbol == defn.RepeatedParamClass =>
        element

      case _ =>
        Unset

    // Resolution against the *Scala view* of the class: among the metadata's candidates, the
    // methods whose (substituted) parameter types every argument satisfies; a vararg tail
    // absorbs the surplus arguments, each checked against the element type.
    val resolved = candidates.flatMap: member =>
      val real = classSymbol.methodMember(member.jvmName.s).filter: method =>
        !method.flags.is(Flags.Synthetic) && !method.flags.is(Flags.Artifact)

      real.flatMap: method =>
        parameterTypes(repr, method) match
          case types: List[TypeRepr] @unchecked =>
            types.lastOption.map(repeated).getOrElse(Unset) match
              case element: TypeRepr @unchecked =>
                val fixed = types.init

                val front = args.take(fixed.length).zip(fixed).forall: (argument, parameter) =>
                  satisfies(argument, parameter)

                val rest = args.drop(fixed.length).forall(satisfies(_, element))
                val fits = args.length >= fixed.length && front && rest

                if fits then List((method, types)) else Nil

              case _ =>
                val fits = args.zip(types).forall: (argument, parameter) =>
                  satisfies(argument, parameter)

                if types.length == args.length && fits then List((method, types)) else Nil

          case _ =>
            Nil

    // Each metadata candidate scans the same-named JVM methods, so a symbol satisfiable
    // through more than one candidate would repeat; overload identity is the symbol.
    val distinct = resolved.distinctBy(_(0))

    // Exact conformance outranks adaptation, so `Text` never makes two `String`-flavoured
    // overloads ambiguous when one matches the arguments as given.
    val exact = distinct.filter: (_, types) =>
      args.zip(types).forall: (argument, parameter) =>
        argument.tpe.widen <:< solid(parameter)

    val (method, types) = (if exact.nonEmpty then exact else distinct) match
      case List(one) => one

      case Nil =>
        val declared = rendered(field, prototype)
        val supplied = args.map(_.tpe.widen.show.tt).join(t", ")
        halt(m"xenophile: $className declares $declared, which cannot accept ($supplied)")

      case _ =>
        halt(m"xenophile: the call to $className.$field is ambiguous between overloads")

    val adaptedArgs = types.lastOption.map(repeated).getOrElse(Unset) match
      case element: TypeRepr @unchecked =>
        val fixed = types.init

        val front = args.take(fixed.length).zip(fixed).map: (argument, parameter) =>
          adapted(argument, parameter)

        // The element type may carry capture-set variables that a pure argument conforms to
        // but a retype outside the inference context rejects; every element is cast to the
        // bare class type, which is free at runtime.
        val target = element.dealias match
          case AppliedType(_, _) =>
            solid(element)

          case other =>
            other.classSymbol.map { symbol => symbol.typeRef: TypeRepr }.getOrElse(solid(element))

        val rest = args.drop(fixed.length).map: argument =>
          val unwrapped =
            if argument.tpe.widen <:< TypeRepr.of[Facade]
            then '{${argument.asExprOf[Facade]}.raw}.asTerm
            else argument

          TypeApply(Select.unique(unwrapped, "asInstanceOf"), List(Inferred(target)))

        val repeatedType = AppliedType(defn.RepeatedParamClass.typeRef, List(target))
        front :+ Typed(Repeated(rest, Inferred(target)), Inferred(repeatedType))

      case _ =>
        args.zip(types).map: (argument, parameter) =>
          adapted(argument, parameter)

    val call = Apply(Select(receiver(self, repr), method), adaptedArgs)

    refined(call, prototype.result)

  def construct[kotlinType: Type](arguments: Expr[Seq[Any]])(using Quotes): Expr[Any] =

    import quotes.reflect.*

    val repr = TypeRepr.of[kotlinType]
    val className = classNameOf(repr)

    val args = arguments match
      case Varargs(exprs) => exprs.to(List).map(_.asTerm)
      case _              => halt(m"xenophile: the arguments must be passed directly")

    // Ensures the class resolves as Kotlin (it has metadata) before construction.
    if KotlinDialect.resolve(className).absent
    then halt(m"xenophile: $className is not a Kotlin class on the compile classpath")

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))

    // A generic class's constructor is polymorphic in the class's own type parameters, which
    // `memberType` does not substitute; applying the constructor tree to the facade's type
    // arguments first makes the tree's own type carry the substituted parameter types.
    val targs = repr.dealias match
      case AppliedType(_, targs) => targs
      case _                     => Nil

    val constructors = classSymbol.declarations.filter(_.isClassConstructor).flatMap: method =>
      val selection = Select(New(Inferred(repr)), method)

      val application =
        if targs.isEmpty then selection else TypeApply(selection, targs.map(Inferred(_)))

      application.tpe.widen match
        case MethodType(_, types, _) =>
          val fits = args.zip(types).forall: (argument, parameter) =>
            satisfies(argument, parameter)

          if types.length == args.length && fits then List((application, types)) else Nil

        case _ =>
          Nil

    val (application, types) = constructors match
      case List(one) => one
      case one :: _  => one
      case Nil       => halt(m"xenophile: no constructor of $className accepts these arguments")

    val adaptedArgs = args.zip(types).map: (argument, parameter) =>
      adapted(argument, parameter)

    val created = Apply(application, adaptedArgs)

    // Refine the facade type with function-typed methods, so lambdas passed to them infer
    // (`list.removeIf(x => …)` with no ascription). The tree's `asInstanceOf[refined]` carries
    // the precise type out through `make`'s transparent inlining.
    functionRefinement(repr).asType.absolve match
      case '[refined] =>
        val facade = '{Facade[kotlinType](${created.asExprOf[Any]}.asInstanceOf[kotlinType])}
        '{$facade.asInstanceOf[refined]}

  def companion[kotlinType: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    val repr = TypeRepr.of[kotlinType]
    val className = classNameOf(repr)

    val name = KotlinDialect.companionName(className).or:
      halt(m"xenophile: $className has no companion object")

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))
    val field = classSymbol.companionModule.fieldMember(name.s)

    if !field.exists
    then halt(m"xenophile: the companion object of $className is not accessible")

    val term = Select(Ref(classSymbol.companionModule), field)

    solid(term.tpe).asType.absolve match
      case '[c] => '{Facade[c](${term.asExpr}.asInstanceOf[c])}

  // Calls a member through its synthetic `name$default` bridge: a static method taking the
  // receiver, every declared parameter (absent ones as zero values), a bitmask of the absent
  // positions, and a marker. The bridge is ACC_SYNTHETIC — invisible to the symbol table — so
  // the call goes through `KotlinRuntime`'s cached `MethodHandle` rather than a direct emission.
  private def bridgeCall(using Quotes)
    ( self:      Expr[Facade],
      repr:      quotes.reflect.TypeRepr,
      className: Text,
      member:    KotlinDialect.JvmMember,
      provided:  Map[Int, quotes.reflect.Term],
      prototype: Prototype )
  :   Expr[Any] =

    import quotes.reflect.*

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))
    val bridgeName = t"${member.jvmName}$$default"

    // The visible sibling: the real method, whose signature supplies the parameter types for
    // adaptation and zero-filling, and the precise result type.
    val method =
      classSymbol.methodMember(member.jvmName.s)
      . filter(parameterTypes(repr, _).let(_.length).or(-1) == member.arity) match
        case method :: _ => method
        case Nil         => halt(m"xenophile: no JVM method ${member.jvmName} on $className")

    val (types, result) = repr.memberType(method) match
      case MethodType(_, types, result) => (types, result)
      case _                            => halt(m"xenophile: ${member.jvmName} is unsupported")

    def zero(parameter: TypeRepr): Expr[Any] =
      if parameter <:< TypeRepr.of[Int] then '{0}
      else if parameter <:< TypeRepr.of[Long] then '{0L}
      else if parameter <:< TypeRepr.of[Boolean] then '{false}
      else if parameter <:< TypeRepr.of[Double] then '{0.0}
      else if parameter <:< TypeRepr.of[Float] then '{0.0f}
      else if parameter <:< TypeRepr.of[Short] then '{0.toShort}
      else if parameter <:< TypeRepr.of[Byte] then '{0.toByte}
      else if parameter <:< TypeRepr.of[Char] then '{0.toChar}
      else '{null}

    val values = types.zipWithIndex.map: (parameter, index) =>
      provided.at(index).let { argument => adapted(argument, parameter).asExpr }.or(zero(parameter))

    // Bit `i` set means parameter `i` takes its default.
    val mask: Int = types.indices.filter(!provided.contains(_)).foldLeft(0): (acc, index) =>
      acc | (1 << index)

    val ownerClass = Literal(ClassOfConstant(repr.dealias match
      case AppliedType(constructor, _) => constructor
      case other                       => other))

    val trailer = List(Expr[Int](mask).asExprOf[Any | Null], '{null}.asExprOf[Any | Null])
    val head = receiver(self, repr).asExprOf[Any | Null]
    val boxed = values.map(_.asExprOf[Any | Null])
    val arguments = Varargs[Any | Null](head :: boxed ::: trailer)

    val call: Expr[Any | Null] =
      ' {
          KotlinRuntime.invokeDefault
            ( ${ownerClass.asExprOf[Class[?]]},
              ${Expr(bridgeName.s)},
              Array[Any | Null]($arguments*) )
        }

    val cast = TypeApply(Select.unique(call.asTerm, "asInstanceOf"), List(Inferred(solid(result))))

    refined(cast, prototype.result)

  // Assignment to a `var` property: `facade.name = value` emits the Kotlin setter.
  def update(self: Expr[Facade], name: Expr[String], value: Expr[Any])(using Quotes)
  :   Expr[Unit] =

    import quotes.reflect.*

    val field = name.valueOrAbort.tt
    val repr = transport(self)
    val className = classNameOf(repr)

    val setter = KotlinDialect.setter(className, field).or:
      halt(m"xenophile: $className has no mutable property $field")

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))

    val method = classSymbol.methodMember(setter.jvmName.s) match
      case method :: _ => method
      case Nil         => halt(m"xenophile: no JVM setter ${setter.jvmName} on $className")

    val parameter = parameterTypes(repr, method).or(Nil) match
      case parameter :: _ => parameter
      case Nil            => halt(m"xenophile: ${setter.jvmName} is not a setter")

    val call = Apply(Select(receiver(self, repr), method), List(adapted(value.asTerm, parameter)))

    '{${call.asExpr}; ()}

  // The `facade(…) = value` form: Kotlin's `operator fun set`, its result discarded.
  def discarded(self: Expr[Facade], name: Expr[String], arguments: Expr[Seq[Any]])(using Quotes)
  :   Expr[Unit] =

    '{${applied(self, name, arguments)}; ()}

  // The facade of a Kotlin `object` singleton, through its static `INSTANCE` field.
  def singleton[kotlinType: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    val repr = TypeRepr.of[kotlinType]
    val className = classNameOf(repr)

    if KotlinDialect.resolve(className).absent
    then halt(m"xenophile: $className is not a Kotlin class on the compile classpath")

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))
    val field = classSymbol.companionModule.fieldMember("INSTANCE")

    if !field.exists then halt(m"xenophile: $className is not a Kotlin object")

    val term = Select(Ref(classSymbol.companionModule), field)

    '{Facade[kotlinType](${term.asExpr}.asInstanceOf[kotlinType])}

  // Named (and mixed) arguments: each named value is assigned to its declared parameter's
  // position; positional values fill the earliest unassigned positions. A full assignment
  // resolves as an ordinary call; missing positions must declare defaults, and route through
  // the `$default` bridge.
  def appliedNamed
    ( self: Expr[Facade], name: Expr[String], arguments: Expr[Seq[(String, Any)]] )
    ( using Quotes )
  :   Expr[Any] =

    import quotes.reflect.*

    val field = name.valueOrAbort.tt
    val repr = transport(self)
    val className = classNameOf(repr)

    val pairs: List[(Text, Term)] = arguments match
      case Varargs(exprs) => exprs.to(List).map:
        case '{($key: String, $value: v)} => (key.valueOrAbort.tt, value.asTerm)
        case other                        => (t"", other.asTerm)

      case _ =>
        halt(m"xenophile: the arguments must be passed directly")

    val prototype = KotlinDialect.memberPrototype(className, field).or:
      missing(className, field)

    val candidates = KotlinDialect.members(className, field).filter: member =>
      !member.property && member.parameters.nonEmpty

    val member = candidates match
      case member :: _ => member
      case Nil         => halt(m"xenophile: $className.$field takes no named arguments")

    val positions: Map[Text, Int] = member.parameters.zipWithIndex.to(Map)

    val provided: Map[Int, Term] =
      def assign(pairs: List[(Text, Term)], next: Int, acc: Map[Int, Term]): Map[Int, Term] =
        pairs match
          case Nil =>
            acc

          case (t"", term) :: rest =>
            val index = (0 until member.arity).find(!acc.contains(_)).getOrElse:
              halt(m"xenophile: too many arguments for $className.$field")

            assign(rest, next, acc.updated(index, term))

          case (key, term) :: rest =>
            val index = positions.at(key).or:
              val declared = member.parameters.join(t", ")
              halt(m"xenophile: $className.$field has no parameter $key; it declares: $declared")

            assign(rest, next, acc.updated(index, term))

      assign(pairs, 0, Map())

    val absent = (0 until member.arity).filter(!provided.contains(_)).to(List)

    if absent.isEmpty then
      val ordered = (0 until member.arity).to(List).map(provided(_))
      invocation(self, repr, className, field, ordered, prototype)
    else
      val undefaulted = absent.filter: index =>
        !member.defaults.lift(index).getOrElse(false)

      if undefaulted.nonEmpty then
        val names = undefaulted.map { index => member.parameters(index) }.join(t", ")
        halt(m"xenophile: $className.$field requires arguments for: $names")

      bridgeCall(self, repr, className, member, provided, prototype)

  // The facade of an `enum class` entry, a static field of the enum's own type.
  def entry[kotlinType: Type](name: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val repr = TypeRepr.of[kotlinType]
    val className = classNameOf(repr)
    val entryName = name.valueOrAbort.tt

    val entries = KotlinDialect.enumEntries(className)

    if entries.nonEmpty && !entries.contains(entryName) then
      val listed = entries.join(t", ")
      halt(m"xenophile: $className has no entry $entryName; its entries are: $listed")

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))
    val field = classSymbol.companionModule.fieldMember(entryName.s)

    if !field.exists then halt(m"xenophile: $className has no entry $entryName")

    val term = Select(Ref(classSymbol.companionModule), field)

    '{Facade[kotlinType](${term.asExpr}.asInstanceOf[kotlinType])}

  // A data class's components, as a Scala tuple with each component's facade-refined type.
  def tuple(self: Expr[Facade])(using Quotes): Expr[Any] =
    val repr = transport(self)
    val className = classNameOf(repr)

    def components(index: Int, acc: List[Expr[Any]]): List[Expr[Any]] =
      val name = t"component$index"

      if KotlinDialect.members(className, name).exists(!_.property)
      then components(index + 1, applied(self, Expr(name.s), Varargs(Nil)) :: acc)
      else acc.reverse

    components(1, Nil) match
      case Nil        => halt(m"xenophile: $className declares no components to destructure")
      case components => Expr.ofTupleFromSeq(components)

  // Copies a Kotlin/Java collection out to the corresponding immutable Scala collection, with
  // `String` elements reading as `Text`. The copy is explicit: facades themselves stay
  // zero-cost views.
  def scalaCollection(self: Expr[Facade])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val repr = transport(self)

    def textual(element: TypeRepr): TypeRepr =
      if solid(element) <:< TypeRepr.of[String] then TypeRepr.of[Text] else solid(element)

    val unwrapped = '{$self.raw}

    repr.dealias match
      case AppliedType(constructor, List(element)) if repr <:< TypeRepr.of[java.util.List[?]] =>
        textual(element).asType.absolve match
          case '[e] =>
            ' {
                List.from[e]:
                  scala.jdk.javaapi.CollectionConverters
                  . asScala($unwrapped.asInstanceOf[java.util.List[e]])
              }

      case AppliedType(constructor, List(element)) if repr <:< TypeRepr.of[java.util.Set[?]] =>
        textual(element).asType.absolve match
          case '[e] =>
            ' {
                Set.from[e]:
                  scala.jdk.javaapi.CollectionConverters
                  . asScala($unwrapped.asInstanceOf[java.util.Set[e]])
              }

      case AppliedType(constructor, List(key, value))
      if repr <:< TypeRepr.of[java.util.Map[?, ?]] =>
        (textual(key).asType, textual(value).asType).absolve match
          case ('[k], '[v]) =>
            ' {
                Map.from[k, v]:
                  scala.jdk.javaapi.CollectionConverters
                  . asScala($unwrapped.asInstanceOf[java.util.Map[k, v]])
              }

      case _ =>
        halt(m"xenophile: ${repr.show} is not a collection type")

  // The underlying value at the facade's full recorded Scala type, so handing it to plain
  // Java-level code needs no cast.
  def unwrapped(self: Expr[Facade])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val transport = Xenophile.refinements(self.asTerm.tpe.widen).at(t"Transport")

    if transport.absent then '{$self.raw} else
      transport.vouch.asType.absolve match
        case '[u] => '{$self.raw.asInstanceOf[u]}
