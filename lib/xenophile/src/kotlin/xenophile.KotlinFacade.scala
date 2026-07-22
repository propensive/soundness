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
import prepositional.*
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
  private def kotlinType(tpe: Foreign.Type): Text = tpe match
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

  private def rendered(name: Text, prototype: Prototype): Text =
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
      case other                                              => other

  // Refines a raw call into the facade-typed result: `kotlin.String` reads as `Text`, a
  // nullable result wraps as an `Optional`, primitives and substituted type parameters pass
  // through, and any other Kotlin-typed result wraps as a further `Facade`.
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

      case Foreign.Type.Named(name) if passthrough(name) =>
        halt(m"xenophile: nullable primitive results are not yet supported")

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

      case _ => underlying.asType.absolve match
        case '[u] => '{Facade[u](${call.asExpr}.asInstanceOf[u])}

  // The receiver, unwrapped and cast to its full applied Scala type, so that member selection
  // substitutes the class's type parameters (`getFirst` on a `Pair[Text, Text]` types as
  // `Text`) without consulting the metadata.
  private def receiver(using quotes: Quotes)
    ( self: Expr[Facade], repr: quotes.reflect.TypeRepr )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    repr.asType.absolve match
      case '[u] => '{$self.unwrap.asInstanceOf[u]}.asTerm

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

    argument.tpe.widen <:< target || (textual && stringy) || transported

  // The argument term as passed to the JVM method: facades unwrap, and anything not already
  // conforming (an opaque `Text` against `String`) is cast, which is free at runtime.
  private def adapted(using quotes: Quotes)
    ( argument: quotes.reflect.Term, parameter: quotes.reflect.TypeRepr )
  :   quotes.reflect.Term =

    import quotes.reflect.*

    val unwrapped =
      if argument.tpe.widen <:< TypeRepr.of[Facade]
      then '{${argument.asExprOf[Facade]}.unwrap}.asTerm
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

    val method =
      repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))
      . methodMember(member.jvmName.s)
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

    if candidates.isEmpty then
      val declared = rendered(field, prototype)
      val count = args.length
      halt(m"xenophile: $className declares $declared, which does not take $count arguments")

    val classSymbol = repr.classSymbol.getOrElse(halt(m"xenophile: not a class type"))

    // Resolution against the *Scala view* of the class: among the metadata's candidates, the
    // methods whose (substituted) parameter types every argument satisfies.
    val resolved = candidates.flatMap: member =>
      val real = classSymbol.methodMember(member.jvmName.s).filter: method =>
        !method.flags.is(Flags.Synthetic) && !method.flags.is(Flags.Artifact)

      real.flatMap: method =>
        parameterTypes(repr, method) match
          case types: List[TypeRepr] @unchecked =>
            val fits = args.zip(types).forall: (argument, parameter) =>
              satisfies(argument, parameter)

            if types.length == args.length && fits then List((method, types)) else Nil

          case _ =>
            Nil

    // Exact conformance outranks adaptation, so `Text` never makes two `String`-flavoured
    // overloads ambiguous when one matches the arguments as given.
    val exact = resolved.filter: (_, types) =>
      args.zip(types).forall: (argument, parameter) =>
        argument.tpe.widen <:< solid(parameter)

    val (method, types) = (if exact.nonEmpty then exact else resolved) match
      case List(one) => one

      case Nil =>
        val declared = rendered(field, prototype)
        val supplied = args.map(_.tpe.widen.show.tt).join(t", ")
        halt(m"xenophile: $className declares $declared, which cannot accept ($supplied)")

      case _ =>
        halt(m"xenophile: the call to $className.$field is ambiguous between overloads")

    val adaptedArgs = args.zip(types).map: (argument, parameter) =>
      adapted(argument, parameter)

    val call = Apply(Select(receiver(self, repr), method), adaptedArgs)

    refined(call, prototype.result)

  def construct[kotlinType: Type](arguments: Expr[Seq[Any]])(using Quotes)
  :   Expr[Facade over kotlinType] =

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

    '{Facade[kotlinType](${created.asExprOf[Any]}.asInstanceOf[kotlinType])}

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
