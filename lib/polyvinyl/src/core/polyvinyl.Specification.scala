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
package polyvinyl

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import rudiments.map
import vacuous.*

object Specification:
  def record[self: Type, origin: Type, form: Type](value: Expr[origin])(using Quotes)
  :   Expr[Record] =

    val (target, specification) = locate[self, origin, form]
    val (refined, transform) = Expansion[origin, form](target).record(specification.fields)

    refined.absolve match
      case '[type refined <: Record; refined] =>
        '{$target.build($value, $transform).asInstanceOf[refined]}

  def tuple[self: Type, origin: Type, form: Type](value: Expr[origin])(using Quotes)
  :   Expr[NamedTuple.AnyNamedTuple] =

    val (target, specification) = locate[self, origin, form]
    val (tuple, make) = Expansion[origin, form](target).tuple(specification.fields)

    tuple.absolve match
      case '[type tuple <: NamedTuple.AnyNamedTuple; tuple] => '{$make($value).asInstanceOf[tuple]}

  // The specification object whose type is `self`: as a reference for the generated code, and as
  // an instance, loaded through the macro's own classloader, for its `fields`
  private def locate[self: Type, origin: Type, form: Type](using Quotes)
  :   (Expr[Specification in form from origin], Specification) =

    import quotes.reflect.*

    // `self` is the receiver's type: a reference to the object itself, or — the inliner binds
    // the receiver to a proxy — a reference to a value of the object's module class
    val repr = TypeRepr.of[self]
    val widened = repr.widenTermRefByName

    def isModule(symbol: Symbol): Boolean = symbol.exists && symbol.flags.is(Flags.Module)

    val module =
      if isModule(repr.termSymbol) then repr.termSymbol
      else if isModule(widened.typeSymbol) then widened.typeSymbol.companionModule
      else Symbol.noSymbol

    if !isModule(module) then
      val shown = repr.show.tt
      halt(m"record and tuple can only be called on a specification object, not $shown")

    val target = Ref(module).asExprOf[Specification in form from origin]

    // The object's binary name: `pkg.Outer$Inner$`
    def binaryName(symbol: Symbol): String =
      val owner = symbol.owner
      val simple = symbol.name.stripSuffix("$")
      if owner.isPackageDef then s"${owner.fullName}.$simple" else s"${binaryName(owner)}$$$simple"

    val name = binaryName(module) + "$"

    // The macro's own classloader sees the compilation classpath, including the output of the
    // files already compiled. When the object is defined in the current run, its class does not
    // exist yet: the `ClassNotFoundException` is left to propagate, since the compiler answers
    // one naming a class of the current run by suspending this unit and compiling it again once
    // the others are emitted — the ordering the "compilation order" note describes.
    val classloader = Specification.getClass.getClassLoader

    val instance =
      Class.forName(name, true, classloader).nn.getField("MODULE$").nn.get(null)
      . asInstanceOf[Specification]

    (target, instance)

  // The macro expansion shared by both modes. Each field becomes an accessor from the origin
  // value to the field's value, and a type; the modes differ only in how the accessors and types
  // are assembled, and in what a nested `Member.Record` becomes. Its `Quotes` is its own path, so
  // the types it produces leave as `Type[?]`s; internally, the lists are the compiler's own.
  private class Expansion[origin: Type, form: Type]
    ( target: Expr[Specification in form from origin] )
    ( using Quotes ):

    import quotes.reflect.*

    private case class Field(name: Text, tpe: TypeRepr, accessor: Expr[origin -> Any])

    private type Instance = Intensional in form from origin
    private type FallibleInstance = Intensional.Fallible in form from origin

    private def missing(name: Text, label: Text): Nothing =
      halt(m"could not find an Intensional instance for the field $name with type $label")

    def record(fields: List[(Text, Member)]): (Type[?], Expr[Text -> origin -> Any]) =
      val (refined, transform) = expandRecord(fields)
      (refined.asType, transform)

    def tuple(fields: List[(Text, Member)]): (Type[?], Expr[origin -> Any]) =
      val (tuple, make) = expandTuple(fields)
      (tuple.asType, make)

    private val consCtor = TypeRepr.of[Any *: EmptyTuple].absolve match
      case AppliedType(tycon, _) => tycon

    private val tacticSymbol = TypeRepr.of[Tactic[Hazard]].typeSymbol

    private def tupleType(elements: scala.List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple]): (head, tail) =>
        consCtor.appliedTo(scala.List(head, tail))

    private def expandRecord(fields: List[(Text, Member)])
    :   (TypeRepr, Expr[Text -> origin -> Any]) =

      val members = fields.stdlib.map(expand(_, eager = false, nested))

      // The record's `origin` is fixed too, so its `data` has the origin type at the call site
      val refined = members.foldLeft(TypeRepr.of[Record from origin]): (refined, field) =>
        Refinement(refined, field.name.s, field.tpe)

      val caseDefs = members.map: field =>
        CaseDef(Literal(StringConstant(field.name.s)), None, field.accessor.asTerm)

      // Unreachable through a refined record, whose fields are exactly the cases; reachable
      // through `selectDynamic` with an undeclared name
      def fallback(name: Expr[Text]): CaseDef =
        val missing: Expr[Nothing] =
          ' {
              val field: Text = $name
              panic(m"the record has no field named $field")
            }

        CaseDef(Wildcard(), None, missing.asTerm)

      val transform: Expr[Text -> origin -> Any] =
        ' {
            (name: Text) =>
              ${Match('name.asTerm, caseDefs :+ fallback('name)).asExprOf[origin => Any]}
          }

      (refined, transform)

    // A nested record: built like a top-level one, from the nested origin value
    private def nested(fields: List[(Text, Member)]): (TypeRepr, Expr[origin -> Any]) =
      val (refined, transform) = expandRecord(fields)
      (refined, '{field => $target.build(field, $transform)})

    private def expandTuple(fields: List[(Text, Member)]): (TypeRepr, Expr[origin -> Any]) =
      val members = fields.stdlib.map(expand(_, eager = true, expandTuple))
      val names = tupleType(members.map { field => ConstantType(StringConstant(field.name.s)) })
      val values = tupleType(members.map(_.tpe))
      val tuple = TypeRepr.of[NamedTuple.NamedTuple].appliedTo(scala.List(names, values))

      val elements: scala.List[Expr[origin -> Object]] = members.map: field =>
        '{data => ${field.accessor}(data).asInstanceOf[Object]}

      val make: Expr[origin -> Any] =
        ' {
            data =>
              val array: scala.Array[Object] =
                scala.Array(${Varargs(elements.map { fn => '{$fn(data)} })}*)

              scala.runtime.Tuples.fromArray(array)
          }

      (tuple, make)

    // Where a field's values come from: the enclosing value, by the field's name; or a value
    // already in hand, for the alternative of a union
    private enum Source:
      case Named(name: Text)
      case Held

    private def fetch(source: Source, data: Expr[origin], required: Boolean): Expr[origin] =
      source match
        case Source.Named(name) =>
          val nameExpr = Expr(name)

          if required then '{$target.required($nameExpr, $target.access($nameExpr, $data))}
          else '{$target.access($nameExpr, $data)}

        case Source.Held => data

    private def fetchMany(source: Source, data: Expr[origin]): Expr[List[origin]] = source match
      case Source.Named(name) => '{$target.repeated(${Expr(name)}, $data)}
      case Source.Held        => '{$target.elements($data)}

    private def fetchKeyed(source: Source, data: Expr[origin]): Expr[List[(Text, origin)]] =
      source match
        case Source.Named(name) => '{$target.entries(${Expr(name)}, $data)}
        case Source.Held        => '{$target.pairs($data)}

    // Expands one field to its type and accessor. `nested` expands a nested record's fields in
    // this mode, to the type they produce and the function making that value from the nested
    // origin value. The member's transform of a single value comes from `single`, and is read
    // under the member's multiplicity by `multiply`.
    private def expand
      ( field:  (Text, Member),
        eager:  Boolean,
        nested: List[(Text, Member)] => (TypeRepr, Expr[origin -> Any]) )
    :   Field =

      val (name, member) = field
      val (result, transform) = single(name, member, nested)
      val (tpe, accessor) = multiply(Source.Named(name), member.multiplicity, result, transform)
      val expanded = Field(name, tpe, accessor)

      if eager then discharge(expanded) else expanded

    // The type and transform of one value of a member, before its multiplicity: an
    // `Intensional`'s (or `Intensional.Fallible`'s) for a value member, the nested record's for
    // a record, and for a union a dispatch on the value's kind to the alternative it selects
    private def single
      ( name:   Text,
        member: Member,
        nested: List[(Text, Member)] => (TypeRepr, Expr[origin -> Any]) )
    :   (TypeRepr, Expr[origin -> Any]) =

      member match
        case Member.Value(label, params, _) =>
          ConstantType(StringConstant(label.s)).asType.absolve match
            case '[type label <: Label; label] =>
              val paramsExpr: Expr[List[Text]] = '{List.from(${Expr(params.stdlib)})}

              Expr.summon[label is Instance].absolve match
                case
                  Some('{$accessor: label `is` Instance `to` result}) =>
                    val transform: Expr[origin -> result] =
                      '{value => $accessor.transform(value, $paramsExpr)}

                    (TypeRepr.of[result], transform)

                case _ =>
                  Expr.summon[label is FallibleInstance].absolve match
                    case None => missing(name, label)

                    // A fallible instance reads under a `Tactic` supplied when the field is read
                    case
                      Some
                        ( ' {
                              type error <: Hazard

                              $fallible: ((`label` `is` FallibleInstance `to` result)
                                  { type Error = error })
                            } ) =>

                      val transform: Expr[origin -> Any] =
                        ' {
                            value =>
                              (tactic: Tactic[error]) ?=>
                                given Tactic[error] = tactic
                                $fallible.transform(value, $paramsExpr)
                          }

                      (raising(TypeRepr.of[result], TypeRepr.of[error]), transform)

        case Member.Record(fields, _) => nested(fields)

        case Member.Union(alternatives, _) =>
          // Each alternative, read under its own multiplicity from the value in hand
          val read: scala.List[(Text, TypeRepr, Expr[origin -> Any])] =
            alternatives.stdlib.map: (kind, alternative) =>
              val (result, transform) = single(name, alternative, nested)
              val (tpe, read) = multiply(Source.Held, alternative.multiplicity, result, transform)
              (kind, tpe, read)

          val successes = read.map: (_, tpe, _) => fallible(tpe).fold(tpe)(_(0))
          val errors = read.flatMap: (_, tpe, _) => fallible(tpe).map(_(1)).map(strip)
          val union = successes.reduce(OrType(_, _))

          if errors.isEmpty then
            val plain = read.map: (kind, _, read) => (kind, read)
            val transform: Expr[origin -> Any] = '{(value: origin) => ${dispatch('value, plain)}}

            (union, transform)
          else
            // The union of the alternatives' errors: a `Tactic` for it serves each alternative
            val error = errors.reduce(OrType(_, _))

            error.asType.absolve match
              case '[type error <: Hazard; error] =>
                // Each fallible alternative is applied to the union's tactic, which serves its
                // own error type by contravariance; the cast states that relation, which the
                // types at this level cannot
                def cases(tactic: Expr[Tactic[error]]): scala.List[(Text, Expr[origin -> Any])] =
                  read.map: (kind, tpe, read) =>
                    fallible(tpe) match
                      case Some((success, alternativeError)) =>
                        (success.asType, strip(alternativeError).asType).absolve match
                          case ('[success], '[type alternativeError <: Hazard; alternativeError]) =>
                            val applied: Expr[origin -> Any] =
                              ' {
                                  value =>
                                    val own = $tactic.asInstanceOf[Tactic[alternativeError]]
                                    type Run = Tactic[alternativeError] ?=> success
                                    ($read(value).asInstanceOf[Run])(using own)
                                }

                            (kind, applied)

                      case None => (kind, read)

                val transform: Expr[origin -> Any] =
                  ' {
                      (value: origin) =>
                        (tactic: Tactic[error]) ?=> ${dispatch('value, cases('tactic))}
                    }

                (raising(union, error), transform)

    // A match on the kind of `value`, reading it with the alternative of that kind
    private def dispatch(value: Expr[origin], cases: scala.List[(Text, Expr[origin -> Any])])
    :   Expr[Any] =

      val caseDefs = cases.map: (kind, read) =>
        CaseDef(Literal(StringConstant(kind.s)), None, '{$read($value)}.asTerm)

      val unknown: Expr[Nothing] =
        ' {
            val kind: Text = $target.kind($value)
            panic(m"the value's kind, $kind, is none the union offers")
          }

      val fallback = CaseDef(Wildcard(), None, unknown.asTerm)

      Match('{$target.kind($value)}.asTerm, caseDefs :+ fallback).asExprOf[Any]

    // Reads a value under a multiplicity, fetched as `source` says: one value, transformed; an
    // optional value, `Unset` where the format finds it absent; every value of a repeated
    // field, as a `List`; or every named value of a keyed field, as a `Map`. A fallible
    // transform, whose result is `success raises error`, keeps its `raises` clause outside the
    // `Optional`, `List` or `Map`, so the field is read under one `Tactic`.
    private def multiply
      ( source:       Source,
        multiplicity: Multiplicity,
        result:       TypeRepr,
        transform:    Expr[origin -> Any] )
    :   (TypeRepr, Expr[origin -> Any]) =

      multiplicity match
        case Multiplicity.One =>
          (result, '{data => $transform(${fetch(source, 'data, required = true)})})

        case Multiplicity.Optional => fallible(result) match
          case Some((success, error)) =>
            (success.asType, strip(error).asType).absolve match
              case ('[success], '[type error <: Hazard; error]) =>
                val accessor: Expr[origin -> Any] =
                  ' {
                      data =>
                        (tactic: Tactic[error]) ?=>
                          given Tactic[error] = tactic
                          val value = ${fetch(source, 'data, required = false)}

                          if $target.absent(value) then Unset
                          else $transform(value).asInstanceOf[Tactic[error] ?=> success]
                    }

                (refallible(result, TypeRepr.of[Optional[success]]), accessor)

          case None => result.asType.absolve match
            case '[result] =>
              val accessor: Expr[origin -> Any] =
                ' {
                    data =>
                      val value = ${fetch(source, 'data, required = false)}

                      if $target.absent(value) then Unset
                      else $transform(value).asInstanceOf[result]
                  }

              (TypeRepr.of[Optional[result]], accessor)

        case Multiplicity.Many => fallible(result) match
          case Some((success, error)) =>
            (success.asType, strip(error).asType).absolve match
              case ('[success], '[type error <: Hazard; error]) =>
                val accessor: Expr[origin -> Any] =
                  ' {
                      data =>
                        (tactic: Tactic[error]) ?=>
                          given Tactic[error] = tactic

                          ${fetchMany(source, 'data)}.map: value =>
                            $transform(value).asInstanceOf[Tactic[error] ?=> success]
                    }

                (refallible(result, TypeRepr.of[List[success]]), accessor)

          case None => result.asType.absolve match
            case '[result] =>
              val accessor: Expr[origin -> Any] =
                ' {
                    data =>
                      ${fetchMany(source, 'data)}.map: value =>
                        $transform(value).asInstanceOf[result]
                  }

              (TypeRepr.of[List[result]], accessor)

        case Multiplicity.Keyed => fallible(result) match
          case Some((success, error)) =>
            (success.asType, strip(error).asType).absolve match
              case ('[success], '[type error <: Hazard; error]) =>
                val accessor: Expr[origin -> Any] =
                  ' {
                      data =>
                        (tactic: Tactic[error]) ?=>
                          given Tactic[error] = tactic

                          val pairs = ${fetchKeyed(source, 'data)}.stdlib.map: (key, value) =>
                            val run = $transform(value).asInstanceOf[Tactic[error] ?=> success]
                            (key, run: success)

                          Map.from(pairs)
                    }

                (refallible(result, TypeRepr.of[Map[Text, success]]), accessor)

          case None => result.asType.absolve match
            case '[result] =>
              val accessor: Expr[origin -> Any] =
                ' {
                    data =>
                      val pairs = ${fetchKeyed(source, 'data)}.stdlib.map: (key, value) =>
                        (key, $transform(value).asInstanceOf[result])

                      Map.from(pairs)
                  }

              (TypeRepr.of[Map[Text, result]], accessor)

    // `result raises error`, as an application of the alias itself, found by its symbol: its
    // expansion names a context function over a capability, which a `Type` cannot carry across
    // a quote.
    private val raisesAlias: TypeRepr =
      Symbol.requiredModule("contingency.contingency_core$package").typeMember("raises").typeRef

    private def raising(result: TypeRepr, error: TypeRepr): TypeRepr =
      raisesAlias.appliedTo(scala.List(result, error))

    private def strip(repr: TypeRepr): TypeRepr = repr match
      case AnnotatedType(inner, _) => strip(inner)
      case other                   => other

    // The fallible result `result` with its success type replaced: `success raises error` becomes
    // `success2 raises error`, keeping the `Tactic`'s own capture annotation.
    private def refallible(result: TypeRepr, success2: TypeRepr): TypeRepr =
      result.dealias.absolve match
        case AppliedType(tycon, scala.List(tactic, _)) =>
          tycon.appliedTo(scala.List(tactic, success2))

    // A result of the form `success raises error`, recognised by the alias's expansion: a context
    // function over a `Tactic`.
    private def fallible(result: TypeRepr): Option[(TypeRepr, TypeRepr)] = result.dealias match
      case AppliedType(_, scala.List(tactic, success)) if result.dealias.isContextFunctionType =>
        strip(tactic) match
          case AppliedType(tycon, scala.List(error)) if tycon.typeSymbol == tacticSymbol =>
            Some((success, error))

          case _ => None

      case _ => None

    // In a tuple, a field whose result is `success raises error` becomes an element of type
    // `success`: the `Tactic` is found at the call site and applied when the tuple is built.
    private def discharge(field: Field): Field = fallible(field.tpe) match
      case Some((success, error)) =>
        success.asType.absolve match
          case '[success] => strip(error).asType.absolve match
            case '[type error <: Hazard; error] =>
              Expr.summon[Tactic[error]].absolve match
                case None =>
                  val hazard = TypeRepr.of[error].show.tt
                  val name = field.name
                  halt(m"the field $name may raise $hazard, but no Tactic for it is in scope")

                case Some(tactic) =>
                  val read = field.accessor

                  val accessor: Expr[origin -> Any] =
                    ' {
                        data =>
                          given Tactic[error] = $tactic
                          $read(data).asInstanceOf[Tactic[error] ?=> success]
                      }

                  Field(field.name, TypeRepr.of[success], accessor)

      case None => field

// A type provider over one document format: a schema, as `fields`, and the format's reading
// primitives. The `build` and `tuple` macros manufacture a record or named tuple type from the
// fields, applying each member's multiplicity themselves, so a concrete specification supplies
// only single-value `Intensional` instances and the three primitives, `access`, `absent` and
// `repeated`.
trait Specification extends Original:
  type Origin
  protected type Origin0 = Origin
  type Form <: { type Origin = Origin0 }

  // The fields in order: a record's members are unordered, but a named tuple's elements follow
  // this order.
  def fields: List[(Text, Member)]

  // The field's own value within a document, which may be a sentinel for a missing field
  def access(name: Text, value: Origin): Origin

  // Whether a value `access` returned stands for a missing field
  def absent(value: Origin): Boolean

  // Every value of a field carrying any number of them, in document order: the elements of a
  // JSON array, or every sibling with the field's keyword in a format which repeats the field.
  def repeated(name: Text, value: Origin): List[Origin]

  // The value of a field which must be present, read under `Multiplicity.One`: a specification
  // may fail here, in its own way, when the value is absent. By default the value is returned
  // as it is, and the field's `Intensional` meets the absent value.
  def required(name: Text, value: Origin): Origin = value

  // The remaining hooks serve features a format may lack; each fails unless the specification
  // provides it. `entries` gives a keyed field's values by name, for `Multiplicity.Keyed`.
  def entries(name: Text, value: Origin): List[(Text, Origin)] =
    panic(m"this specification has no keyed fields")

  // The kind of a value, by which a `Member.Union` chooses its alternative
  def kind(value: Origin): Text = panic(m"this specification has no unions")

  // The elements of a value which is itself a sequence, for a `Many` alternative of a union
  def elements(value: Origin): List[Origin] = panic(m"this specification has no unions")

  // The named values within a value which is itself a dictionary, for a `Keyed` alternative
  def pairs(value: Origin): List[(Text, Origin)] = panic(m"this specification has no unions")

  // A pure function (`->`): the built Record retains the transform, and a capturing one would
  // make the record itself a capability, which Record's pure self type (rightly) forbids.
  def build(data: Origin, transform: Text -> Origin -> Any): Record = Record(data, transform)

  // The typed record over `value`, refined with one member per field, read lazily: each access
  // runs the field's accessor on the record's data. Inlined at the call site, where the receiver
  // is the specification object, so the macro can find that object by its type and evaluate its
  // `fields` — which is why the object must be compiled before the calling code.
  transparent inline def record(inline value: Origin): Record =
    ${Specification.record[this.type, Origin, Form]('value)}

  // A named tuple with one element per field, in the specification's order, read eagerly: every
  // field's accessor runs when the tuple is built, so a fallible field fails then, and its
  // element has the successful type, with its `raises` clause discharged by a `Tactic` at the
  // call site.
  transparent inline def tuple(inline value: Origin): NamedTuple.AnyNamedTuple =
    ${Specification.tuple[this.type, Origin, Form]('value)}
