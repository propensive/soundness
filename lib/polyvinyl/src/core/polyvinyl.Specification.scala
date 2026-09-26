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

  // Builds a `Record` refined with one member per field, read lazily: each access runs the
  // field's accessor on the record's data.
  def build(value: Expr[Origin])(using Type[Origin], Type[Form])(using thisType: Type[this.type])
  :   Quotes ?->{this} Expr[Record] =

    val (refined, transform) = Expansion(target).record(fields)

    refined.absolve match
      case '[type refined <: Record; refined] =>
        '{$target.build($value, $transform).asInstanceOf[refined]}

  // Builds a named tuple with one element per field, in the specification's order, read eagerly:
  // every field's accessor runs when the tuple is built, so a fallible field fails then, and its
  // element has the successful type, with its `raises` clause discharged by a `Tactic` at the
  // call site.
  def tuple(value: Expr[Origin])(using Type[Origin], Type[Form])(using thisType: Type[this.type])
  :   Quotes ?->{this} Expr[NamedTuple.AnyNamedTuple] =

    val (tuple, make) = Expansion(target).tuple(fields)

    tuple.absolve match
      case '[type tuple <: NamedTuple.AnyNamedTuple; tuple] => '{$make($value).asInstanceOf[tuple]}

  private def target(using Type[Origin], Type[Form])(using thisType: Type[this.type])
  :   Quotes ?->{this} Expr[Specification in Form from Origin] =

    import quotes.reflect.*

    thisType.absolve match
      case '[thisType] =>
        Ref(TypeRepr.of[thisType].typeSymbol.companionModule)
        . asExprOf[Specification in Form from Origin]

  // The macro expansion shared by both modes. Each field becomes an accessor from the origin
  // value to the field's value, and a type; the modes differ only in how the accessors and types
  // are assembled, and in what a nested `Member.Record` becomes. Its `Quotes` is its own path, so
  // the types it produces leave as `Type[?]`s; internally, the lists are the compiler's own.
  private class Expansion(target: Expr[Specification in Form from Origin])
    ( using Quotes, Type[Origin], Type[Form] ):

    import quotes.reflect.*

    private case class Field(name: Text, tpe: TypeRepr, accessor: Expr[Origin -> Any])

    private def missing(name: Text, label: Text): Nothing =
      halt(m"could not find an Intensional instance for the field $name with type $label")

    def record(fields: List[(Text, Member)]): (Type[?], Expr[Text -> Origin -> Any]) =
      val (refined, transform) = expandRecord(fields)
      (refined.asType, transform)

    def tuple(fields: List[(Text, Member)]): (Type[?], Expr[Origin -> Any]) =
      val (tuple, make) = expandTuple(fields)
      (tuple.asType, make)

    private val consCtor = TypeRepr.of[Any *: EmptyTuple].absolve match
      case AppliedType(tycon, _) => tycon

    private val tacticSymbol = TypeRepr.of[Tactic[Hazard]].typeSymbol

    private def tupleType(elements: scala.List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple]): (head, tail) =>
        consCtor.appliedTo(scala.List(head, tail))

    private def expandRecord(fields: List[(Text, Member)])
    :   (TypeRepr, Expr[Text -> Origin -> Any]) =

      val members = fields.stdlib.map(expand(_, eager = false, nested))

      // The record's `Origin` is fixed too, so its `data` has the origin type at the call site
      val refined = members.foldLeft(TypeRepr.of[Record from Origin]): (refined, field) =>
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

      val transform: Expr[Text -> Origin -> Any] =
        ' {
            (name: Text) =>
              ${Match('name.asTerm, caseDefs :+ fallback('name)).asExprOf[Origin => Any]}
          }

      (refined, transform)

    // A nested record: built like a top-level one, from the nested origin value
    private def nested(fields: List[(Text, Member)]): (TypeRepr, Expr[Origin -> Any]) =
      val (refined, transform) = expandRecord(fields)
      (refined, '{field => $target.build(field, $transform)})

    private def expandTuple(fields: List[(Text, Member)]): (TypeRepr, Expr[Origin -> Any]) =
      val members = fields.stdlib.map(expand(_, eager = true, expandTuple))
      val names = tupleType(members.map { field => ConstantType(StringConstant(field.name.s)) })
      val values = tupleType(members.map(_.tpe))
      val tuple = TypeRepr.of[NamedTuple.NamedTuple].appliedTo(scala.List(names, values))

      val elements: scala.List[Expr[Origin -> Object]] = members.map: field =>
        '{data => ${field.accessor}(data).asInstanceOf[Object]}

      val make: Expr[Origin -> Any] =
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

    private def fetch(source: Source, data: Expr[Origin], required: Boolean): Expr[Origin] =
      source match
        case Source.Named(name) =>
          val nameExpr = Expr(name)

          if required then '{$target.required($nameExpr, $target.access($nameExpr, $data))}
          else '{$target.access($nameExpr, $data)}

        case Source.Held => data

    private def fetchMany(source: Source, data: Expr[Origin]): Expr[List[Origin]] = source match
      case Source.Named(name) => '{$target.repeated(${Expr(name)}, $data)}
      case Source.Held        => '{$target.elements($data)}

    private def fetchKeyed(source: Source, data: Expr[Origin]): Expr[List[(Text, Origin)]] =
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
        nested: List[(Text, Member)] => (TypeRepr, Expr[Origin -> Any]) )
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
        nested: List[(Text, Member)] => (TypeRepr, Expr[Origin -> Any]) )
    :   (TypeRepr, Expr[Origin -> Any]) =

      member match
        case Member.Value(label, params, _) =>
          ConstantType(StringConstant(label.s)).asType.absolve match
            case '[type label <: Label; label] =>
              val paramsExpr: Expr[List[Text]] = '{List.from(${Expr(params.stdlib)})}

              Expr.summon[label is Intensional in Form from Origin].absolve match
                case
                  Some('{$accessor: label `is` Intensional `in` Form `from` Origin `to` result}) =>
                    val transform: Expr[Origin -> result] =
                      '{value => $accessor.transform(value, $paramsExpr)}

                    (TypeRepr.of[result], transform)

                case _ =>
                  Expr.summon[label is Intensional.Fallible in Form from Origin].absolve match
                    case None => missing(name, label)

                    // A fallible instance reads under a `Tactic` supplied when the field is read
                    case
                      Some
                        ( ' {
                              type error <: Hazard

                              $fallible: ((`label` `is` Intensional.Fallible `in` Form `from` Origin
                                  `to` result) { type Error = error })
                            } ) =>

                      val transform: Expr[Origin -> Any] =
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
          val read: scala.List[(Text, TypeRepr, Expr[Origin -> Any])] =
            alternatives.stdlib.map: (kind, alternative) =>
              val (result, transform) = single(name, alternative, nested)
              val (tpe, read) = multiply(Source.Held, alternative.multiplicity, result, transform)
              (kind, tpe, read)

          val successes = read.map: (_, tpe, _) => fallible(tpe).fold(tpe)(_(0))
          val errors = read.flatMap: (_, tpe, _) => fallible(tpe).map(_(1)).map(strip)
          val union = successes.reduce(OrType(_, _))

          if errors.isEmpty then
            val plain = read.map: (kind, _, read) => (kind, read)
            val transform: Expr[Origin -> Any] = '{(value: Origin) => ${dispatch('value, plain)}}

            (union, transform)
          else
            // The union of the alternatives' errors: a `Tactic` for it serves each alternative
            val error = errors.reduce(OrType(_, _))

            error.asType.absolve match
              case '[type error <: Hazard; error] =>
                // Each fallible alternative is applied to the union's tactic, which serves its
                // own error type by contravariance; the cast states that relation, which the
                // types at this level cannot
                def cases(tactic: Expr[Tactic[error]]): scala.List[(Text, Expr[Origin -> Any])] =
                  read.map: (kind, tpe, read) =>
                    fallible(tpe) match
                      case Some((success, alternativeError)) =>
                        (success.asType, strip(alternativeError).asType).absolve match
                          case ('[success], '[type alternativeError <: Hazard; alternativeError]) =>
                            val applied: Expr[Origin -> Any] =
                              ' {
                                  value =>
                                    val own = $tactic.asInstanceOf[Tactic[alternativeError]]
                                    type Run = Tactic[alternativeError] ?=> success
                                    ($read(value).asInstanceOf[Run])(using own)
                                }

                            (kind, applied)

                      case None => (kind, read)

                val transform: Expr[Origin -> Any] =
                  ' {
                      (value: Origin) =>
                        (tactic: Tactic[error]) ?=> ${dispatch('value, cases('tactic))}
                    }

                (raising(union, error), transform)

    // A match on the kind of `value`, reading it with the alternative of that kind
    private def dispatch(value: Expr[Origin], cases: scala.List[(Text, Expr[Origin -> Any])])
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
        transform:    Expr[Origin -> Any] )
    :   (TypeRepr, Expr[Origin -> Any]) =

      multiplicity match
        case Multiplicity.One =>
          (result, '{data => $transform(${fetch(source, 'data, required = true)})})

        case Multiplicity.Optional => fallible(result) match
          case Some((success, error)) =>
            (success.asType, strip(error).asType).absolve match
              case ('[success], '[type error <: Hazard; error]) =>
                val accessor: Expr[Origin -> Any] =
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
              val accessor: Expr[Origin -> Any] =
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
                val accessor: Expr[Origin -> Any] =
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
              val accessor: Expr[Origin -> Any] =
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
                val accessor: Expr[Origin -> Any] =
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
              val accessor: Expr[Origin -> Any] =
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

                  val accessor: Expr[Origin -> Any] =
                    ' {
                        data =>
                          given Tactic[error] = $tactic
                          $read(data).asInstanceOf[Tactic[error] ?=> success]
                      }

                  Field(field.name, TypeRepr.of[success], accessor)

      case None => field
