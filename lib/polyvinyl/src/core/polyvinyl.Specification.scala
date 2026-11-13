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
  // The failure to read a specification — a schema which cannot be parsed or composed — which
  // the `record` and `tuple` macros report as a compile error at the use site.
  case class Error(detail: Message)(using Diagnostics)
  extends fulminate.Error(m"the specification could not be read because $detail")

  // A specification whose fields read from `origin` in `form`, raising `failure`
  type Raising[form, origin, failure <: Hazard] =
    (Specification in form from origin) { type Failure = failure }

  def record[self: Type, origin: Type, form: Type, failure <: Hazard: Type](value: Expr[origin])
    ( using Quotes )
  :   Expr[Record] =

    val (target, specification) = locate[self, origin, form, failure]
    given Tactic[Specification.Error] = HaltTactic[Specification.Error, Any]()

    val (refined, transform) =
      Expansion[origin, Unit, form, failure](target, false).record(specification.fields)

    refined.absolve match
      case '[type refined <: Record; refined] =>
        '{$target.build($value, $transform).asInstanceOf[refined]}

  // As `record`, for a specification whose members are read in a context: each member of the
  // record is read only where a `context` is given, as well as a `Tactic`. The context's type
  // is the caller's to choose, within the specification's `Context`, so that it may say more
  // than the specification can: which of several sources a record was read from, say.
  def recordWith[self: Type, origin: Type, context: Type, form: Type, failure <: Hazard: Type]
    ( value: Expr[origin] )
    ( using Quotes )
  :   Expr[Record] =

    val (target, specification) = locate[self, origin, form, failure]
    given Tactic[Specification.Error] = HaltTactic[Specification.Error, Any]()

    val (refined, transform) =
      Expansion[origin, context, form, failure](target, true).record(specification.fields)

    refined.absolve match
      case '[type refined <: Record; refined] =>
        '{$target.build($value, $transform).asInstanceOf[refined]}

  def tuple[self: Type, origin: Type, form: Type, failure <: Hazard: Type](value: Expr[origin])
    ( using Quotes )
  :   Expr[NamedTuple.AnyNamedTuple] =

    val (target, specification) = locate[self, origin, form, failure]
    given Tactic[Specification.Error] = HaltTactic[Specification.Error, Any]()
    val (tuple, make) =
      Expansion[origin, Unit, form, failure](target, false).tuple(specification.fields)

    tuple.absolve match
      case '[type tuple <: NamedTuple.AnyNamedTuple; tuple] => '{$make($value).asInstanceOf[tuple]}

  // The expansions over an explicit field list, for a macro which has the fields of a part of a
  // document in hand — a response schema within an OpenAPI document, say — and a specification
  // object supplying only the format's primitives. `recordExpansion` yields the refined `Record`
  // type and the transform `build` takes; `tupleExpansion` the named-tuple type and its maker.
  def recordExpansion[origin: Type, form: Type, failure <: Hazard: Type]
    ( target: Expr[Raising[form, origin, failure]], fields: List[(Text, Member)] )
    ( using Quotes )
  :   (Type[?], Expr[Text -> origin -> Any]) =

    Expansion[origin, Unit, form, failure](target, false).record(fields)

  def tupleExpansion[origin: Type, form: Type, failure <: Hazard: Type]
    ( target: Expr[Raising[form, origin, failure]], fields: List[(Text, Member)] )
    ( using Quotes )
  :   (Type[?], Expr[origin -> Any]) =

    Expansion[origin, Unit, form, failure](target, false).tuple(fields)

  // The specification object whose type is `self`: as a reference for the generated code, and as
  // an instance, loaded through the macro's own classloader, for its `fields`
  private def locate[self: Type, origin: Type, form: Type, failure <: Hazard: Type](using Quotes)
  :   (Expr[Raising[form, origin, failure]], Specification) =

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

    val target = Ref(module).asExprOf[Raising[form, origin, failure]]

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

  // The macro expansion shared by both modes. Each field becomes the code reading its value from
  // the origin value, and a type; the modes differ only in how the readings and types are
  // assembled, and in what a nested `Member.Record` becomes. Its `Quotes` is its own path, so
  // the types it produces leave as `Type[?]`s; internally, the lists are the compiler's own.
  //
  // Every reading may fail, through the specification's `Tactic[failure]`. A reading is a
  // function from a value and the tactic, which is applied by beta-reduction wherever it is
  // used, rather than called: a record is expanded at every call site, so its code is kept to
  // one function per record (and one per repeated member) however many fields it has.
  //
  // `contextual` is whether members are read in a `context`, as `recordWith` asks: if not, the
  // context is `Unit`, nothing asks for one, and the code generated is what it always was.
  private class Expansion[origin: Type, context: Type, form: Type, failure <: Hazard: Type]
    ( target: Expr[Raising[form, origin, failure]], contextual: Boolean )
    ( using Quotes ):

    import quotes.reflect.*

    // Within the code generated, a context is passed as a value like any other. It is tracked
    // where that code meets what is written by hand: in the type of a member, `Record.Bound`,
    // and in the specification's hooks.
    private type Reading = Expr[(origin, context, Tactic[failure]) -> Any]
    private case class Field(name: Text, tpe: TypeRepr, reading: Reading)

    // The code reading `value` with `reading`, in `context` and under `tactic`
    private def read
      ( reading: Reading,
        value:   Expr[origin],
        context: Expr[context],
        tactic:  Expr[Tactic[failure]] )
    :   Expr[Any] =

      Expr.betaReduce('{$reading($value, $context, $tactic)})

    // The context of a specification which has none
    private def vacant: Expr[context] = '{()}.asExprOf[context]

    // A call of one of the specification's contextual hooks, made as a tree: the context has
    // the caller's type, which only the expansion can check against the specification's.
    private def hook(name: String, types: scala.List[TypeRepr], arguments: scala.List[Term])
      ( context: Expr[context], tactic: Expr[Tactic[failure]] )
    :   Term =

      val method = Select.unique(target.asTerm, name)
      val applied = if types.isEmpty then method else method.appliedToTypes(types)
      applied.appliedToArgs(arguments).appliedToArgs(scala.List(context.asTerm, tactic.asTerm))

    private type Instance = Intensional in form from origin
    private type FallibleInstance = Intensional.Fallible in form from origin

    private def missing(name: Text, label: Text): Nothing =
      halt(m"could not find an Intensional instance for the field $name with type $label")

    def record(fields: List[(Text, Member)]): (Type[?], Expr[Text -> origin -> Any]) =
      val (refined, transform) = expandRecord(fields)
      (refined.asType, transform)

    // A tuple's fields are read as it is built, under the `Tactic` found where it is built
    def tuple(fields: List[(Text, Member)]): (Type[?], Expr[origin -> Any]) =
      val (tuple, reading) = expandTuple(fields)

      val context: Expr[context] =
        if !contextual then vacant else Expr.summon[context].getOrElse:
          val needed = TypeRepr.of[context].show.tt
          halt(m"the tuple's fields are read in a context, but no $needed is in scope")

      Expr.summon[Tactic[failure]] match
        case Some(tactic) => (tuple.asType, '{data => ${read(reading, 'data, context, tactic)}})

        case None =>
          val hazard = TypeRepr.of[failure].show.tt
          halt(m"the tuple's fields may raise $hazard, but no Tactic for it is in scope")

    private val consCtor = TypeRepr.of[Any *: EmptyTuple].absolve match
      case AppliedType(tycon, _) => tycon

    private def tupleType(elements: scala.List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple]): (head, tail) =>
        consCtor.appliedTo(scala.List(head, tail))

    // The record's transform: from a field's name and the record's data to the field's reading,
    // a context function over the `Tactic` of the code reading the field, which the refined
    // field type, `Record.Field[result, failure]`, applies there
    private def expandRecord(fields: List[(Text, Member)])
    :   (TypeRepr, Expr[Text -> origin -> Any]) =

      val members = fields.stdlib.map(expand(_, nested))

      // The record's `origin` is fixed too, so its `data` has the origin type at the call site
      val refined = members.foldLeft(TypeRepr.of[Record from origin]): (refined, field) =>
        Refinement(refined, field.name.s, raising(field.tpe, TypeRepr.of[failure]))

      def select
        ( name:    Expr[Text],
          data:    Expr[origin],
          context: Expr[context],
          tactic:  Expr[Tactic[failure]] )
      :   Expr[Any] =

        val caseDefs = members.map: field =>
          val reading = read(field.reading, data, context, tactic)
          CaseDef(Literal(StringConstant(field.name.s)), None, reading.asTerm)

        // Unreachable through a refined record, whose fields are exactly the cases; reachable
        // through `selectDynamic` with an undeclared name
        val missing: Expr[Nothing] =
          ' {
              val field: Text = $name
              panic(m"the record has no field named $field")
            }

        Match(name.asTerm, caseDefs :+ CaseDef(Wildcard(), None, missing.asTerm)).asExprOf[Any]

      val transform: Expr[Text -> origin -> Any] =
        if contextual then
          ' {
              (name: Text) => (data: origin) => (context: context, tactic: Tactic[failure]) ?=>
                ${select('name, 'data, 'context, 'tactic)}
            }
        else
          ' {
              (name: Text) => (data: origin) => (tactic: Tactic[failure]) ?=>
                ${select('name, 'data, vacant, 'tactic)}
            }

      (refined, transform)

    // A nested record: built like a top-level one, from the nested origin value
    private def nested(fields: List[(Text, Member)]): (TypeRepr, Reading) =
      val (refined, transform) = expandRecord(fields)
      (refined, '{(value, _, _) => $target.build(value, $transform)})

    // A tuple, nested or not, read from a value under the tactic in hand
    private def expandTuple(fields: List[(Text, Member)]): (TypeRepr, Reading) =
      val members = fields.stdlib.map(expand(_, expandTuple))
      val names = tupleType(members.map { field => ConstantType(StringConstant(field.name.s)) })
      val values = tupleType(members.map(_.tpe))
      val tuple = TypeRepr.of[NamedTuple.NamedTuple].appliedTo(scala.List(names, values))

      def elements
        ( data: Expr[origin], context: Expr[context], tactic: Expr[Tactic[failure]] )
      :   Expr[scala.Seq[Object]] =

        Varargs:
          members.map: field =>
            '{${read(field.reading, data, context, tactic)}.asInstanceOf[Object]}

      val reading: Reading =
        ' {
            (data, context, tactic) =>
              val array: scala.Array[Object] =
                scala.Array(${elements('data, 'context, 'tactic)}*)

              scala.runtime.Tuples.fromArray(array)
          }

      (tuple, reading)

    // Where a field's values come from: the enclosing value, by the field's name; or a value
    // already in hand, for the alternative of a union
    private enum Source:
      case Named(name: Text)
      case Held

    private def fetch
      ( source:   Source,
        data:     Expr[origin],
        required: Boolean,
        context:  Expr[context],
        tactic:   Expr[Tactic[failure]] )
    :   Expr[origin] =

      source match
        case Source.Named(name) =>
          val nameExpr = Expr(name)

          val value =
            if !contextual then '{$target.access($nameExpr, $data)(using $tactic)} else
              hook("accessWith", scala.Nil, scala.List(nameExpr.asTerm, data.asTerm))
                ( context, tactic )
              . asExprOf[origin]

          if required then '{$target.required($nameExpr, $value)(using $tactic)} else value

        case Source.Held => data

    private def fetchMany
      ( source:  Source,
        data:    Expr[origin],
        context: Expr[context],
        tactic:  Expr[Tactic[failure]] )
    :   Expr[List[origin]] =

      source match
        case Source.Held => '{$target.elements($data)(using $tactic)}

        case Source.Named(name) =>
          if !contextual then '{$target.repeated(${Expr(name)}, $data)(using $tactic)} else
            hook("repeatedWith", scala.Nil, scala.List(Expr(name).asTerm, data.asTerm))
              ( context, tactic )
            . asExprOf[List[origin]]

    private def fetchKeyed
      ( source:  Source,
        data:    Expr[origin],
        context: Expr[context],
        tactic:  Expr[Tactic[failure]] )
    :   Expr[List[(Text, origin)]] =

      source match
        case Source.Held => '{$target.pairs($data)(using $tactic)}

        case Source.Named(name) =>
          if !contextual then '{$target.entries(${Expr(name)}, $data)(using $tactic)} else
            hook("entriesWith", scala.Nil, scala.List(Expr(name).asTerm, data.asTerm))
              ( context, tactic )
            . asExprOf[List[(Text, origin)]]

    // Expands one field to its value's type and its reading from the enclosing value. `nested`
    // expands a nested record's fields in this mode. The member's reading of a single value
    // comes from `single`, and is read under the member's multiplicity by `multiply`.
    private def expand
      ( field:  (Text, Member),
        nested: List[(Text, Member)] => (TypeRepr, Reading) )
    :   Field =

      val (name, member) = field
      val (result, reading) = single(name, member, nested)
      val (tpe, multiplied) = multiply(Source.Named(name), member.multiplicity, result, reading)

      Field(name, tpe, multiplied)

    // The type and reading of one value of a member, before its multiplicity: an
    // `Intensional`'s (or `Intensional.Fallible`'s) for a value member, the nested record's for
    // a record, and for a union a dispatch on the value's kind to the alternative it selects
    private def single
      ( name:   Text,
        member: Member,
        nested: List[(Text, Member)] => (TypeRepr, Reading) )
    :   (TypeRepr, Reading) =

      member match
        case Member.Value(label, params, _) =>
          ConstantType(StringConstant(label.s)).asType.absolve match
            case '[type label <: Label; label] =>
              val paramsExpr: Expr[List[Text]] = '{List.from(${Expr(params.stdlib)})}

              Expr.summon[label is Instance].absolve match
                case
                  Some('{$accessor: label `is` Instance `to` result}) =>
                    val reading: Reading =
                      '{(value, _, _) => $accessor.transform(value, $paramsExpr)}

                    (TypeRepr.of[result], reading)

                case _ =>
                  Expr.summon[label is FallibleInstance].absolve match
                    case None => missing(name, label)

                    // A fallible instance reads under the specification's `Tactic`, which
                    // serves the instance's own error, by contravariance, only where that error
                    // is one the specification raises
                    case
                      Some
                        ( ' {
                              type error <: Hazard

                              $fallible: ((`label` `is` FallibleInstance `to` result)
                                  { type Error = error })
                            } ) =>

                      if !(TypeRepr.of[error] <:< TypeRepr.of[failure]) then
                        val raised = TypeRepr.of[error].show.tt
                        val failure = TypeRepr.of[failure].show.tt
                        val specification = m"the specification's $failure"
                        halt(m"the instance for $label raises $raised, not $specification")

                      // The cast states the relation just checked, which the quote's types
                      // cannot
                      val reading: Reading =
                        ' {
                            (value, _, tactic) =>
                              val own = tactic.asInstanceOf[Tactic[error]]
                              $fallible.transform(value, $paramsExpr)(using own)
                          }

                      (TypeRepr.of[result], reading)

        case Member.Record(fields, _) => nested(fields)

        case Member.Union(alternatives, _) =>
          // Each alternative, read under its own multiplicity from the value in hand
          val readings: scala.List[(Text, TypeRepr, Reading)] =
            alternatives.stdlib.map: (kind, alternative) =>
              val (result, reading) = single(name, alternative, nested)
              val multiplicity = alternative.multiplicity
              val (tpe, multiplied) = multiply(Source.Held, multiplicity, result, reading)
              (kind, tpe, multiplied)

          val union = readings.map(_(1)).reduce(OrType(_, _))
          val kinds = readings.map(_(0).s)

          def dispatch
            ( value: Expr[origin], context: Expr[context], tactic: Expr[Tactic[failure]] )
          :   Expr[Any] =

            val caseDefs = readings.map: (kind, _, reading) =>
              val chosen = read(reading, value, context, tactic)
              CaseDef(Literal(StringConstant(kind.s)), None, chosen.asTerm)

            val unknown: Expr[Nothing] =
              ' {
                  val offered = List.from(${Expr(kinds)}.map(_.tt))
                  $target.unoffered($target.kind($value), offered)(using $tactic)
                }

            val fallback = CaseDef(Wildcard(), None, unknown.asTerm)
            Match('{$target.kind($value)}.asTerm, caseDefs :+ fallback).asExprOf[Any]

          (union, '{(value, context, tactic) => ${dispatch('value, 'context, 'tactic)}})

    // Reads a value under a multiplicity, fetched as `source` says: one value; an optional value,
    // `Unset` where the format finds it absent; every value of a repeated field, as a `List`; or
    // every named value of a keyed field, as a `Map`
    private def multiply
      ( source:       Source,
        multiplicity: Multiplicity,
        result:       TypeRepr,
        reading:      Reading )
    :   (TypeRepr, Reading) =

      result.asType.absolve match
        case '[result] => multiplicity match
          case Multiplicity.One =>
            val multiplied: Reading =
              ' {
                  (data, context, tactic) =>
                    $ {
                        val value = fetch(source, 'data, true, 'context, 'tactic)
                        read(reading, value, 'context, 'tactic)
                      }
                }

            (result, multiplied)

          case Multiplicity.Optional =>
            val multiplied: Reading =
              ' {
                  (data, context, tactic) =>
                    val value = ${fetch(source, 'data, false, 'context, 'tactic)}

                    if $target.absent(value) then Unset
                    else ${read(reading, 'value, 'context, 'tactic)}
                }

            (TypeRepr.of[Optional[result]], multiplied)

          case Multiplicity.Many =>
            val multiplied: Reading =
              ' {
                  (data, context, tactic) =>
                    ${fetchMany(source, 'data, 'context, 'tactic)}.map: value =>
                      ${read(reading, 'value, 'context, 'tactic)}.asInstanceOf[result]
                }

            (TypeRepr.of[List[result]], multiplied)

          case Multiplicity.Keyed =>
            val multiplied: Reading =
              ' {
                  (data, context, tactic) =>
                    val pairs =
                      ${fetchKeyed(source, 'data, 'context, 'tactic)}.stdlib.map: (key, value) =>
                        (key, ${read(reading, 'value, 'context, 'tactic)}.asInstanceOf[result])

                    Map.from(pairs)
                }

            (TypeRepr.of[Map[Text, result]], multiplied)

          // The specification's own collection, which is given how to read each of its
          // elements and reads none until it is asked: the reading is passed as it is, to be
          // applied to a context and a tactic of the collection's own.
          case Multiplicity.Series =>
            val name = source match
              case Source.Named(name) => name
              case Source.Held        => halt(m"a series cannot be an alternative of a union")

            if !contextual then halt(m"the field $name is a series, which is read in a context")

            // The cast is of the context alone, from a value to one which is tracked: the
            // collection is given its context as a capability, and passes it back to this.
            val each: Expr[Any] =
              ' {
                  ((value: origin, context: context, tactic: Tactic[failure]) =>
                    $reading(value, context, tactic).asInstanceOf[result])
                  . asInstanceOf[(origin, context^, Tactic[failure]) -> result]
                }

            val series =
              TypeSelect(target.asTerm, "Series").tpe
              . appliedTo(scala.List(TypeRepr.of[context], result))

            val multiplied: Reading =
              ' {
                  (data, context, tactic) =>
                    $ {
                        hook
                          ( "series",
                            scala.List(TypeRepr.of[context], result),
                            scala.List(Expr(name).asTerm, 'data.asTerm, each.asTerm) )
                          ( 'context, 'tactic )
                        . asExprOf[Any]
                      }
                }

            (series, multiplied)

    // `Record.Field[result, error]`, as an application of the alias itself, found by its symbol:
    // its expansion names a context function over a capability, which a `Type` cannot carry
    // across a quote.
    private val fieldAlias: TypeRepr =
      Symbol.requiredModule("polyvinyl.Record").typeMember("Field").typeRef

    private val boundAlias: TypeRepr =
      Symbol.requiredModule("polyvinyl.Record").typeMember("Bound").typeRef

    private def raising(result: TypeRepr, error: TypeRepr): TypeRepr =
      if contextual then boundAlias.appliedTo(scala.List(result, TypeRepr.of[context], error))
      else fieldAlias.appliedTo(scala.List(result, error))

  // How many values a field carries: exactly one, at most one (read as `Optional`), any number
  // (read as a `List`), or any number each under a name (read as a `Map[Text, _]`). The
  // specification's macro applies the multiplicity itself, so an `Intensional` instance describes
  // the single value only.
  enum Multiplicity:
    import anticipation.*

    case One, Optional, Many, Keyed, Series

  // A specification's description of one field. A `Value`'s `fieldType` is the label by which its
  // `Intensional` instance is found; a `Record` nests further fields, read from the field's own
  // origin value by the same specification; a `Union` offers alternatives, each under the *kind*
  // of value (as the specification's `kind` reports it) which selects it, and reads as the union
  // of their types. Fields are ordered, because a named tuple's elements follow the
  // specification's order.
  enum Member:
    import anticipation.*

    case Value
      ( fieldType:    Text,
        params:       List[Text]   = Nil,
        multiplicity: Multiplicity = Multiplicity.One )

    case Record(fields: List[(Text, Member)], multiplicity: Multiplicity = Multiplicity.One)
    case Union(alternatives: List[(Text, Member)], multiplicity: Multiplicity = Multiplicity.One)

    def multiplicity: Multiplicity

    def of(multiplicity: Multiplicity): Member = this match
      case value: Value   => value.copy(multiplicity = multiplicity)
      case record: Record => record.copy(multiplicity = multiplicity)
      case union: Union   => union.copy(multiplicity = multiplicity)

    def optional: Member = of(Multiplicity.Optional)
    def many: Member = of(Multiplicity.Many)
    def keyed: Member = of(Multiplicity.Keyed)
    def series: Member = of(Multiplicity.Series)

// A type provider over one document format: a schema, as `fields`, and the format's reading
// primitives. The `build` and `tuple` macros manufacture a record or named tuple type from the
// fields, applying each member's multiplicity themselves, so a concrete specification supplies
// only single-value `Intensional` instances and the three primitives, `access`, `absent` and
// `repeated`.
trait Specification extends Original:
  import Specification.{Member, Multiplicity}

  type Origin
  protected type Origin0 = Origin
  type Form <: { type Origin = Origin0 }

  // The error every field of a record may raise, through the `Tactic` of the code which reads
  // it: a value of the wrong kind, a required value which is absent, or one which breaks the
  // schema's constraints. Each field's type is `Record.Field[result, Failure]`, so a caller can
  // gather every problem in a document under one `Tactic`. The hooks below which can fail take
  // the `Tactic` as a `using` parameter rather than declaring `raises`: under capture checking, a
  // `raises` result cannot implement an abstract method.
  type Failure <: Hazard

  // The fields in order: a record's members are unordered, but a named tuple's elements follow
  // this order. A schema which cannot be read is raised, which the `record` and `tuple` macros
  // turn into a compile error.
  def fields(using Tactic[Specification.Error]): List[(Text, Member)]

  // The field's own value within a document, which may be a sentinel for a missing field
  def access(name: Text, value: Origin)(using Tactic[Failure]): Origin

  // Whether a value `access` returned stands for a missing field
  def absent(value: Origin): Boolean

  // Every value of a field carrying any number of them, in document order: the elements of a
  // JSON array, or every sibling with the field's keyword in a format which repeats the field.
  def repeated(name: Text, value: Origin)(using Tactic[Failure]): List[Origin]

  // The value of a field which must be present, read under `Multiplicity.One`: a specification
  // may fail here, in its own way, when the value is absent. By default the value is returned
  // as it is, and the field's `Intensional` meets the absent value.
  def required(name: Text, value: Origin)(using Tactic[Failure]): Origin = value

  // The remaining hooks serve features a format may lack; each fails unless the specification
  // provides it. `entries` gives a keyed field's values by name, for `Multiplicity.Keyed`.
  def entries(name: Text, value: Origin)(using Tactic[Failure]): List[(Text, Origin)] =
    panic(m"this specification has no keyed fields")

  // The kind of a value, by which a `Member.Union` chooses its alternative
  def kind(value: Origin): Text = panic(m"this specification has no unions")

  // The elements of a value which is itself a sequence, for a `Many` alternative of a union
  def elements(value: Origin)(using Tactic[Failure]): List[Origin] =
    panic(m"this specification has no unions")

  // The named values within a value which is itself a dictionary, for a `Keyed` alternative
  def pairs(value: Origin)(using Tactic[Failure]): List[(Text, Origin)] =
    panic(m"this specification has no unions")

  // A value of a union whose kind, `kind`, is none of those the union `offered`
  def unoffered(kind: Text, offered: List[Text])(using Tactic[Failure]): Nothing =
    panic(m"the value's kind, $kind, is none the union offers")

  // What the members of a record are read in, for a specification whose source is not in the
  // values it reads: an open file, say, which a record only says where to look in. A record
  // made by `recordWith` asks for one wherever a member is selected, and passes it to the
  // hooks below, which stand for `access`, `repeated` and `entries`. A specification without
  // one leaves all of this alone. The context is tracked wherever it is passed, so the type
  // named here need not say that it is a capability, and may be one.
  type Context

  def accessWith(name: Text, value: Origin)(using Context^, Tactic[Failure]): Origin =
    access(name, value)

  def repeatedWith(name: Text, value: Origin)(using Context^, Tactic[Failure]): List[Origin] =
    repeated(name, value)

  def entriesWith(name: Text, value: Origin)(using Context^, Tactic[Failure])
  :   List[(Text, Origin)] =

    entries(name, value)

  // What a member whose multiplicity is `Series` reads as: a collection of the specification's
  // own, of the values `name` has in `value`, which reads each with `read` when it is asked
  // for, in whatever context it is then given. `context` is the context a record was made
  // with, which a collection keeps in its type so as to ask for the same.
  type Series[context <: Context, +element]

  def series[context <: Context, element]
    ( name: Text, value: Origin, read: (Origin, context^, Tactic[Failure]) -> element )
    ( using context^, Tactic[Failure] )
  :   Series[context, element] =

    panic(m"this specification has no series")

  // A pure function (`->`): the built Record retains the transform, and a capturing one would
  // make the record itself a capability, which Record's pure self type (rightly) forbids.
  def build(data: Origin, transform: Text -> Origin -> Any): Record = Record(data, transform)

  // The typed record over `value`, refined with one member per field, read lazily: each access
  // runs the field's accessor on the record's data. Inlined at the call site, where the receiver
  // is the specification object, so the macro can find that object by its type and evaluate its
  // `fields` — which is why the object must be compiled before the calling code.
  transparent inline def record(inline value: Origin): Record =
    ${Specification.record[this.type, Origin, Form, Failure]('value)}

  // A named tuple with one element per field, in the specification's order, read eagerly: every
  // field's accessor runs when the tuple is built, so a field fails then, and its element has
  // the successful type, its `raises` clause discharged by a `Tactic` at the call site.
  transparent inline def tuple(inline value: Origin): NamedTuple.AnyNamedTuple =
    ${Specification.tuple[this.type, Origin, Form, Failure]('value)}
