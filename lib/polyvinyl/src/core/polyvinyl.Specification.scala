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

trait Specification extends Original:
  type Origin
  protected type Origin0 = Origin
  type Form <: { type Origin = Origin0 }

  // The fields in order: a record's members are unordered, but a named tuple's elements follow
  // this order.
  def fields: List[(Text, Member)]

  // A pure function (`->`): the built Record retains the transform, and a capturing one would
  // make the record itself a capability, which Record's pure self type (rightly) forbids.
  def build(data: Origin, transform: Text -> Origin -> Any): Record
  def access(name: Text, value: Origin): Origin

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

      val refined = members.foldLeft(TypeRepr.of[Record]): (refined, field) =>
        Refinement(refined, field.name.s, field.tpe)

      val caseDefs = members.map: field =>
        CaseDef(Literal(StringConstant(field.name.s)), None, field.accessor.asTerm)

      val fallback = CaseDef(Wildcard(), None, '{???}.asTerm)

      val transform: Expr[Text -> Origin -> Any] =
        ' {
            (name: Text) =>
              ${Match('name.asTerm, caseDefs :+ fallback).asExprOf[Origin => Any]}
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

    // Expands one member to its type and accessor. `nested` expands a nested record's fields in
    // this mode, to the type they produce and the function making that value from the nested
    // origin value.
    private def expand
      ( field:  (Text, Member),
        eager:  Boolean,
        nested: List[(Text, Member)] => (TypeRepr, Expr[Origin -> Any]) )
    :   Field =

      field.absolve match
        case (name, Member.Value(label, params*)) =>
          ConstantType(StringConstant(label.s)).asType.absolve match
            case '[type label <: Label; label] =>
              Expr.summon[label is Intensional in Form from Origin].absolve match
                case None =>
                  halt:
                    m"could not find an Intensional instance for the field $name with type $label"

                case
                  Some('{$accessor: label `is` Intensional `in` Form `from` Origin `to` result}) =>
                    val read: Expr[Origin -> result] =
                      ' {
                          data =>
                            $accessor.transform
                              ( $target.access(${Expr(name)}, data),
                                List.from(${Expr(params.to(List).stdlib)}) )
                        }

                    if eager then discharge(name, TypeRepr.of[result], read)
                    else Field(name, TypeRepr.of[result], read)

        case (name, Member.Record(label, fields)) =>
          ConstantType(StringConstant(label.s)).asType.absolve match
            case '[type label <: Label; label] =>
              Expr.summon[label is Structural[?] in Form from Origin].absolve match
                case None =>
                  halt:
                    m"could not find a Structural instance for the field $name with type $label"

                case Some
                  ( ' {
                        type constructor[_]
                        $structural: (label `is` Structural[constructor] `in` Form `from` Origin)
                      } ) =>
                  val (tpe, make) = nested(fields)

                  tpe.asType.absolve match
                    case '[element] =>
                      val makeElement: Expr[Origin -> element] =
                        '{field => $make(field).asInstanceOf[element]}

                      val accessor: Expr[Origin -> Any] =
                        ' {
                            data =>
                              $structural.transform[element]
                                ( $target.access(${Expr(name)}, data), $makeElement )
                          }

                      Field(name, TypeRepr.of[constructor[element]], accessor)

    // In a tuple, a field whose result is `success raises error` becomes an element of type
    // `success`: the `Tactic` is found at the call site and applied when the tuple is built. The
    // `raises` alias is recognised by its expansion, a context function over a `Tactic`.
    private def discharge(name: Text, result: TypeRepr, read: Expr[Origin -> Any]): Field =
      def strip(repr: TypeRepr): TypeRepr = repr match
        case AnnotatedType(inner, _) => strip(inner)
        case other                   => other

      val fallible = result.dealias match
        case AppliedType(_, scala.List(tactic, success)) if result.dealias.isContextFunctionType =>
          strip(tactic) match
            case AppliedType(tycon, scala.List(error)) if tycon.typeSymbol == tacticSymbol =>
              Some((success, error))

            case _ => None

        case _ => None

      fallible match
        case Some((success, error)) =>
          success.asType.absolve match
            case '[success] => strip(error).asType.absolve match
              case '[type error <: Hazard; error] =>
                Expr.summon[Tactic[error]].absolve match
                  case None =>
                    val hazard = TypeRepr.of[error].show.tt
                    halt(m"the field $name may raise $hazard, but no Tactic for it is in scope")

                  case Some(tactic) =>
                    val accessor: Expr[Origin -> Any] =
                      ' {
                          data =>
                            given Tactic[error] = $tactic
                            $read(data).asInstanceOf[Tactic[error] ?=> success]
                        }

                    Field(name, TypeRepr.of[success], accessor)

        case None =>
          Field(name, result, read)
