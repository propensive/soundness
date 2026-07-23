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
package stratiform

import scala.collection.immutable.Seq

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import gossamer.*
import rudiments.*
import vacuous.*

// Compile-time navigation for schema-typed `Tel` values. A `Tel of P from R`
// carries a phantom *position* (`Topic = P`, a Scala model type) within a *root
// schema* (`Origin = R`). The `Dynamic` methods on `Tel` are `transparent inline`
// and delegate here: when the receiver's position is bound and the field name is a
// literal, the macro looks the field up in `P`'s structure and yields a `Tel of
// <field-type> from R`; otherwise it falls back to the plain
// (`DynamicTelEnabler`-gated) runtime access. Mirrors `jacinta.internal`.
object Stratiform:

  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   scala.collection.immutable.Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => scala.collection.immutable.Map()

  // Builds the refined type `Tel of <position> from <root>`.
  private def telType(using quotes: Quotes)
    ( position: quotes.reflect.TypeRepr, root: quotes.reflect.TypeRepr )
  :   quotes.reflect.TypeRepr =

    import quotes.reflect.*

    Refinement
      ( Refinement(TypeRepr.of[Tel], "Topic", TypeBounds(position, position)),
        "Origin",
        TypeBounds(root, root) )

  // The single ordered-collection element type of `repr`, if it is one.
  private def elementType(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   Optional[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    // The opaque prelude `List`/`Series` don't conform to `Seq`, so match their
    // type constructors by symbol as well.
    val listSym = TypeRepr.of[proscenium.List[Any]].typeSymbol
    val seriesSym = TypeRepr.of[proscenium.Series[Any]].typeSymbol

    repr.dealias match
      case AppliedType(constructor, scala.collection.immutable.List(element))
      if repr <:< TypeRepr.of[Seq[Any]] || constructor.typeSymbol == defn.ArrayClass
      || constructor.typeSymbol == listSym || constructor.typeSymbol == seriesSym =>
        element

      case _ =>
        Unset

  // Reads `Topic` (position) and `Origin` (root) from a receiver, if present.
  private def receiver(using quotes: Quotes)(self: Expr[Tel])
  :   Optional[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =

    import quotes.reflect.*
    val members = Map.of(refinements(self.asTerm.tpe.widen))

    members.at(t"Topic").let: position =>
      (position, members.at(t"Origin").or(position))

  def select(self: Expr[Tel], field: Expr[String]): Macro[Tel] =

    def plain: Expr[Tel] =
      if Expr.summon[DynamicTelEnabler].nil
      then
        halt:
          m"""
            dynamic field access on an unverified `Tel` requires `import dynamicTelAccess.enabled`
            (or verify the value against a schema first)
          """

      '{$self.selectField($field)}

    receiver(self) match
      case (position0, root0) =>
        val position = position0.asInstanceOf[quotes.reflect.TypeRepr]
        val root = root0.asInstanceOf[quotes.reflect.TypeRepr]

        field.value match
          case Some(name) => position.typeSymbol.caseFields.find(_.name == name) match
            case Some(member) =>
              telType(position.memberType(member), root).asType.absolve match
                case '[type result <: Tel; result] =>
                  '{$self.selectField(${Expr(name)}).asInstanceOf[result]}

            case None =>
              halt(m"the schema position ${position.show} has no field $name")

          case None =>
            plain

      case _ =>
        plain

  def applied(self: Expr[Tel], field: Expr[String], idx: Expr[Int]): Macro[Tel] =
    import quotes.reflect.*

    def plain: Expr[Tel] =
      if Expr.summon[DynamicTelEnabler].nil then halt:
        m"""
          dynamic field access on an unverified `Tel` requires `import dynamicTelAccess.enabled`
          (or verify the value against a schema first)
        """

      '{$self.selectFieldIndex($field, $idx)}

    receiver(self) match
      case (position0, root0) =>
        val position = position0.asInstanceOf[TypeRepr]
        val root = root0.asInstanceOf[TypeRepr]

        field.value match
          case Some(name) => position.typeSymbol.caseFields.find(_.name == name) match
            case Some(member) =>
              val element = elementType(position.memberType(member))

              if element.absent
              then halt(m"the field $name of ${position.show} is not an indexable collection")

              telType(element.vouch, root).asType.absolve match
                case '[type result <: Tel; result] =>
                  '{$self.selectRepeatedField(${Expr(name)}, $idx).asInstanceOf[result]}

            case None =>
              halt(m"the schema position ${position.show} has no field $name")

          case None =>
            plain

      case _ =>
        plain
