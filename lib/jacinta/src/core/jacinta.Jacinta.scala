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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package jacinta

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// Compile-time navigation for schema-typed `Json` values. A `Json of P from R`
// carries a phantom *position* (`Topic = P`, a Scala model type) within a *root
// schema* (`Origin = R`). The `Dynamic` methods on `Json` are `transparent
// inline` and delegate here: when the receiver's position is bound and the field
// name is a literal, the macro looks the field up in `P`'s structure and yields a
// `Json of <field-type> from R`; otherwise it falls back to the plain
// (`DynamicJsonEnabler`-gated) runtime access, exactly as before.
object Jacinta:

  // Every `type X = …` member of a (possibly nested) refinement, by name.
  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :     Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => Map()

  // Builds the refined type `Json of <position> from <root>`.
  private def jsonType(using quotes: Quotes)
      ( position: quotes.reflect.TypeRepr, root: quotes.reflect.TypeRepr )
  :     quotes.reflect.TypeRepr =

    import quotes.reflect.*

    Refinement
      ( Refinement(TypeRepr.of[Json], "Topic", TypeBounds(position, position)),
        "Origin",
        TypeBounds(root, root) )

  // The single ordered-collection element type of `repr`, if it is one (`List`,
  // `Vector`, `Seq`, `LazyList`, `Array`, `IArray`); `Set` is excluded as it has
  // no positional index.
  private def elementType(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :     Optional[quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    repr.dealias match
      case AppliedType(constructor, List(element))
      if repr <:< TypeRepr.of[Seq[Any]] || constructor.typeSymbol == defn.ArrayClass =>
        element

      case _ =>
        Unset

  // Reads `Topic` (position) and `Origin` (root) from a receiver, if present.
  private def receiver(using quotes: Quotes)(self: Expr[Json])
  :     Optional[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =

    import quotes.reflect.*
    val members = refinements(self.asTerm.tpe.widen)

    members.at(t"Topic").let: position =>
      (position, members.at(t"Origin").or(position))

  def select(self: Expr[Json], field: Expr[String]): Macro[Json] =
    import quotes.reflect.*

    // Plain (unverified) access: gate on the enabler (resolved at the call site),
    // then read the field totally.
    def plain: Expr[Json] =
      if Expr.summon[DynamicJsonEnabler].isEmpty
      then halt(m"""dynamic field access on an unverified `Json` requires
                    `import dynamicJsonAccess.enabled` (or verify the value against a schema first)""")

      '{$self.selectField($field)}

    receiver(self) match
      case (position, root) => field.value match
        case Some(name) => position.typeSymbol.caseFields.find(_.name == name) match
          case Some(member) =>
            jsonType(position.memberType(member), root).asType.absolve match
              case '[type result <: Json; result] =>
                '{$self.selectField(${Expr(name)}).asInstanceOf[result]}

          case None =>
            halt(m"the schema position ${position.show} has no field $name")

        case None =>
          plain

      case _ =>
        plain

  def index(self: Expr[Json], idx: Expr[Int]): Macro[Json] =
    import quotes.reflect.*

    receiver(self) match
      case (position, root) =>
        val element = elementType(position)

        if element.absent
        then halt(m"the schema position ${position.show} is not an indexable array type")

        jsonType(element.vouch, root).asType.absolve match
          case '[type result <: Json; result] =>
            '{$self.selectIndex($idx).asInstanceOf[result]}

      case _ => Expr.summon[Tactic[JsonError]] match
        case Some(tactic) =>
          '{$self.indexValue($idx)(using $tactic)}

        case None =>
          halt(m"""indexing a `Json` array may raise `JsonError`; a `Tactic[JsonError]`
                   must be in scope (e.g. via `raises JsonError`)""")

  def applied(self: Expr[Json], field: Expr[String], idx: Expr[Int]): Macro[Json] =
    import quotes.reflect.*

    def plain: Expr[Json] =
      if Expr.summon[DynamicJsonEnabler].isEmpty
      then halt(m"""dynamic field access on an unverified `Json` requires
                    `import dynamicJsonAccess.enabled` (or verify the value against a schema first)""")

      Expr.summon[Tactic[JsonError]] match
        case Some(tactic) => '{$self.selectField($field).indexValue($idx)(using $tactic)}

        case None =>
          halt(m"""indexing a `Json` array may raise `JsonError`; a `Tactic[JsonError]`
                   must be in scope (e.g. via `raises JsonError`)""")

    receiver(self) match
      case (position, root) => field.value match
        case Some(name) => position.typeSymbol.caseFields.find(_.name == name) match
          case Some(member) =>
            val element = elementType(position.memberType(member))

            if element.absent
            then halt(m"the field $name of ${position.show} is not an indexable array")

            jsonType(element.vouch, root).asType.absolve match
              case '[type result <: Json; result] =>
                '{$self.selectField(${Expr(name)}).selectIndex($idx).asInstanceOf[result]}

          case None =>
            halt(m"the schema position ${position.show} has no field $name")

        case None =>
          plain

      case _ =>
        plain
