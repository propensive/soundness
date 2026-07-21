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
package abacist

import scala.collection.immutable.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import gossamer.*
import hieroglyph.*
import quantitative.*
import rudiments.*
import symbolism.*
import vacuous.*

object internal:
  import quantitative.internal.*

  // Encodes the (reversed, smallest-first) unit values against the (reversed, smallest-first)
  // multipliers into the packed `Long` representation.
  private def encode(using Quotes)(multipliers: List[Multiplier], values: List[Expr[Int]])
  :   Expr[Long] =

    def recur(multipliers: List[Multiplier], values: List[Expr[Int]], expr: Expr[Long])
    :   Expr[Long] =

      values match
        case Nil => expr

        case unitValue :: valuesTail =>
          multipliers match
            case Multiplier(unitPower, subdivision, max) :: tail =>
              unitValue.value match
                case Some(unitValue) =>
                  if unitValue < 0
                  then halt:
                    m"the value for the ${unitPower.ref.name} unit ($unitValue) cannot be negative"
                  else if unitValue >= max
                  then halt:
                    m"""
                      the value for the ${unitPower.ref.name} unit $unitValue must be less than $max
                    """

                  recur
                    ( tail,
                      valuesTail,
                      '{$expr + (${Expr(unitValue.toLong)}*${Expr(subdivision)})} )

                case None =>
                  recur(tail, valuesTail, '{$expr + ($unitValue.toLong*${Expr(subdivision)})})

            case Nil => halt:
              m"""
                ${values.length} unit values were provided, but this Quanta has fewer units
              """

    recur(multipliers, values, '{0L})


  private def inputsOf(using Quotes)(values: Expr[Seq[Int]]): List[Expr[Int]] =
    values.absolve match
      case Varargs(values) => values.to(List).reverse


  // Construction (`Quanta(...)`): `base` and `form` come from the expected type, so the cascade
  // is built directly from them rather than by decomposing an opaque `Quanta` type (whose parent
  // would dealias to `Long` within this scope).
  // Returns just the encoded `Long`; the caller casts it to `Quanta[base] { type Form = form }` in an
  // inline body. Producing the refined `Quanta` type *inside* the splice makes capture checking box the
  // `Form` tuple with a fresh capture set, which then fails to unify with the (independently boxed)
  // expected type; casting in the inline body keeps the refined type out of the macro and clean.
  def assembleParts[base <: AnyUnit: Type, form <: Divisions: Type](values: Expr[Seq[Int]])
  :   Macro[Long] =

    import quotes.reflect.*

    // A bare single-unit `Quanta` (no `Form` in the expected type) leaves `form` unconstrained,
    // so it is inferred as `Nothing`, `Any`, or its bound `Divisions`; either way the form is
    // empty. (An `=:=` check is needed because the `'[Any]` quote pattern would match every type.)
    val formRepr = TypeRepr.of[form]
    val empty = List(TypeRepr.of[Nothing], TypeRepr.of[Any], TypeRepr.of[Divisions])
    val formUnits = if empty.exists(_ =:= formRepr) then Nil else elements(formRepr)

    val multipliers = multipliersOf(TypeRepr.of[base] :: formUnits)

    encode(multipliers.reverse, inputsOf(values))


  def describeQuanta[quanta: Type](count: Expr[quanta])
  :   Macro[ListMap[Text, Long]] =

    def recur(slices: List[Multiplier], expr: Expr[ListMap[Text, Long]])
    :   Expr[ListMap[Text, Long]] =

      slices match
        case Nil => expr

        case (slice@Multiplier(unitPower, subdivision, max)) :: tail =>
          val power: Text = if unitPower.power == 1 then "".tt else
            unitPower.power.toString.tt.tr(_.superscript.or(' '))

          val value = '{(($count.asInstanceOf[Long]/${Expr(subdivision)})%(${Expr(max)}))}

          recur
            ( tail,
              ' {
                  ( $expr.updated
                      ( ${unitPower.ref.designation}+${Expr(power)}.asInstanceOf[Text], $value ) )
                } )

    recur(multipliers[quanta], '{ListMap()})


  def multiplyQuanta[quanta: Type]
    ( count: Expr[quanta], multiplier: Expr[Double], division: Boolean )
  :   Macro[Any] =

    val long = '{$count.asInstanceOf[Long]}

    if division then '{Quanta.fromLong[quanta](($long/$multiplier + 0.5).toLong)}
    else '{Quanta.fromLong[quanta](($long*$multiplier + 0.5).toLong)}


  def toQuantity[quanta: Type](count: Expr[quanta]): Macro[Any] =
    val lastUnit = multipliers[quanta].last
    val quantityUnit = lastUnit.unitPower.ref.dimensionRef.principal
    val ratioExpr = ratio(lastUnit.unitPower.ref, quantityUnit, lastUnit.unitPower.power)

    quantityUnit.power(1).asType.absolve match
      case '[type quantity <: Measure; quantity] =>
        '{Quantity[quantity]($count.asInstanceOf[Long]*$ratioExpr)}


  def fromQuantity[quantity <: Measure: Type, result: Type]
    ( quantity: Expr[Quantity[quantity]] )
  :   Macro[result] =

    import quotes.reflect.*

    val lastUnit = multipliers[result].last.unitPower
    val quantityUnit = readUnitPower(TypeRepr.of[quantity].dealias)
    val ratioExpr = ratio(quantityUnit.ref, lastUnit.ref, lastUnit.power)

    '{($quantity.value*$ratioExpr + 0.5).toLong.asInstanceOf[result]}


  def get[quanta: Type, unit <: Units[1, ? <: Dimension]: Type]
    ( value: Expr[quanta] )
  :   Macro[Int] =

    import quotes.reflect.*

    val lookupUnit = readUnitPower(TypeRepr.of[unit])

    val multiplier: Multiplier = multipliers[quanta].seek(_.unitPower == lookupUnit).or:
      halt(557, m"the Quanta does not include this unit")

    '{(($value.asInstanceOf[Long]/${Expr(multiplier.subdivision)})%${Expr(multiplier.max)}).toInt}


  def collapse[quanta: Type](count: Expr[quanta], length: Expr[Int]): Macro[Any] =
    import quotes.reflect.*

    val drop = length.valueOrAbort
    val (parent, formElements) = decompose(TypeRepr.of[quanta])

    if drop < 0 || drop > formElements.length then halt:
      m"""
        cannot collapse $drop units; this Quanta has ${formElements.length} divisions above its base
        unit
      """

    val consCtor = TypeRepr.of[Any *: EmptyTuple].absolve match
      case AppliedType(tycon, _) => tycon

    def tupleType(elements: List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple]): (head, tail) =>
        consCtor.appliedTo(List(head, tail))

    val remaining = formElements.dropRight(drop)

    val resultType =
      if remaining.isEmpty then parent
      else Refinement(parent, "Form", TypeBounds(tupleType(remaining), tupleType(remaining)))

    resultType.asType.absolve match
      case '[result] => '{Quanta.fromLong[result]($count.asInstanceOf[Long])}


  private case class Multiplier(unitPower: UnitPower, subdivision: Int, max: Int)

  private def elements(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    tpe.dealias.asType match
      case '[EmptyTuple]   => Nil
      case '[head *: tail] => TypeRepr.of[head] :: elements(TypeRepr.of[tail])
      case _               => List(tpe)


  private def decompose(using Quotes)(repr: quotes.reflect.TypeRepr)
  :   (quotes.reflect.TypeRepr, List[quotes.reflect.TypeRepr]) =

    import quotes.reflect.*

    repr.dealias match
      case Refinement(parent, "Form", info) =>
        val form = info match
          case TypeBounds(_, hi) => hi
          case other             => other

        (parent, elements(form))

      // An unreduced `prepositional.in[Quanta[base], form]` applied alias (the `Quanta` opaque
      // can dealias to `Long` here, so the refinement is not always recovered).
      case AppliedType(_, List(parent, form)) =>
        (parent, elements(form))

      case other =>
        (other, Nil)


  // Computes the `Multiplier`s from the units in smallest-to-largest (base-first) order.
  private def multipliersOf(using Quotes)(units: List[quotes.reflect.TypeRepr]): List[Multiplier] =
    val cascade: List[UnitPower] = units.map(readUnitPower)
    val dimension = cascade.head.ref.dimensionRef

    cascade.tail.foreach: unitPower =>
      if unitPower.ref.dimensionRef != dimension then halt:
        m"""
          the Quanta type incorrectly mixes units of ${unitPower.ref.dimensionRef.name} and
          ${dimension.name}
        """

    def recur(todo: List[UnitPower], units: List[Multiplier] = Nil): List[Multiplier] = todo match
      case Nil => units

      case head :: tail =>
        val value = ratio(head.ref, cascade.head.ref, head.power).valueOrAbort
        val value2 = tail.prim.let(_.ref).let(ratio(_, head.ref, head.power).valueOrAbort + 0.5)

        recur
          ( tail,
            Multiplier(head, (value + 0.5).toInt, value2.let(_.toInt).or(Int.MaxValue)) :: units )

    recur(cascade)


  private def multipliers[quanta: Type](using Quotes): List[Multiplier] =
    import quotes.reflect.*

    val (parent, formElements) = decompose(TypeRepr.of[quanta])

    // Match the application directly without dealiasing: the `Quanta` opaque type can dealias
    // to `Long` in this scope, which would discard the base unit.
    val base = parent match
      case AppliedType(_, List(baseRepr)) => baseRepr
      case _                              => halt(m"this is not a valid Quanta type")

    multipliersOf(base :: formElements)
