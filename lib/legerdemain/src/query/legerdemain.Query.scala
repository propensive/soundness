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
package legerdemain

import scala.language.dynamics

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*
import wisteria.*
import fulminate.*

object Query extends Dynamic:
  def apply(): Query = new Query(Nil)
  def apply(parameter: Text): Query = new Query(List(t"" -> parameter))

  given encodable: Query is Encodable in Text =
    _.values.map: (key, value) => t"${key.urlEncode}=${value.urlEncode}"
    . join(t"&")

  given decodable: Query is Decodable in Text = text => Query:
    text.cut(t"&").map: next =>
      next.cut(t"=", 2) match
        case List(key, value) => (key.urlDecode, value.urlDecode)
        case List(key)        => (key.urlDecode, t"")
        case _                => (t"", t"")

  object EncodableDerivation extends ProductDerivation[[Type] =>> Type is Encodable in Query]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Query =

      value =>
        Query:
          // Via the stdlib view: inline re-elaboration freshens the frozen array, defeating
          // both `to[List]` and the compat `toList`.
              fields(value) { [field] => field => contextual.encoded(field).prefix(label) }
              . readable.toList
              . to(proscenium.List)
              . flatMap(_.values)

  object DecodableDerivation extends ProductDerivation[[Type] =>> Type is Decodable in Query]:
    // Each outer `focus` runs *after* the inner one (contingency's try/finally order), so a
    // nested record's error must be extended at the ROOT side, landing at `outer.inner` rather
    // than `inner.outer`.
    private def prepend(pointer: Pointer, root: Text): Pointer = pointer match
      case Pointer.Self                 => Pointer(root)
      case Pointer.Child(parent, label) => prepend(parent, root)(label)
    // Scans the venture slots and constructs positionally through the threaded `Mirror` — a
    // plain method: the argument buffer must not be allocated inside an inline expansion,
    // where its fresh root capability leaks into the expansion site's capture sets. Returns
    // an unused null when any slot failed: the caller's accruing scope is tainted, so the
    // result is discarded.
    private final class ArrayProduct(values: Array[Any]^{}) extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int = values.length
      def productElement(index: Int): Any = values.readUnchecked(index)

    private def gate[derivation <: Product]
      ( reflection: ProductReflection[derivation],
        slots:      Array[Venture[Any]]^{},
        active:     Boolean )
    :   derivation =

      var failed = false
      var slot = 0

      if active then
        while slot < slots.length do
          if !slots.readUnchecked(slot).ready then failed = true
          slot += 1

      if failed then null.asInstanceOf[derivation]
      else
        val arguments = Array.allocate[Any](slots.length)
        slot = 0

        while slot < slots.length do
          arguments(slot) = slots.readUnchecked(slot).vouch
          slot += 1

        reflection.fromProduct(ArrayProduct(Array.freeze(arguments)))

    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Query =

      // The `Foci` is threaded from the derivation site: a `provide` here would mint a fresh,
      // inert instance, silently discarding the pointer of every accrued error and hiding the
      // ambient scope's registrations — which is what stopped `validate` working over form
      // decoding. A SINGLE field traversal (each additional wisteria traversal re-summons every
      // field's decoder, multiplying inline expansion exponentially with nesting depth), with
      // construction gated on all fields decoding cleanly, so refinement-typed constructors
      // never run on fallback values and report phantom validation errors.
      val foci = infer[Foci[Pointer]]

      value =>
        val active = foci.active

        val slots: Array[Venture[Any]]^{} =
          contexts[derivation]()[Venture[Any]]:
            [field] => context =>
              if !active then Venture(context.decoded(value(label)))
              else
                // `focus`'s `Foci` is passed explicitly: an inline def's using parameter would
                // otherwise resolve at this DEFINITION site (to the inert default), not at the
                // expansion site where the validation scope's instance is in context.
                focus(using foci)(prior.lay(Pointer(label))(prepend(_, label))):
                  val before = foci.length
                  val decoded: field = context.decoded(value(label))
                  if foci.length > before then Venture.failed else Venture(decoded)

        gate[derivation](infer[ProductReflection[derivation]], slots, active)

  given booleanEncodable: Boolean is Encodable in Query =
    boolean => if boolean then Query(t"on") else Query()

  given booleanDecodable: Boolean is Decodable in Query = _().present

  inline given encodable: [value] => value is Encodable in Query = summonFrom:
    case given (`value` is Encodable in Text) => value => Query(value.encode)

    case given ProductReflection[`value` & Product] =>
      EncodableDerivation.conjunction[value & Product].asInstanceOf[value is Encodable in Query]

  // The URL-encoded form, agreeing with `encodable` and `queryString`: `Query.show` must
  // not disagree with `Query.encode` (#1500). The debug rendering lives in `inspectable`.
  given showable: Query is Showable = _.queryString

  // Declared explicitly because `Inspectable`'s derivation prefers `Encodable in Text`
  // over `Showable`, which would otherwise render `.inspect` URL-encoded too.
  given inspectable: Query is Inspectable =
    _.values.map { case (key, value) => t"$key = \"${value}\"" }.join(t", ")

  inline given decodable: [value] => value is Decodable in Query =
    summonFrom:
      case given (`value` is Decodable in Text) =>
        provide[Tactic[Query.Error]]:
          summonFrom:
            case default: Default[`value`] =>
              _().let(_.as).or:
                raise(Query.Error(Query.Error.Reason.Missing))
                default()

            case _ =>
              query =>
                query().lay:
                  // Under an accruing scope, a missing required parameter records its error
                  // and returns an unused null — the caller's slot is marked failed via the
                  // foci delta, so siblings keep accruing. Fail-fast scopes abort as before.
                  if infer[Foci[Pointer]].active
                  then
                    raise(Query.Error(Query.Error.Reason.Missing))
                    null.asInstanceOf[value]
                  else abort(Query.Error(Query.Error.Reason.Missing))
                . apply(_.as)

      case given ProductReflection[`value` & Product] =>
        DecodableDerivation.conjunction[value & Product].asInstanceOf[value is Decodable in Query]

  inline def applyDynamicNamed(method: "make")(inline parameters: (Label, Any)*): Query =
    ${legerdemain.internal.query('parameters)}

  def apply(parameters: List[(Text, Text)]): Query = new Query(parameters)

  given addable: Query is Addable by Query to Query =
    Addable: (left, right) => new Query((left.values.stdlib ++ right.values.stdlib).to(List))

  // QueryError → Query.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Missing extends Reason(1)

    given communicable: Reason is Communicable =
      case Reason.Missing => m"the parameter was not present in the query string"

  case class Error(reason: Query.Error.Reason)(using Diagnostics)
  extends fulminate.Error(205, reason.number)(m"the query parameter could not be read because $reason")

case class Query private (values: List[(Text, Text)]) extends Dynamic:
  // private lazy val map: Map[Text, Text | List[Text]] = values.groupMap(_(0))(_(1))
  def append(more: Query): Query = new Query((values.stdlib ++ more.values.stdlib).to(List))
  def nil: Boolean = values.nil

  @targetName("appendAll")
  infix def ++ (query: Query) = Query((values.stdlib ++ query.values.stdlib).to(List))


  def selectDynamic[result](label: String)(using erased parametric: label.type is Parametric to result)
    ( using decodable: result is Decodable in Query )
  :   result =

    decodable.decoded(apply(label.tt))


  def updateDynamic(label: String)[result: Encodable in Query]
    ( using erased parametric: label.type is Parametric to result )
    ( value: result )
  :   Query =

    val updates = value.encode.values.stdlib

    val values2 =
      if updates.length == 1 && updates(0)(0) == ""
      then (label.tt, updates(0)(1)) :: values
      else (values.stdlib ++ (updates.map { (key, value) => (t"$label.$key", value) })).to(List)

    new Query(values2)


  def at[value: Decodable in Text](name: Text): Optional[value] = apply(name)().let(_.as)
  def as[value: Decodable in Query]: value tracks Pointer = value.decoded(this)

  def apply(): Optional[Text] = values match
    case List((t"", value)) => value
    case other              => Unset

  def apply(label: Text): Query =
    val prefix = label+t"."

    Query:

        values.stdlib.collect:
          case (`label`, value)                   => (t"", value)
          case (key, value) if key.starts(prefix) => (key.skip(prefix.length), value)
        . to(List)

  def prefix(string: Text): Query = Query:
    values.map: (key, value) =>
      if key.length == 0 then string -> value else t"$string.$key" -> value

  def queryString: Text =
    values.map: (key, value) =>
      if key.length == 0 then value.urlEncode else t"${key.urlEncode}=${value.urlEncode}"

    . join(t"&")
