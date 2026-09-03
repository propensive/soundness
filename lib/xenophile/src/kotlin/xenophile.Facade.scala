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

import scala.language.dynamics

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*
import stenography.*
import vacuous.*
import rudiments.sortingAlgorithms.timsort

object Facade extends prophesy.Completable:

  // Dynamic tab completions, invoked reflectively through `Completable` by a completion
  // engine: the members of the underlying Kotlin class, rendered in Kotlin syntax.
  def completions(using quotes: scala.quoted.Quotes)
    ( receiver: quotes.reflect.TypeRepr, prefix: Text )
  :   List[prophesy.Completion] =

    Xenophile.refinements(receiver).to(Map)(t"Transport").lay(Nil): transport =>
      val className =
        transport.dealias.classSymbol.map(_.fullName.replace("$.", ".").nn.tt)

      className.map: className =>
        KotlinDialect.resolve(className).lay(List[prophesy.Completion]()): members =>
          members.to[List].order(_(0).s).map: (name, prototype) =>
            // The `Optional` parameter list is bound to a typed local before it is read
            // (`wildApprox`).
            val declared: Optional[List[Foreign.Type]] = prototype.parameters

            val kind = declared.lay(prophesy.Completion.Kind.Term): _ =>
              prophesy.Completion.Kind.Method

            val signature = KotlinFacade.rendered(name, prototype)
            prophesy.Completion(name, kind, Syntax.Symbolic(signature))

      . getOrElse(Nil)

  // The sole constructor: wraps a JVM value originating from Kotlin. Zero-cost at the boundary;
  // the value travels verbatim and every navigation is a direct call on it.
  def apply[underlying](value: underlying): Facade over underlying =
    val facade = new Facade:
      def raw: Any = value

    facade.asInstanceOf[Facade over underlying]

// An eager facade over a live Kotlin/JVM value, typed `Facade over T` where `T` is the real
// Scala view of the underlying class — type arguments included, so `Facade over Pair[Text,
// Text]` knows its components are `Text`. Unlike `Foreign` (a deferred expression AST for
// cross-runtime ecosystems), every member access materializes immediately as a direct,
// statically-typed JVM call, transparently typed by the Kotlin metadata of the underlying
// class: nullable results become `Optional`, `kotlin.String` becomes `Text`, and Kotlin-typed
// results are wrapped as further facades.
trait Facade extends Selectable, Dynamic, Transportive:
  // The underlying value at its erased runtime type; the emission machinery's accessor. User
  // code should prefer `unwrap`, which recovers the precise static type.
  def raw: Any

  // The underlying value (`k` for "Kotlin"), at the full Scala type the facade records: the
  // escape hatch back to plain Java-level interop, typed so no cast is needed at the boundary.
  transparent inline def k: Any = ${KotlinFacade.unwrapped('this)}

  transparent inline def selectDynamic(name: String): Any =
    ${KotlinFacade.select('this, 'name)}

  transparent inline def applyDynamic(name: String)(inline arguments: Any*): Any =
    ${KotlinFacade.applied('this, 'name, 'arguments)}

  transparent inline def applyDynamicNamed(name: String)(inline arguments: (String, Any)*)
  :   Any =

    ${KotlinFacade.appliedNamed('this, 'name, 'arguments)}

  inline def updateDynamic(name: String)(inline value: Any): Unit =
    ${KotlinFacade.update('this, 'name, 'value)}

  // A data class's components as a Scala tuple: `pair.tuple` is `(first, second)`.
  transparent inline def tuple: Any = ${KotlinFacade.tuple('this)}

  // An explicit copy of a Kotlin/Java collection into the immutable Scala equivalent, with
  // `String` elements reading as `Text`.
  transparent inline def scala: Any = ${KotlinFacade.scalaCollection('this)}

  // Kotlin's index-access operators: `facade(key)` reaches `operator fun get`, and
  // `facade(key) = value` reaches `operator fun set`.
  transparent inline def apply(inline arguments: Any*): Any =
    ${KotlinFacade.applied('this, '{"get"}, 'arguments)}

  inline def update(inline arguments: Any*): Unit =
    ${KotlinFacade.discarded('this, '{"set"}, 'arguments)}
