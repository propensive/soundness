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

object Foreign extends prophesy.Completable:
  // A foreign expression: a reference to a named foreign value, a member selection (recording the
  // `owner` foreign type it is selected from, which backends needing that type's layout — e.g. the
  // native evaluator — use, while self-describing backends like JSON ignore it), a function
  // application, an indexed access into an array-typed value, or a literal operand.
  enum Expression:
    case Reference(name: Text)
    case Select(target: Expression, member: Text, owner: Text)
    case Apply(target: Expression, arguments: List[Expression])
    case Index(target: Expression, index: Expression)
    case Literal(value: Any)

  // A foreign type: a named type, a union of alternatives, or a generic application such as a
  // TypeScript `Map<number, string>` or a C pointer `T*`.
  enum Type:
    case Named(name: Text)
    case Union(members: List[Type])
    case Applied(constructor: Text, arguments: List[Type])

    def text: Text = this match
      case Named(name)         => name
      case Union(members)      => members.map(_.text).join(t"|")
      case Applied(name, args) => t"$name<${args.map(_.text).join(t", ")}>"

  def make(tree: Expression): Foreign = new Foreign:
    def expr: Expression = tree

  transparent inline def apply[name <: Label, origin]: Foreign = ${Xenophile.root[name, origin]}

  // Dynamic tab completions, invoked reflectively through `Completable` by a completion engine
  // (such as Harlequin's): the members of the receiver's foreign type, enumerated from the
  // definitions resource its `Locus` refinement records. A receiver with a compound topic (a
  // union), or one whose root recorded no `Locus`, offers nothing.
  def completions(using quotes: scala.quoted.Quotes)
    ( receiver: quotes.reflect.TypeRepr, prefix: Text )
  :   List[prophesy.Completion] =

    import quotes.reflect.*

    val members = Xenophile.refinements(receiver)

    members.at(t"Topic").lay(Nil): topicRepr =>
      members.at(t"Origin").lay(Nil): originRepr =>
        members.at(t"Locus").lay(Nil): locusRepr =>
          (topicRepr.dealias, locusRepr.dealias) match
            case (ConstantType(StringConstant(topic)), ConstantType(StringConstant(locus))) =>
              Xenophile.definitions(originRepr, locus.tt).at(topic.tt).lay(Nil): prototypes =>
                prototypes.to(List).sortBy(_(0).s).map: (name, prototype) =>
                  val kind = prototype.parameters.lay(prophesy.Completion.Kind.Term): _ =>
                    prophesy.Completion.Kind.Method

                  val signature = prototype.parameters.lay(prototype.result.text): parameters =>
                    t"(${parameters.map(_.text).join(t", ")}): ${prototype.result.text}"

                  prophesy.Completion(name, kind, Syntax.Symbolic(signature))

            case _ =>
              Nil

  // A Scala value with an `Interoperable` instance converts into a `Foreign` literal carrying the
  // value verbatim; the foreign type is the instance's `Topic`.
  given converter: [value, ecosystem <: Ecosystem]
  =>  ( interoperable: value is Interoperable in ecosystem )
  =>  Conversion[value, Foreign of interoperable.Topic from ecosystem] =
    instance =>
      val literal = Expression.Literal(instance)
      Foreign.make(literal).asInstanceOf[Foreign of interoperable.Topic from ecosystem]

trait Foreign extends Dynamic, Topical, Original:
  def expr: Foreign.Expression

  transparent inline def selectDynamic(field: String): Foreign =
    ${Xenophile.select('this, 'field)}

  // Indexes into an array-typed foreign value (a `sequence`/`FrozenArray`/`Array`/`list`),
  // returning a `Foreign` of the element type. This is a real `apply` (taking precedence over
  // `Dynamic`), so `array(0)` resolves here; the macro rejects indexing a non-array foreign type.
  transparent inline def apply(index: Int): Foreign = ${Xenophile.index('this, 'index)}

  // Arguments are typed `Foreign from Origin` — i.e. a foreign value from this receiver's own
  // source language. A Scala value with an `Interoperable` instance for that language is converted
  // to a `Foreign` literal at the call site by the `converter` `Conversion` above (pinning the
  // ecosystem to `Origin` is what lets the conversion infer its type parameters); the macro then
  // checks each argument's foreign type against the declared parameter type.
  transparent inline def applyDynamic(field: String)(inline arguments: (Foreign from Origin)*)
  :   Foreign =

    ${Xenophile.applied('this, 'field, 'arguments)}
