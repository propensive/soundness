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
package xenophile

import scala.language.dynamics

import anticipation.*
import gossamer.*
import prepositional.*

object Foreign:
  // A foreign expression: a reference to a named foreign value, a member selection (recording the
  // `owner` foreign type it is selected from, which backends needing that type's layout — e.g. the
  // native evaluator — use, while self-describing backends like JSON ignore it), a function
  // application, or a literal operand.
  enum Expression:
    case Reference(name: Text)
    case Select(target: Expression, member: Text, owner: Text)
    case Apply(target: Expression, arguments: List[Expression])
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

  given converter: [value, ecosystem <: Ecosystem]
  =>  ( interoperable: value is Interoperable in ecosystem )
  =>  Conversion[value, Foreign of interoperable.Topic from ecosystem] =
    instance =>
      val literal = Expression.Literal(interoperable.operand(instance))
      Foreign.make(literal).asInstanceOf[Foreign of interoperable.Topic from ecosystem]

trait Foreign extends Dynamic, Topical, Original:
  def expr: Foreign.Expression

  transparent inline def selectDynamic(field: String): Foreign =
    ${Xenophile.select('this, 'field)}

  // Arguments are typed `Foreign from Origin` — i.e. a foreign value from this receiver's own
  // source language. A Scala value with an `Interoperable` instance for that language is converted
  // to a `Foreign` literal at the call site by the `converter` `Conversion` above (pinning the
  // ecosystem to `Origin` is what lets the conversion infer its type parameters); the macro then
  // checks each argument's foreign type against the declared parameter type.
  transparent inline def applyDynamic(field: String)(inline arguments: (Foreign from Origin)*)
  :   Foreign =

    ${Xenophile.applied('this, 'field, 'arguments)}

  inline def as[ScalaType]: ScalaType = ${Xenophile.convert[ScalaType]('this)}
