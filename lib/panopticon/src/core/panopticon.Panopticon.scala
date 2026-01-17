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
┃    Soundness, version 0.51.0.                                                                    ┃
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
package panopticon

import anticipation.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

import scala.quoted.*
import scala.compiletime.*

import language.dynamics
import scala.annotation.internal.preview

object Panopticon:
  private given realm: Realm = realm"panopticon"

object Optic:
  def identity[value]: Optic from value to value by value onto value =
    new Optic:
      type Origin = value
      type Result = value
      type Operand = value
      type Target = value

      def apply(origin: Origin): Operand = origin
      def update(origin: Origin, value: Target): Result = value

      def modify(origin: Origin)(lambda: Operand => Target): Result = lambda(origin)



trait Optic extends Typeclass, Dynamic:
  lens0 =>
    type Origin
    type Result
    type Operand
    type Target

    def modify(origin: Origin)(lambda: Operand => Target): Result

    def selectDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
    : Optic from Origin to Result by lens.Operand onto lens.Target =
        Composable.optics.composition(this, lens)

    def updateDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
         (value: lens.Target)
    : Origin => Result =
        origin =>
          Composable.optics.composition(this, lens).modify(origin)(_ => value)

    def update[result, traversal: Optic from Operand to Target onto result as optic]
         (traversal: traversal, value: result)
    : Origin => Result =
        origin =>
          Composable.optics.composition(this, optic).modify(origin)(_ => value)

    def applyDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
         [target, traversal: Optic from lens.Operand to lens.Target onto target as optic]
         (traversal: traversal)
    : Optic from Origin to Result by optic.Operand onto target =
        Composable.optics.composition(Composable.optics.composition(this, lens), optic)

object Composable:

  given optics: [origin, result, operand, target, operand2, target2]
        => (Optic from origin to result by operand onto target) is Composable by
            (Optic from operand to target by operand2 onto target2) to
            (Optic from origin to result by operand2 onto target2) =
    (left, right) =>
      new Optic:
        type Origin = origin
        type Result = result
        type Operand = operand2
        type Target = target2

        def modify(origin: origin)(lambda: Operand => target2): result =
          left.modify(origin)(right.modify(_)(lambda))

  given lenses: [origin, target, target2]
        => (Lens from origin onto target) is Composable by
            (Lens from target onto target2) to
            (Lens from origin onto target2) =
    (left, right) =>
      new Lens:
        type Origin = origin
        type Target = target2

        def apply(origin: Origin): Operand = right(left(origin))

        def update(origin: Origin, value: Target): Result =
          left(origin) = right(left(origin)) = value

trait Lens extends Optic:
  type Result = Origin
  type Operand = Target

  def apply(origin: Origin): Operand
  def update(origin: Origin, value: Operand): Origin

  def modify(origin: Origin)(lambda: Operand => Target): Origin =
    update(origin, lambda(apply(origin)))


trait Composable:
  type Self
  type Operand
  type Result

  def composition(left: Self, right: Operand): Result

  extension (left: Self) def compose(right: Operand): Result = composition(left, right)

trait Traversal:
  type Result[topic]
  def traverse[topic, target](result: Result[topic])(lambda: topic => target): Result[target]

object Each extends Traversal:
  type Result[topic] = List[topic]

  def traverse[topic, target](result: Result[topic])(lambda: topic => target): Result[target] =
    result.map(lambda)

object Head extends Traversal:
  type Result[topic] = Optional[topic]
  def traverse[topic, target](result: Result[topic])(lambda: topic => target): Result[target] =
    result.let(lambda)


extension [value](value: value)
  def lens[target](lambda: Optic from value to value by value onto value => value => target): target =
    lambda(Optic.identity)(value)

case class Company(ceo: Person, name: Text)
case class Person(name: Text, roles: List[Role])
case class Role(name: Text, count: Int)

@main
def test: Unit =

  given companyName: ("name" is Lens from Company onto Text) = new Lens:
    type Self = "name"
    type Origin = Company
    type Target = Text
    def apply(company: Company): Text = company.name
    def update(company: Company, value: Text): Company = company.copy(name = value)

  given personName: ("name" is Lens from Person onto Text) = new Lens:
    type Self = "name"
    type Origin = Person
    type Target = Text
    def apply(person: Person): Text = person.name
    def update(person: Person, value: Text): Person = person.copy(name = value)

  given roleName: ("name" is Lens from Role onto Text) = new Lens:
    type Self = "name"
    type Origin = Role
    type Target = Text
    def apply(role: Role): Text = role.name
    def update(role: Role, value: Text): Role = role.copy(name = value)

  given companyPerson: ("ceo" is Lens from Company onto Person) = new Lens:
    type Self = "ceo"
    type Origin = Company
    type Target = Person
    def apply(company: Company): Person = company.ceo
    def update(company: Company, value: Person): Company = company.copy(ceo = value)

  given personRoles: ("roles" is Lens from Person onto List[Role]) = new Lens:
    type Self = "roles"
    type Origin = Person
    type Target = List[Role]
    def apply(person: Person): List[Role] = person.roles
    def update(person: Person, value: List[Role]): Person = person.copy(roles = value)

  given list: [element]
              => Each.type is Optic from List[element] to List[element] by element onto element =
    new Optic:
      type Self = Each.type
      type Origin = List[element]
      type Target = element
      type Result = List[element]
      type Operand = element

      def modify(list: List[element])(lambda: element => element): List[element] =
        list.map(lambda)


  // given headPersonRoles: (Optic from Person to Person by Optional[Role] onto Optional[Role]) = new Optic:
  //   type Origin = Person
  //   type Operand = Optional[Role]
  //   type Result = Person
  //   type Target = Optional[Role]
  //   def apply(person: Person): Optional[Role] = person.roles.prim
  //   def update(person: Person, value: Optional[Role]): Person = person.roles match
  //     case Nil          => person.copy(roles = List(value).compact)
  //     case head :: tail => person.copy(roles = List(value).compact ++ tail)

  val company = Company(Person("John", List(Role("CEO", 1), Role("CFO", 2), Role("CIO", 3))), "Acme")
  println(company.lens(_.ceo = Person("John Doe", List(Role("CTO", 7)))))
  println(company.lens(_.ceo.name = "Jimmy"))
  println(company.lens(_.ceo.roles = Nil))
  println(company.lens(_.ceo.roles(Each).name = "Developer"))
