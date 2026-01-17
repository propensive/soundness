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

      def modify[applicative[_]: Applicative](origin: Origin)
           (lambda: Operand => applicative[Target])
      : applicative[Result] =
          lambda(origin)



object Applicative:
  type Id[value] = value

  given identity: Id is Applicative:
    type Self = Id
    def pure[value](value: value): Self[value] = value
    def ap[left, right](lambda: Self[left => right], left: Self[left]): Self[right] = lambda(left)
    def map[left, right](left: Self[left], lambda: left => right): Self[right] = lambda(left)

trait Applicative:
  type Self[_]
  def pure[value](value: value): Self[value]
  def ap[left, right](lambda: Self[left => right], left: Self[left]): Self[right]
  def map[left, right](left: Self[left], lambda: left => right): Self[right]


trait Optic extends Typeclass, Dynamic:
  lens0 =>
    type Origin
    type Result
    type Operand
    type Target

    def modify[applicative[_]: Applicative](origin: Origin)(lambda: Operand => applicative[Target])
    : applicative[Result]

    def selectDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
    : Optic from Origin to Result by lens.Operand onto lens.Target =
        println("select dynamic")
        Composable.optics.composition(this, lens)

    def updateDynamic(name: Label)(using lens: name.type is Optic from Operand to Target)
         (value: lens.Target)
    : Origin => Result =
        origin =>
          println("apply dynamic")
          Composable.optics.composition(this, lens).modify[[any] =>> any](origin)(_ => value)

object Composable:

  given optics: [origin, result, operand, target, operand2, target2]
        => (Optic from origin to result by operand onto target) is Composable by
            (Optic from operand to target by operand2 onto target2) to
            (Optic from origin to result by operand2 onto target2) =
    (left, right) =>
      new Optic:
        println("new optic")
        type Origin = origin
        type Result = result
        type Operand = operand2
        type Target = target2

        def modify[applicative[_]: Applicative](origin: origin)
             (lambda: Operand => applicative[target2])
        : applicative[result] =
            println("modify in optic")
            left.modify[applicative](origin)(right.modify(_)(lambda))

  given lenses: [origin, target, target2]
        => (Lens from origin onto target) is Composable by
            (Lens from target onto target2) to
            (Lens from origin onto target2) =
    (left, right) =>
      new Lens:
        println("new lens")
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

  def modify[applicative[_]: Applicative](origin: Origin)(lambda: Operand => applicative[Target])
  : applicative[Origin] =

        applicative.map(lambda(apply(origin)), this(origin) = _)



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
  val company = Company(Person("John", List(Role("CEO", 1))), "Acme")

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

  given eachPersonRoles: (Optic from Person to Person by List[Role] onto Role) = new Optic:
    type Origin = Person
    type Target = Role
    type Result = Person
    type Operand = List[Role]

    def modify[applicative[_]: Applicative](person: Person)(lambda: List[Role] => applicative[Role]): applicative[Person] =
      applicative.map(lambda(person.roles), role => person.copy(roles = person.roles.map(_ => role)))


  // given headPersonRoles: (Optic from Person to Person by Optional[Role] onto Optional[Role]) = new Optic:
  //   type Origin = Person
  //   type Operand = Optional[Role]
  //   type Result = Person
  //   type Target = Optional[Role]
  //   def apply(person: Person): Optional[Role] = person.roles.prim
  //   def update(person: Person, value: Optional[Role]): Person = person.roles match
  //     case Nil          => person.copy(roles = List(value).compact)
  //     case head :: tail => person.copy(roles = List(value).compact ++ tail)

  println(company.lens(_.ceo = Person("John Doe", List(Role("CTO", 7)))))
  println(company.lens(_.ceo.name = "Jimmy"))
