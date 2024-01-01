/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import wisteria.examples.*

import probably.*
import rudiments.*
import gossamer.*
import eucalyptus.*

given Log(Everything |-> Stdout)

import java.time.LocalDate

import unsafeExceptions.canThrowAny

type ShowStr = [X] =>> AsString[Text, X ]

sealed trait Tree[+T] derives Eq
object Tree:
  given [T: [X] =>> AsString[Text, X]] : AsString[Text, Tree[T]] = AsString.derived

case class Leaf[+L](value: L) extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Path[+A]
case class Destination[+A](value: A) extends Path[A]
case class Crossroad[+A](left: Path[A], right: Path[A]) extends Path[A]
case class OffRoad[+A](path: Option[Path[A]]) extends Path[A]

sealed trait Entity

case class Company(name: Text) extends Entity
case class Person(name: Text, age: Int) extends Entity
case class Address(line1: Text, occupant: Person)

class Length(val value: Int) extends AnyVal

case class FruitBasket(fruits: Fruit*)
case class Lunchbox(fruit: Fruit, drink: Text)
case class Fruit(name: Text)

object Fruit:
  given showFruit: AsString[Text, Fruit] = (f: Fruit) => f.name

case class Item(name: Text, quantity: Int = 1, price: Int)

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Orange extends Color
case object Pink extends Color

case class MyAnnotation(order: Int) extends StaticAnnotation
case class MyTypeAnnotation(order: Int) extends StaticAnnotation

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
  @MyAnnotation(1) p1: Text @MyTypeAnnotation(0),
  @MyAnnotation(2) p2: Int @MyTypeAnnotation(1)
) extends AttributeParent @MyTypeAnnotation(2)

case class `%%`(`/`: Int, `#`: Text)

case class Param(a: Text, b: Text)
case class TestEntry(param: Param)
object TestEntry {
  def apply(): TestEntry = TestEntry(Param(t"", t""))

  def apply(a: Text)(using b: Int): TestEntry = TestEntry(Param(a, Showable(b).show))

  def apply(a: Text, b: Text): TestEntry = TestEntry(Param(a, b))
}

sealed trait Politician[+S]
case class Accountable[+S](slogan: S) extends Politician[S]
case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L) extends Politician[S]

sealed trait Box[+A]
case class SimpleBox[+A](value: A) extends Box[A]
case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]

case class Account(id: Text, emails: Text*)

case class Portfolio(companies: Company*)

case class Recursive(children: Seq[Recursive])
object Recursive {
  given showRecursive: AsString[Text, Recursive] = AsString.derived[Recursive]
}

// This tests compilation.
// class GenericCsv[A: Csv]
// object ParamCsv extends GenericCsv[Param]


class NotDerivable

case class NoDefault(value: Boolean)

final case class ServiceName1(value: Text) extends AnyVal
final case class ServiceName2(value: Text)

@MyAnnotation(0)
@SuppressWarnings(Array("deprecation"))
@JavaExampleAnnotation(description = "Some model")
case class MyDto(foo: Text, bar: Int)

@SerialVersionUID(42) case class Schedule(events: Seq[Event])
case class Event(date: LocalDate)

case class RPerson(age: Int, name: Text, children: Seq[RPerson])
object RPerson {
  given AsString[Text, RPerson] = AsString.derived
}
case class GPerson(children: Seq[RPerson])

case class ProtectedCons protected (name: Text)
object ProtectedCons {
  def apply(firstName: Text, familyName: Text): ProtectedCons =
    new ProtectedCons(t"$firstName $familyName")
  given show: AsString[Text, ProtectedCons] = AsString.derived
}

case class PrivateCons private (name: Text)
object PrivateCons {
  def apply(firstName: Text, familyName: Text): PrivateCons =
    new PrivateCons(t"$firstName $familyName")
  given show: AsString[Text, PrivateCons] = AsString.derived
}

// class PrivateValueClass private (val value: Int) extends AnyVal
// object PrivateValueClass {
//   def apply(l: Int) = new PrivateValueClass(l)
//   implicit val show: AsString[Text, PrivateValueClass] = AsString.derived
// }

case class KArray(value: List[KArray]) derives Eq
case class Wrapper(v: Option[KArray])

case class VeryLong(
  p1: Text,
  p2: Text,
  p3: Text,
  p4: Text,
  p5: Text,
  p6: Text,
  p7: Text,
  p8: Text,
  p9: Text,
  p10: Text,
  p11: Text,
  p12: Text,
  p13: Text,
  p14: Text,
  p15: Text,
  p16: Text,
  p17: Text,
  p18: Text,
  p19: Text,
  p20: Text,
  p21: Text,
  p22: Text,
  p23: Text
)

case class Character(id: Character.Id)
object Character {
  trait Tag extends Any
  type Id = Long with Tag
}

case class AnotherCharacter(id: AnotherCharacter.Id)
object AnotherCharacter {
  trait Tag extends Any
  type Id = Long with Tag

  given idShow: AsString[Text, Id] = Showable(_).show
}

final case class Abc(
  private val a: Int,
  private val b: Long,
  c: Text
)

sealed trait Covariant[+A]
sealed trait Contravariant[-A]
sealed trait Exactly[A] extends Covariant[A] with Contravariant[A]

object Exactly {
  case object Any extends Exactly[Any]
  case class Custom[A](value: A) extends Exactly[A]
  case object Int extends Exactly[Int]
  case object Nothing extends Exactly[Nothing]
  case object String extends Exactly[String]
}

case class ParamsWithDefault(a: Int = 3, b: Int = 4)
case class ParamsWithDefaultGeneric[A, B](a: A = t"A", b: B = t"B")

sealed trait Parent
trait BadChild extends Parent // escape hatch!
sealed trait GoodChild extends Parent
final case class Huey(height: Int) extends GoodChild
class Dewey(val height: Int) extends GoodChild
final case class Louie(height: Int) extends BadChild

object Tests extends Suite(t"Wisteria tests"):

  def run() =
    test(t"construct a AsShow product instance with alternative apply functions") {
      AsString.derived[TestEntry].asString(TestEntry(t"a", t"b"))
    }.check(_ == t"""TestEntry(param=Param(a=a,b=b))""")

    test(t"construct a AsString product instance") {
      AsString.derived[Person].asString(Person(t"John Smith", 34))
    }.check(_ == t"""Person(name=John Smith,age=34)""")

    test(t"construct a AsString coproduct instance") {
      AsString.derived[Person].asString(Person(t"John Smith", 34))
    }.check(_ == t"Person(name=John Smith,age=34)")

    test(t"construct a AsString instance for product with partially private fields") {
      AsString.derived[Abc].asString(Abc(12, 54, t"pm"))
    }.check(_ == t"Abc(a=12,b=54L,c=pm)")

    test(t"construct a AsString instance for a product with multiple default values") {
      AsString.derived[ParamsWithDefault].asString(ParamsWithDefault())
    }.check(_ == t"ParamsWithDefault(a=3,b=4)")

    test(t"local implicit beats Wisteria") {
      given showPerson: AsString[Text, Person] = _ => t"nobody"
      summon[AsString[Text, Address]].asString(Address(t"Home", Person(t"John Smith", 44)))
    }.check(_ == t"Address(line1=Home,occupant=nobody)")

    test(t"even low-priority implicit beats Wisteria for nested case") {
      summon[AsString[Text, Lunchbox]].asString(Lunchbox(Fruit(t"apple"), t"lemonade"))
    }.check(_ == t"Lunchbox(fruit=apple,drink=lemonade)")

    test(t"low-priority implicit beats Wisteria when not nested") {
      summon[AsString[Text, Fruit]].asString(Fruit(t"apple"))
    }.check(_ == t"apple")

    test(t"low-priority implicit beats Wisteria when chained") {
      summon[AsString[Text, FruitBasket]].asString(FruitBasket(Fruit(t"apple"), Fruit(t"banana")))
    }.check(_ == t"FruitBasket(fruits=[apple,banana])")

    test(t"typeclass implicit scope has lower priority than ADT implicit scope") {
      summon[AsString[Text, Fruit]].asString(Fruit(t"apple"))
    }.check(_ == t"apple")

    test(t"test equality false") {
      Eq.derived[Entity].equal(Person(t"John Smith", 34), Person(t"", 0))
    }.assert(!_)

    test(t"test equality true") {
      Eq.derived[Entity].equal(Person(t"John Smith", 34), Person(t"John Smith", 34))
    }.assert(_ == true)

    test(t"test branch equality true") {
      Eq.derived[Tree[Text]].equal(Branch(Leaf(t"one"), Leaf(t"two")), Branch(Leaf(t"one"), Leaf(t"two")))
    }.assert(_ == true)

    test(t"construct a default value") {
      HasDefault.derived[Entity].defaultValue
    }.assert(_ == Right(Company(t"")))

    test(t"serialize a Leaf") {
      implicitly[AsString[Text, Leaf[Text]]].asString(Leaf(t"testing"))
    }.check(_ == t"Leaf[Text](value=testing)")

    test(t"serialize case object") {
      summon[AsString[Text, Red.type]].asString(Red)
    }.check(_ == t"Red()")

    test(t"serialize self recursive type") {
      summon[AsString[Text, GPerson]].asString(GPerson(Nil))
    }.check(_ == t"GPerson(children=[])")

    test(t"serialize case object as a sealed trait") {
      summon[AsString[Text, Color]].asString(Blue)
    }.check(_ == t"Blue()")

    test(t"serialize case class with protected constructor") {
      ProtectedCons.show.asString(ProtectedCons(t"dada", t"phil"))
    }.check(_ == t"ProtectedCons(name=dada phil)")

    test(t"serialize case class with accessible private constructor") {
      PrivateCons.show.asString(PrivateCons(t"dada", t"phil"))
    }.check(_ == t"PrivateCons(name=dada phil)")

    test(t"read-only typeclass can serialize case class with inaccessible private constructor") {
      summon[Print[PrivateCons]].print(PrivateCons(t"dada", t"phil")): Text
    }.check(_ == t"PrivateCons(dada phil)")

    test(t"read-only typeclass can serialize case class with protected constructor") {
      summon[Print[ProtectedCons]].print(ProtectedCons(t"dada", t"phil")): Text
    }.check(_ == t"ProtectedCons(dada phil)")

    // test(t"decode a company") {
    //   Decoder.derived[Company].decode(t"""Company(name=Acme Inc)""")
    // }.assert(_ == Company(t"Acme Inc"))

    // test(t"decode a Person as an Entity") {
    //   summon[Decoder[Entity]].decode(t"""tests.Person(name=John Smith,age=32)""")
    // }.assert(_ == Person(t"John Smith", 32))

    // test(t"decode a nested product") {
    //   summon[Decoder[Address]].decode(
    //     t"""Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
    //   )
    // }.assert(_ == Address(t"53 High Street", Person(t"Richard Jones", 44)))

    test(t"typenames and labels are not encoded") {
      summon[AsString[Text, `%%`]].asString(`%%`(1, t"two"))
    }.check(_ == t"%%(/=1,#=two)")

    val tupleDerivation = summon[AsString[Text, (Int, Text)]]

    test(t"serialize a tuple") {
      tupleDerivation.asString((42, t"Hello World"))
    }.check(_ == t"Tuple2[Int,Text](_1=42,_2=Hello World)")

    // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[Text, _]
    test(t"show a Politician with covariant lobby") {
      AsString.derived[Politician[Text]].asString(Corrupt(t"wall", Seq(Company(t"Alice Inc"))))
    }.check(_ == t"Corrupt[Text,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])")

    // test(t"patch a Person via a Patcher[Entity]") {
    //  val person = Person(t"Bob", 42)
    //  summon[Patcher[Entity]].patch(person, Seq(null, 21))
    // }.assert(_ == Person(t"Bob", 21))

    test(t"show an Account") {
      AsString.derived[Account].asString(Account(t"john_doe", t"john.doe@yahoo.com", t"john.doe@gmail.com"))
    }.check(_ == t"Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])")

    test(t"construct a default Account") {
      HasDefault.derived[Account].defaultValue
    }.assert(_ == Right(Account(t"")))

    test(t"construct a failed NoDefault") {
      HasDefault.derived[NoDefault].defaultValue
    }.assert(_ == Left(t"truth is a lie"))

    test(t"show a Portfolio of Companies") {
      AsString.derived[Portfolio].asString(Portfolio(Company(t"Alice Inc"), Company(t"Bob & Co")))
    }.check(_ == t"Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])")

    // test(t"show a List[Int]") {
    //   given [T: [X] =>> AsString[Text, X]] : AsString[Text, List[T]] = AsString.derived

    //   AsString.derived[List[Int]].asString(List(1, 2, 3))
    // }.assert(_ == t"::[Int](head=1,next$access$1=::[Int](head=2,next$access$1=::[Int](head=3,next$access$1=Nil())))")
    
    test(t"sealed trait typeName should be complete and unchanged") {
      TypeNameInfo.derived[Color].name
    }.check(_.full == "wisteria.Color")

    test(t"sealed trait subtypes should be ordered") {
      TypeNameInfo.derived[Color].subtypeNames.map(_.short)
    }.assert(_ == Seq(t"Red", t"Green", t"Blue", t"Orange", t"Pink"))

    test(t"show a recursive case class") {
      AsString.derived[Recursive].asString(Recursive(Seq(Recursive(Nil))))
    }.check(_ == t"Recursive(children=[Recursive(children=[])])")

    test(t"manually derive a recursive case class instance") {
      Recursive.showRecursive.asString(Recursive(Seq(Recursive(Nil))))
    }.check(_ == t"Recursive(children=[Recursive(children=[])])")

    test(t"show underivable type with fallback") {
      summon[TypeNameInfo[NotDerivable]].name
    }.assert(_ == TypeInfo("", "Unknown Type", Seq.empty))

    test(t"equality of Wrapper") {
      Eq.derived[Wrapper].equal(Wrapper(Some(KArray(KArray(Nil) :: Nil))), Wrapper(Some(KArray(KArray(Nil) :: KArray(Nil) :: Nil))))
    }.assert(!_)

    test(t"very long") {
      val vl =
        VeryLong(t"p1", t"p2", t"p3", t"p4", t"p5", t"p6", t"p7", t"p8", t"p9", t"p10", t"p11", t"p12", t"p13", t"p14", t"p15",
            t"p16", t"p17", t"p18", t"p19", t"p20", t"p21", t"p22", t"p23")
      Eq.derived[VeryLong].equal(vl, vl)
    }.assert(_ == true)
