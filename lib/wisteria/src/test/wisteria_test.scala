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
package wisteria

import soundness.*

object Tests extends Suite(m"Wisteria tests"):

  // ===== Typeclass: Presentation =====

  trait Presentation[value]:
    def present(value: value): Text

  object Presentation extends Derivation[Presentation]:
    given Presentation[Text] = identity(_)
    given Presentation[Double] = _.toString.tt
    given Presentation[Boolean] = boolean => if boolean then t"yes" else t"no"
    given Presentation[Int] = _.toString.tt

    inline def conjunction[derivation <: Product: ProductReflection]: Presentation[derivation] = value =>
      inline if singleton then typeName else
        val prefix = inline if tuple then t"" else typeName
        fields(value):
          [field] => field => s"$index:$label=${contextual.present(field)}".tt
        .mkString((prefix.s+"("), ", ", ")").tt

    inline def disjunction[derivation: SumReflection]: Presentation[derivation] = value =>
      variant(value):
        [variant <: derivation] =>
          variant => (typeName.s+"."+contextual.present(variant)).tt

  extension [value](value: value)
    def present(using presentation: Presentation[value]): Text = presentation.present(value)

  // ===== Typeclass: Readable =====

  trait Readable[value]:
    def read(text: Text): value

  object Readable extends Derivation[Readable]:
    given text: Readable[Text] = identity(_)
    given int: Readable[Int] = _.s.toInt
    given boolean: Readable[Boolean] = _ == t"yes"

    inline def conjunction[derivation <: Product: ProductReflection]: Readable[derivation] = text =>
      text.s.split(",").nn.to(List).map(_.nn).pipe:
        array =>
          build:
            [field] =>
              readable =>
                if index < array.length then readable.read(array(index).tt) else default.or(???)

    inline def disjunction[derivation: SumReflection]: Readable[derivation] = text =>
      text.s.split(":").nn.to(List).map(_.nn.tt).absolve match
        case List(variant, text2) => delegate(variant):
          [variant <: derivation] =>
            context => context.read(text2)

  extension (text: Text)
    def read[value](using readable: Readable[value]): value = readable.read(text)

  // ===== Typeclass: Eq =====

  trait Eq[value]:
    def equal(left: value, right: value): Boolean

  object Eq extends Derivation[Eq]:
    given int: Eq[Int] = _ == _
    given text: Eq[Text] = _.s.toLowerCase == _.s.toLowerCase
    given boolean: Eq[Boolean] = _ == _
    given double: Eq[Double] = (left, right) => math.abs(left - right) < 0.1

    inline def conjunction[derivation <: Product: ProductReflection]: Eq[derivation] =
      (left, right) =>
        fields(left):
          [field] => leftValue => contextual.equal(leftValue, complement(right))
        . all { boolean => boolean }

    inline def disjunction[derivation: SumReflection]: Eq[derivation] =
      (left, right) =>
        variant(left):
          [variant <: derivation] => leftValue =>
            complement(right).lay(false): rightValue => contextual.equal(leftValue, rightValue)

  // ===== Typeclass: Parser =====

  trait Parser[value]:
    def parse(s: String): Option[value]

  object Parser extends ProductDerivation[Parser]:
    given Parser[Int]:
      def parse(s: String): Option[Int] = s.toIntOption

    given Parser[Boolean] with
      def parse(s: String): Option[Boolean] = s.toBooleanOption

    inline def conjunction[derivation <: Product: ProductReflection]: Parser[derivation] = input =>
      IArray.from(input.split(',')).pipe: inputArr =>
        construct[Option](
          [in, out] => _.flatMap,
          [monadic] => Some(_),
          [field] => context =>
            if index < inputArr.length then context.parse(inputArr(index))
            else None
        )

  // ===== Typeclass: Show =====

  trait Show[-T]:
    def show(value: T): String

  object Show extends Derivation[Show]:
    inline def conjunction[derivation <: Product: ProductReflection]: Show[derivation] = value =>
      typeName.s

    inline def disjunction[derivation: SumReflection]: Show[derivation] = value =>
      inline if choice then
        variant(value):
          [variant <: derivation] =>
            arm => typeName.s+"."+contextual.show(arm)
      else
        compiletime.error("cannot derive Show for adt")

  // ===== Typeclass: Producer =====

  trait Producer[value]:
    def produce(s: String): Option[value]

  object Producer extends Derivation[Producer]:
    inline def conjunction[derivation <: Product: ProductReflection]: Producer[derivation] =
      compiletime.error("not a product choice")

    inline def disjunction[derivation: SumReflection]: Producer[derivation] = input =>
      inline if choice then Some(singleton(input)) else compiletime.error("not a choice")

  // ===== Typeclass: SumOnly =====

  trait SumOnly[Type]:
    def applyTo(value: Type): Unit

  object SumOnly extends SumDerivation[SumOnly]:
    given SumOnly[SumOnlyEnum.Alpha] = alpha => ()
    given SumOnly[SumOnlyEnum.Beta] = beta => ()

    inline def disjunction[derivation: SumReflection]: SumOnly[derivation] =
      value => variant(value):
        [variant <: derivation] => value =>
          contextual.applyTo(value)

  // ===== Typeclass: Labels — exercises `contexts` (product) and `variantLabels` (sum) =====

  trait Labels[value]:
    def labels: List[Text]

  object Labels extends Derivation[Labels]:
    class Impl[derivation](val labels: List[Text]) extends Labels[derivation]

    inline def conjunction[derivation <: Product](using reflection: ProductReflection[derivation])
    :   Labels[derivation] =
      Impl(scala.compiletime.constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt))

    inline def disjunction[derivation](using reflection: SumReflection[derivation])
    :   Labels[derivation] =
      Impl(scala.compiletime.constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt))

  // ===== Data types =====

  enum Month:
    case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

  sealed trait Temporal
  case class Date(day: Int, month: Month, year: Int) extends Temporal
  case class Time(hour: Int, minute: Int, second: Int) extends Temporal

  enum Tree derives Presentation, Eq:
    case Leaf
    case Branch(value: Int, left: Tree, right: Tree)

  case class Person(name: Text = t"noone", age: Int = 100, male: Boolean = true)
  case class User(person: Person, email: Text = t"nobody@nowhere.com")

  enum SumOnlyEnum:
    case Alpha(n: Int)
    case Beta(n: String)

  case class ParserTestCaseClass(intValue: Int, booleanValue: Boolean)

  enum Adt:
    case First
    case Second(a: Boolean)

  case class Empty()
  case class Wrap(n: Int)

  case object Solo

  sealed trait Shape
  case class Circle(radius: Int) extends Shape
  case class Square(side: Int) extends Shape

  // ===== Tests =====

  def run(): Unit =
    given Tactic[VariantError] = strategies.throwUnsafely

    suite(m"Product derivation"):
      test(m"Parse a product via construct"):
        Parser.derived[ParserTestCaseClass].parse("42,true")
      . assert(_ == Some(ParserTestCaseClass(42, true)))

      test(m"Failed parse returns None via construct"):
        Parser.derived[ParserTestCaseClass].parse("notanint,true")
      . assert(_ == None)

      test(m"Present a product with default arguments"):
        Person().present
      . assert(_ == t"Person(0:name=noone, 1:age=100, 2:male=yes)")

      test(m"Present a nested product"):
        User(person = Person()).present
      . assert(_ == t"User(0:person=Person(0:name=noone, 1:age=100, 2:male=yes), 1:email=nobody@nowhere.com)")

      test(m"Present an empty product"):
        Empty().present
      . assert(_ == t"Empty()")

      test(m"Present a single-field product"):
        Wrap(7).present
      . assert(_ == t"Wrap(0:n=7)")

      test(m"Present a tuple"):
        val pair: (Int, Text) = (5, t"hi")
        pair.present
      . assert(_ == t"(0:_1=5, 1:_2=hi)")

      test(m"Read a product by comma-separated text"):
        t"alice,32,yes".read[Person]
      . assert(_ == Person(t"alice", 32, true))

      test(m"Read a product falling back to defaults for missing tail"):
        t"alice".read[Person]
      . assert(_ == Person(t"alice"))

      test(m"Read a product partially with defaults"):
        t"alice,42".read[Person]
      . assert(_ == Person(t"alice", 42, true))

      test(m"Compare equal products"):
        Eq.derived[Person].equal(Person(), Person())
      . assert(_ == true)

      test(m"Compare unequal products"):
        Eq.derived[Person].equal(Person(t"alice"), Person(t"bob"))
      . assert(_ == false)

      test(m"Equality is case-insensitive (per Eq[Text] given)"):
        Eq.derived[Person].equal(Person(t"ALICE"), Person(t"alice"))
      . assert(_ == true)

      test(m"Equality of two empty products"):
        Eq.derived[Empty].equal(Empty(), Empty())
      . assert(_ == true)

      test(m"Equality of single-field products"):
        Eq.derived[Wrap].equal(Wrap(7), Wrap(7))
      . assert(_ == true)

      test(m"Equality of single-field products differs"):
        Eq.derived[Wrap].equal(Wrap(7), Wrap(8))
      . assert(_ == false)

    suite(m"Sum derivation"):
      test(m"Present a singleton variant"):
        val tree: Tree = Tree.Leaf
        tree.present
      . assert(_ == t"Tree.Leaf")

      test(m"Present a recursive product variant"):
        val tree: Tree = Tree.Branch(1, Tree.Leaf, Tree.Leaf)
        tree.present
      . assert(_ == t"Tree.Branch(0:value=1, 1:left=Tree.Leaf, 2:right=Tree.Leaf)")

      test(m"Present a deeply-nested recursive value"):
        val tree: Tree = Tree.Branch(1, Tree.Branch(2, Tree.Leaf, Tree.Leaf), Tree.Leaf)
        tree.present
      . assert(_ == t"Tree.Branch(0:value=1, 1:left=Tree.Branch(0:value=2, 1:left=Tree.Leaf, 2:right=Tree.Leaf), 2:right=Tree.Leaf)")

      test(m"Read a product with multiple Int fields"):
        Readable.derived[Time].read(t"1,30,45")
      . assert(_ == Time(1, 30, 45))

      test(m"Equal sum variants compare equal"):
        val left: Shape = Circle(3)
        val right: Shape = Circle(3)
        Eq.derived[Shape].equal(left, right)
      . assert(_ == true)

      test(m"Different sum variants compare unequal"):
        val left: Shape = Circle(3)
        val right: Shape = Square(3)
        Eq.derived[Shape].equal(left, right)
      . assert(_ == false)

      test(m"Equal recursive trees compare equal"):
        val left: Tree = Tree.Branch(1, Tree.Leaf, Tree.Leaf)
        val right: Tree = Tree.Branch(1, Tree.Leaf, Tree.Leaf)
        Eq.derived[Tree].equal(left, right)
      . assert(_ == true)

      test(m"Different recursive trees compare unequal"):
        val left: Tree = Tree.Branch(1, Tree.Leaf, Tree.Leaf)
        val right: Tree = Tree.Branch(2, Tree.Leaf, Tree.Leaf)
        Eq.derived[Tree].equal(left, right)
      . assert(_ == false)

      test(m"Tree.Leaf and Tree.Branch are unequal"):
        val left: Tree = Tree.Leaf
        val right: Tree = Tree.Branch(1, Tree.Leaf, Tree.Leaf)
        Eq.derived[Tree].equal(left, right)
      . assert(_ == false)

      test(m"VariantError is raised for unknown variant tag"):
        sealed trait Wrapped
        case class WrappedInt(value: Int) extends Wrapped
        case class WrappedBool(value: Boolean) extends Wrapped

        try
          Readable.derived[Wrapped].read(t"Unknown:42")
          t""
        catch case error: VariantError => error.inputLabel
      . assert(_ == t"Unknown")

      test(m"VariantError reports the parent sum type"):
        sealed trait Wrapped
        case class WrappedInt(value: Int) extends Wrapped
        case class WrappedBool(value: Boolean) extends Wrapped

        try
          Readable.derived[Wrapped].read(t"Unknown:42")
          t""
        catch case error: VariantError => error.sum
      . assert(_ == t"Wrapped")

      test(m"VariantError lists the valid variants"):
        sealed trait Wrapped
        case class WrappedInt(value: Int) extends Wrapped
        case class WrappedBool(value: Boolean) extends Wrapped

        try
          Readable.derived[Wrapped].read(t"Unknown:42")
          List.empty[Text]
        catch case error: VariantError => error.validVariants
      . assert(_ == List(t"WrappedInt", t"WrappedBool"))

      test(m"SumOnly applies to a manually-given variant"):
        val sumOnly = summon[SumOnly[SumOnlyEnum]]
        sumOnly.applyTo(SumOnlyEnum.Alpha(42))
        sumOnly.applyTo(SumOnlyEnum.Beta("hi"))
      . assert()

    suite(m"Field labels and variant labels"):
      test(m"Labels for a multi-field product"):
        Labels.derived[Person].labels
      . assert(_ == List(t"name", t"age", t"male"))

      test(m"Labels for a nested product"):
        Labels.derived[User].labels
      . assert(_ == List(t"person", t"email"))

      test(m"Labels for an empty product"):
        Labels.derived[Empty].labels
      . assert(_ == Nil)

      test(m"Labels for a single-field product"):
        Labels.derived[Wrap].labels
      . assert(_ == List(t"n"))

      test(m"Labels for a sum type"):
        Labels.derived[Temporal].labels
      . assert(_ == List(t"Date", t"Time"))

      test(m"Labels for a sealed trait hierarchy"):
        Labels.derived[Shape].labels
      . assert(_ == List(t"Circle", t"Square"))

    suite(m"Arithmetic derivation"):
      case class Pair(name: Text, age: Int)
      case class Pt(x: Int, y: Int)

      test(m"Addable composes products field-wise"):
        import arithmetic.addable
        Pair(t"foo", 10) + Pair(t"bar", 15)
      . assert(_ == Pair(t"foobar", 25))

      test(m"Subtractable composes products field-wise"):
        import arithmetic.subtractable
        Pt(10, 7) - Pt(3, 4)
      . assert(_ == Pt(7, 3))

      test(m"Multiplicable composes products field-wise"):
        import arithmetic.multiplicable
        Pt(3, 4) * Pt(5, 6)
      . assert(_ == Pt(15, 24))

      test(m"Divisible composes products field-wise"):
        import arithmetic.divisible
        Pt(20, 18) / Pt(5, 3)
      . assert(_ == Pt(4, 6))

    suite(m"Compile-time errors"):
      test(m"Show fails on a non-choice ADT"):
        demilitarize:
          Show.derived[Adt]
      . assert(_.nonEmpty)

      test(m"Producer fails on a non-choice ADT"):
        demilitarize:
          Producer.derived[Adt]
      . assert(_.nonEmpty)

      test(m"Producer fails on a regular product"):
        demilitarize:
          Producer.derived[ParserTestCaseClass]
      . assert(_.nonEmpty)

      test(m"Derivation fails when no Mirror is available"):
        demilitarize:
          trait NotADataType
          Eq.derived[NotADataType]
      . assert(_.nonEmpty)

      test(m"Derivation fails when a field's typeclass is missing"):
        demilitarize:
          case class HasUnknown(unknown: java.io.File)
          Eq.derived[HasUnknown]
      . assert(_.nonEmpty)

      test(m"Derivation fails on an unsupported parameter type"):
        demilitarize:
          case class Bag(items: List[java.io.File])
          Readable.derived[Bag]
      . assert(_.nonEmpty)
