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
package stenography

import scala.{caps, Enumeration}
import scala.language.experimental.pureFunctions

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import probably.*
import spectacular.*
import symbolism.*

// Capture sets are written in terms of capabilities, so the tests which render them need some
// capabilities to name.
trait Sensor extends caps.ExclusiveCapability
trait Reader extends caps.SharedCapability, caps.Classifier

object Tests extends Suite(m"Stenography Tests"):
  def run(): Unit =
    test(m"Decode a term"):
      Designator(t"immutable.List")
    . assert(_ == Designator.Term(Designator.Top("immutable"), "List"))

    test(m"Decode a type of a term"):
      Designator(t"immutable.List#Value")
    . assert(_ == Designator.Term(Designator.Type(Designator.Top("immutable"), "List"), "Value"))

    test(m"Decode a term of a term"):
      Designator(t"immutable.List.Value")
    . assert(_ == Designator.Term(Designator.Term(Designator.Top("immutable"), "List"), "Value"))

    test(m"Show `Int`"):
      Syntax.name[Int]
    . assert(_ == t"Int")

    test(m"Show `Int & String`"):
      Syntax.name[Int & String]
    . assert(_ == t"Int & String")

    test(m"Show `Int | String`"):
      Syntax.name[Int | String]
    . assert(_ == t"Int | String")

    test(m"Show `Int | String & Double`"):
      Syntax.name[Int | String & Double]
    . assert(_ == t"Int | String & Double")

    test(m"Show `Double & Int | String`"):
      Syntax.name[Double & Int | String]
    . assert(_ == t"Double & Int | String")

    test(m"Show `Double & (Int | String)`"):
      Syntax.name[Double & (Int | String)]
    . assert(_ == t"Double & (Int | String)")

    test(m"Show `Double & Int & String`"):
      Syntax.name[Double & Int & String]
    . assert(_ == t"Double & Int & String")

    test(m"Show `List[Int]`"):
      Syntax.name[scala.collection.immutable.List[Int]]
    . assert(_ == t"collection.immutable.List[Int]")

    test(m"Show constant String type"):
      Syntax.name["hello"]
    . assert(_ == t""""hello"""")

    test(m"Show constant Int type"):
      Syntax.name[42]
    . assert(_ == t"42")

    test(m"Show constant Long type"):
      Syntax.name[42L]
    . assert(_ == t"42L")

    test(m"Show constant Double type"):
      Syntax.name[4.2]
    . assert(_ == t"4.2")

    test(m"Show constant Float type"):
      Syntax.name[4.2F]
    . assert(_ == t"4.2F")

    test(m"Show constant Boolean true type"):
      Syntax.name[true]
    . assert(_ == t"true")

    test(m"Show constant Boolean false type"):
      Syntax.name[false]
    . assert(_ == t"false")

    test(m"Show constant Char type"):
      Syntax.name['a']
    . assert(_ == t"'a'")

    test(m"Show wildcard type argument"):
      Syntax.name[scala.collection.immutable.List[?]]
    . assert(_ == t"collection.immutable.List[?]")

    test(m"Show upper-bounded wildcard"):
      Syntax.name[scala.collection.immutable.List[? <: AnyRef]]
    . assert(_ == t"collection.immutable.List[? <: AnyRef]")

    test(m"Show lower-bounded wildcard"):
      Syntax.name[scala.collection.immutable.List[? >: String]]
    . assert(_ == t"collection.immutable.List[? >: String]")

    test(m"Show wildcard with both bounds"):
      Syntax.name[scala.collection.immutable.List[? >: String <: AnyRef]]
    . assert(_ == t"collection.immutable.List[? >: String <: AnyRef]")

    test(m"Show into-annotated type"):
      Syntax.name[Int @scala.annotation.internal.$into]
    . assert(_ == t"into Int")

    test(m"Show match type"):
      Syntax.name[[scrutinee] =>> scrutinee match
        case String => Char
        case Int    => Boolean]
    . assert(_ == t"[scrutinee] =>> scrutinee match { case String => Char; case Int => Boolean }")

    test(m"Show match type with parametric case"):
      Syntax.name[[scrutinee] =>> scrutinee match
        case List[item] => item]
    . assert(_ == t"[scrutinee] =>> scrutinee match { case List[item] => item }")

    test(m"Show singleton type"):
      Syntax.name[None.type]
    . assert(_ == t"None.type")

    test(m"Show name with package"):
      Syntax.name[turbulence.Streamable]
    . assert(_ == t"turbulence.Streamable")

    test(m"Show typeclass type"):
      Syntax.name[Addable by Int to Double]
    . assert(_ == t"Addable by Int to Double")

    test(m"Show infix type"):
      Syntax.name[Text is Showable]
    . assert(_ == t"Text is Showable")

    test(m"Show function"):
      Syntax.name[Text => Int]
    . assert(_ == t"Text => Int")

    test(m"Show context function"):
      Syntax.name[Text ?=> Int]
    . assert(_ == t"Text ?=> Int")

    test(m"Show two-parameter function"):
      Syntax.name[(Text, Double) => Int]
    . assert(_ == t"(Text, Double) => Int")

    test(m"Show two-parameter context function"):
      Syntax.name[(Text, Double) ?=> Int]
    . assert(_ == t"(Text, Double) ?=> Int")

    test(m"Show named function"):
      Syntax.name[(text: Text) => Int]
    . assert(_ == t"(text: Text) => Int")

    test(m"Show named context function"):
      Syntax.name[(text: Text) ?=> Int]
    . assert(_ == t"(text: Text) ?=> Int")

    test(m"Show polymorphic lambda"):
      Syntax.name[[field] => (x: field) -> Int]
    . assert(_ == t"[field] => (x: field) -> Int")

    test(m"Show polymorphic lambda with upper bound"):
      Syntax.name[[field <: String] => (x: field) -> Int]
    . assert(_ == t"[field <: String] => (x: field) -> Int")

    test(m"Show polymorphic lambda with lower bound"):
      Syntax.name[[field >: String] => (x: field) -> Int]
    . assert(_ == t"[field >: String] => (x: field) -> Int")

    test(m"Show polymorphic lambda with upper and lower bound"):
      Syntax.name[[field >: String <: AnyRef] => (x: field) -> Int]
    . assert(_ == t"[field >: String <: AnyRef] => (x: field) -> Int")

    test(m"By-name type"):
      Syntax.name[(=> Int) => Double]
    . assert(_ == t"(=> Int) => Double")

    test(m"Projection type"):
      Syntax.name[Enumeration#Value]
    . assert(_ == t"Enumeration#Value")

    test(m"Refined type"):
      Syntax.name[Addable { type Q = Int }]
    . assert(_ == t"Addable { type Q = Int }")

    test(m"Refined type with def foo(x: Int): String"):
      Syntax.name[Addable { def foo(x: Int): String }]
    . assert(_ == t"Addable { def foo(x: Int): String }")

    test(m"Refined type with def foo: Int => String"):
      Syntax.name[Addable { def foo: Int => String }]
    . assert(_ == t"Addable { def foo: Int => String }")

    test(m"Refined type with def foo: (x: Int) => String"):
      Syntax.name[Addable { def foo: (x: Int) => String }]
    . assert(_ == t"Addable { def foo: (x: Int) => String }")

    test(m"Refined types"):
      Syntax.name[Addable { type Q = Int; type Other = String}]
    . assert(_ == t"Addable { type Q = Int; type Other = String }")

    // Written as refinements rather than as `Addable by Int`, because the compiler keeps the
    // alias when it is written, and it is precisely the dealiased form these cover.
    test(m"Prefer an infix alias over the refinement it expands to"):
      Syntax.name[Addable { type Operand = Int }]
    . assert(_ == t"Addable by Int")

    test(m"Prefer `in` over a `Form` refinement"):
      Syntax.name[Addable { type Form = Int }]
    . assert(_ == t"Addable in Int")

    test(m"Prefer infix aliases for several refined members"):
      Syntax.name[Addable { type Operand = Int; type Result = Double }]
    . assert(_ == t"Addable by Int to Double")

    test(m"Prefer an infix alias alongside a member which has none"):
      Syntax.name[Addable { type Operand = Int; type Q = String }]
    . assert(_ == t"Addable by Int { type Q = String }")

    // The parentheses are redundant — an alphabetic infix type operator binds tighter than the
    // function arrow — but `Function` parenthesises any parameter whose precedence it exceeds,
    // and alphabetic operators score lowest. That applies equally to the `is` this generalises,
    // so it is left alone here.
    test(m"Prefer an infix alias within a function type"):
      Syntax.name[(Addable { type Operand = Int }) => String]
    . assert(_ == t"(Addable by Int) => String")

    // `Addable by (? <: Int)` is not what `by` expands to, so a bounded member has to stay a
    // refinement rather than acquire a wildcard operand.
    test(m"Leave a refinement which bounds, rather than aliases, its member"):
      Syntax.name[Addable { type Operand <: Int }]
    . assert(_ == t"Addable { type Operand = ? <: Int }")

    // `Q` has no infix alias refining it, so this must not pick up a neighbour's operator.
    test(m"Leave a refinement whose member has no infix alias"):
      Syntax.name[Addable { type Q = Int }]
    . assert(_ == t"Addable { type Q = Int }")

    test(m"Refined higher-kinded type member"):
      Syntax.name[Addable { type Hkt[Bar] = Option[Bar] }]
    . assert(_ == t"Addable { type Hkt = [Bar] =>> Option[Bar] }")

    // test(m"Refined type member with selection"):
    //   Syntax.name[Addable { val foo: String; type Bar = foo.type }]
    // .assert(_ == t"Addable { val foo: String; type Bar = foo.type }")

    test(m"Refined higher-kinded type member 2"):
      Syntax.name[Addable { type Hkt[Bar <: String] = Option[Bar] }]
    . assert(_ == t"Addable { type Hkt = [Bar <: String] =>> Option[Bar] }")

    // test(m"Refined higher-kinded type member 2"):
    //   Syntax.name[Addable { type Hkt[Coll[T] <: Iterable[T]] = Option[Coll[String]] }]
    // .assert(_ == t"Addable { type Hkt[Coll[T] <: Iterable[T]] = Option[Coll[String]] }")

    test(m"Projected refined type"):
      Syntax.name[(Addable { type Q = Int; type Other = String})#Q]
    . assert(_ == t"Int")

    test(m"Function type of infix types"):
      Syntax.name[Int is Addable by Double => String is Checkable against String]
    . assert(_ == t"Int is Addable by Double => String is Checkable against String")

    test(m"Context function type of infix types"):
      Syntax.name[Int is Addable by Double ?=> String is Checkable against String]
    . assert(_ == t"Int is Addable by Double ?=> String is Checkable against String")

    test(m"Named function type of infix types"):
      Syntax.name[(addable: Int is Addable by Double) ?=> String is Checkable against String]
    . assert(_ == t"(addable: Int is Addable by Double) ?=> String is Checkable against String")

    test(m"Tuple"):
      Syntax.name[(Int, String, Exception)]
    . assert(_ == t"(Int, String, Exception)")

    test(m"Named tuple"):
      Syntax.name[(x: Int, y: String, z: Exception)]
    . assert(_ == t"(x: Int, y: String, z: Exception)")

    test(m"Type lambda"):
      Syntax.name[[T] =>> Option[T]]
    . assert(_ == t"[T] =>> Option[T]")

    test(m"Pure function"):
      Syntax.name[Int -> String]
    . assert(_ == t"Int -> String")

    test(m"Pure function returning a function"):
      Syntax.name[Int -> Text -> String]
    . assert(_ == t"Int -> Text -> String")

    test(m"Pure function taking a function"):
      Syntax.name[(Int -> Text) -> String]
    . assert(_ == t"(Int -> Text) -> String")

    test(m"Pure by-name type"):
      Syntax.name[(-> Int) -> Double]
    . assert(_ == t"(-> Int) -> Double")

    test(m"Dependent function type"):
      Syntax.name[(e: Enumeration) => e.Value]
    . assert(_ == t"(e: Enumeration) => e.Value")

    test(m"Pure dependent function type"):
      Syntax.name[(e: Enumeration) -> e.Value]
    . assert(_ == t"(e: Enumeration) -> e.Value")

    captures(new Sensor {}, new Sensor {}, Nil)

  // The capabilities a capture set names have to be tracked references, so the tests which
  // render capture sets take them as parameters rather than referring to globals.
  def captures(alpha: Sensor, beta: Sensor, sensors: List[Sensor]): Unit =
    test(m"Capture set with one capability"):
      Syntax.name[Text^{alpha}]
    . assert(_ == t"Text^{alpha}")

    test(m"Capture set with two capabilities"):
      Syntax.name[Text^{alpha, beta}]
    . assert(_ == t"Text^{alpha, beta}")

    test(m"Universal capture set"):
      Syntax.name[Text^]
    . assert(_ == t"Text^")

    test(m"Empty capture set"):
      Syntax.name[Text^{}]
    . assert(_ == t"Text^{}")

    test(m"Capture set on an applied type"):
      Syntax.name[scala.collection.immutable.List[Text]^{alpha}]
    . assert(_ == t"collection.immutable.List[Text]^{alpha}")

    test(m"Capture set on a union type"):
      Syntax.name[(Int | Text)^{alpha}]
    . assert(_ == t"(Int | Text)^{alpha}")

    test(m"Function capturing one capability"):
      Syntax.name[Int ->{alpha} String]
    . assert(_ == t"Int ->{alpha} String")

    test(m"Function capturing two capabilities"):
      Syntax.name[Int ->{alpha, beta} String]
    . assert(_ == t"Int ->{alpha, beta} String")

    test(m"Context function capturing a capability"):
      Syntax.name[Int ?->{alpha} String]
    . assert(_ == t"Int ?->{alpha} String")

    test(m"Capture set of a capture-set type"):
      Syntax.name[caps.CapSet^{alpha, beta}]
    . assert(_ == t"caps.CapSet^{alpha, beta}")

    test(m"Restricted capability in a capture set"):
      Syntax.name[Text^{alpha.only[Reader]}]
    . assert(_ == t"Text^{alpha.only[Reader]}")

    test(m"Reach capability in a capture set"):
      Syntax.name[Text^{sensors*}]
    . assert(_ == t"Text^{sensors*}")

    test(m"Universal capability in a capture set"):
      Syntax.name[Text^{caps.any}]
    . assert(_ == t"Text^{any}")

    test(m"Capture-set type parameter"):
      Syntax.name[[cap^] =>> Text^{cap}]
    . assert(_ == t"[cap^] =>> Text^{cap}")

    test(m"Bounded capture-set type parameter"):
      Syntax.name[[cap^ >: {alpha}] =>> Text^{cap}]
    . assert(_ == t"[cap^ >: {alpha}] =>> Text^{cap}")
