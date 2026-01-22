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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import language.experimental.pureFunctions

import anticipation.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import probably.*
import spectacular.*
import symbolism.*

import autopsies.contrastExpectations

object Tests extends Suite(m"Stenography Tests"):

  def run(): Unit =
    import anticipation.*
    test(m"Decode a term"):
      Typename(t"immutable.List")
    .assert(_ == Typename.Term(Typename.Top("immutable"), "List"))

    test(m"Decode a type of a term"):
      Typename(t"immutable.List#Value")
    .assert(_ == Typename.Term(Typename.Type(Typename.Top("immutable"), "List"), "Value"))

    test(m"Decode a term of a term"):
      Typename(t"immutable.List.Value")
    .assert(_ == Typename.Term(Typename.Term(Typename.Top("immutable"), "List"), "Value"))

    test(m"Show `Int`"):
      Syntax.name[Int]
    .assert(_ == t"Int")

    test(m"Show `Int & String`"):
      Syntax.name[Int & String]
    .assert(_ == t"Int & String")

    test(m"Show `Int | String`"):
      Syntax.name[Int | String]
    .assert(_ == t"Int | String")

    test(m"Show `Int | String & Double`"):
      Syntax.name[Int | String & Double]
    .assert(_ == t"Int | String & Double")

    test(m"Show `Double & Int | String`"):
      Syntax.name[Double & Int | String]
    .assert(_ == t"Double & Int | String")

    test(m"Show `Double & (Int | String)`"):
      Syntax.name[Double & (Int | String)]
    .assert(_ == t"Double & (Int | String)")

    test(m"Show `Double & Int & String`"):
      Syntax.name[Double & Int & String]
    .assert(_ == t"Double & Int & String")

    test(m"Show `List[Int]`"):
      Syntax.name[List[Int]]
    .assert(_ == t"collection.immutable.List[Int]")

    test(m"Show constant String type"):
      Syntax.name["hello"]
    .assert(_ == t""""hello"""")

    test(m"Show constant Int type"):
      Syntax.name[42]
    .assert(_ == t"42")

    test(m"Show constant Long type"):
      Syntax.name[42L]
    .assert(_ == t"42L")

    test(m"Show constant Double type"):
      Syntax.name[4.2]
    .assert(_ == t"4.2")

    test(m"Show constant Float type"):
      Syntax.name[4.2F]
    .assert(_ == t"4.2F")

    test(m"Show singleton type"):
      Syntax.name[None.type]
    .assert(_ == t"None.type")

    test(m"Show name with package"):
      Syntax.name[turbulence.Streamable]
    .assert(_ == t"turbulence.Streamable")

    test(m"Show typeclass type"):
      Syntax.name[Addable by Int to Double]
    .assert(_ == t"Addable by Int to Double")

    test(m"Show infix type"):
      Syntax.name[Text is Showable]
    .assert(_ == t"Text is Showable")

    test(m"Show function"):
      Syntax.name[Text => Int]
    .assert(_ == t"Text => Int")

    test(m"Show context function"):
      Syntax.name[Text ?=> Int]
    .assert(_ == t"Text ?=> Int")

    test(m"Show two-parameter function"):
      Syntax.name[(Text, Double) => Int]
    .assert(_ == t"(Text, Double) => Int")

    test(m"Show two-parameter context function"):
      Syntax.name[(Text, Double) ?=> Int]
    .assert(_ == t"(Text, Double) ?=> Int")

    test(m"Show named function"):
      Syntax.name[(text: Text) => Int]
    .assert(_ == t"(text: Text) => Int")

    test(m"Show named context function"):
      Syntax.name[(text: Text) ?=> Int]
    .assert(_ == t"(text: Text) ?=> Int")

    test(m"Show polymorphic lambda"):
      Syntax.name[[field] => (x: field) => Int]
    .assert(_ == t"[field] => (x: field) => Int")

    test(m"Show polymorphic lambda with upper bound"):
      Syntax.name[[field <: String] => (x: field) => Int]
    .assert(_ == t"[field <: String] => (x: field) => Int")

    test(m"Show polymorphic lambda with lower bound"):
      Syntax.name[[field >: String] => (x: field) => Int]
    .assert(_ == t"[field >: String] => (x: field) => Int")

    test(m"Show polymorphic lambda with upper and lower bound"):
      Syntax.name[[field >: String <: AnyRef] => (x: field) => Int]
    .assert(_ == t"[field >: String <: AnyRef] => (x: field) => Int")

    test(m"By-name type"):
      Syntax.name[(=> Int) => Double]
    .assert(_ == t"(=> Int) => Double")

    test(m"Projection type"):
      Syntax.name[Enumeration#Value]
    .assert(_ == t"Enumeration#Value")

    test(m"Refined type"):
      Syntax.name[Addable { type Q = Int }]
    .assert(_ == t"Addable { type Q = Int }")

    test(m"Refined type with def foo(x: Int): String"):
      Syntax.name[Addable { def foo(x: Int): String }]
    .assert(_ == t"Addable { def foo(x: Int): String }")

    test(m"Refined type with def foo: Int => String"):
      Syntax.name[Addable { def foo: Int => String }]
    .assert(_ == t"Addable { def foo: Int => String }")

    test(m"Refined type with def foo: (x: Int) => String"):
      Syntax.name[Addable { def foo: (x: Int) => String }]
    .assert(_ == t"Addable { def foo: (x: Int) => String }")

    test(m"Refined types"):
      Syntax.name[Addable { type Q = Int; type Other = String}]
    .assert(_ == t"Addable { type Q = Int; type Other = String }")

    test(m"Refined higher-kinded type member"):
      Syntax.name[Addable { type Hkt[Bar] = Option[Bar] }]
    .assert(_ == t"Addable { type Hkt = [Bar] =>> Option[Bar] }")

    // test(m"Refined type member with selection"):
    //   Syntax.name[Addable { val foo: String; type Bar = foo.type }]
    // .assert(_ == t"Addable { val foo: String; type Bar = foo.type }")

    test(m"Refined higher-kinded type member 2"):
      Syntax.name[Addable { type Hkt[Bar <: String] = Option[Bar] }]
    .assert(_ == t"Addable { type Hkt = [Bar <: String] =>> Option[Bar] }")

    // test(m"Refined higher-kinded type member 2"):
    //   Syntax.name[Addable { type Hkt[Coll[T] <: Iterable[T]] = Option[Coll[String]] }]
    // .assert(_ == t"Addable { type Hkt[Coll[T] <: Iterable[T]] = Option[Coll[String]] }")

    test(m"Projected refined type"):
      Syntax.name[(Addable { type Q = Int; type Other = String})#Q]
    .assert(_ == t"Int")

    test(m"Function type of infix types"):
      Syntax.name[Int is Addable by Double => String is Checkable against String]
    .assert(_ == t"Int is Addable by Double => String is Checkable against String")

    test(m"Context function type of infix types"):
      Syntax.name[Int is Addable by Double ?=> String is Checkable against String]
    .assert(_ == t"Int is Addable by Double ?=> String is Checkable against String")

    test(m"Named function type of infix types"):
      Syntax.name[(addable: Int is Addable by Double) ?=> String is Checkable against String]
    .assert(_ == t"(addable: Int is Addable by Double) ?=> String is Checkable against String")

    test(m"Tuple"):
      Syntax.name[(Int, String, Exception)]
    .assert(_ == t"(Int, String, Exception)")

    test(m"Named tuple"):
      Syntax.name[(x: Int, y: String, z: Exception)]
    .assert(_ == t"(x: Int, y: String, z: Exception)")

    test(m"Type lambda"):
      Syntax.name[[T] =>> Option[T]]
    .assert(_ == t"[T] =>> Option[T]")

    // test(m"Pure function"):
    //   Syntax.name[Int -> String]
    // .assert(_ == t"Int -> String")

    test(m"Dependent function type"):
      Syntax.name[(e: Enumeration) => e.Value]
    .assert(_ == t"(e: Enumeration) => e.Value")
