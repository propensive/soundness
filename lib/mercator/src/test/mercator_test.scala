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
package mercator

import soundness.*

import scala.util.{Try, Success}

object Tests extends Suite(m"Mercator tests"):
  def run(): Unit =
    test(m"Identity for Option"):
      val point = summon[Identity[Option]]
      point.point(1)
    . assert(_ == Some(1))

    test(m"Identity for List"):
      summon[Identity[List]].point(1)
    . assert(_ == List(1))

    test(m"Identity for Set"):
      summon[Identity[Set]].point(1)
    . assert(_ == Set(1))

    test(m"Identity for Vector"):
      summon[Identity[Vector]].point(1)
    . assert(_ == Vector(1))

    test(m"Identity for Try"):
      summon[Identity[scala.util.Try]].point(1)
    . assert(_ == scala.util.Success(1))

    test(m"Identity for Either"):
      summon[Identity[[T] =>> Either[Any, T]]].point(1)
    . assert(_ == Right(1))

    test(m"Functor for Option"):
      val functor = summon[Functor[Option]]
      functor.map(Some(1))(_ + 1)
    . assert(_ == Some(2))

    test(m"Functor for List"):
      val functor = summon[Functor[List]]
      functor.map(List(1, 2, 3))(_ + 1)
    . assert(_ == List(2, 3, 4))

    test(m"Functor for Set"):
      val functor = summon[Functor[Set]]
      functor.map(Set(1, 3, 5))(_ + 1)
    . assert(_ == Set(2, 4, 6))

    test(m"Functor for Vector"):
      val functor = summon[Functor[Vector]]
      functor.map(Vector(1, 2, 3))(_ + 1)
    . assert(_ == Vector(2, 3, 4))

    test(m"Functor for Try"):
      val functor = summon[Functor[scala.util.Try]]
      functor.map(scala.util.Success(1))(_ + 1)
    . assert(_ == scala.util.Success(2))

    test(m"Functor for Either"):
      val functor = summon[Functor[[T] =>> Either[Any, T]]]
      functor.map(Right(1))(_ + 1)
    . assert(_ == Right(2))

    test(m"Monad for Option"):
      val monad = summon[Monad[Option]]
      monad.flatMap(Some(1)) { v => Some(v + 1) }
    . assert(_ == Some(2))

    test(m"Monad for List"):
      val monad = summon[Monad[List]]
      monad.flatMap(List(1, 2, 3)) { v => if v > 1 then List(v + 1) else Nil }
    . assert(_ == List(3, 4))

    test(m"Monad for Set"):
      val monad = summon[Monad[Set]]
      monad.flatMap(Set(1, 3, 5)) { v => Set(v + 1) }
    . assert(_ == Set(2, 4, 6))

    test(m"Monad for Vector"):
      val monad = summon[Monad[Vector]]
      monad.flatMap(Vector(1, 2, 3)) { v => Vector(v + 1, v + 1) }
    . assert(_ == Vector(2, 2, 3, 3, 4, 4))

    test(m"Monad for Try"):
      val monad = summon[Monad[scala.util.Try]]
      monad.flatMap(scala.util.Success(1)) { v => scala.util.Try(v + 1) }
    . assert(_ == scala.util.Success(2))

    test(m"Sequence on List/Try"):
      List(Try(1), Try(2), Try(3)).sequence
    . assert(_ == Success(List(1, 2, 3)))

    // test(m"Sequence on Vector/Try"):
    //   Vector(Try(1), Try(2), Try(3)).sequence
    // .assert(_ == Success(Vector(1, 2, 3)))

    test(m"Sequence on List/Set"):
      List(Set(1), Set(2), Set(3)).sequence
    . assert(_ == Set(List(1, 2, 3)))

    // test(m"Monad for Either"):
    //   val monad = summon[Monad[[T] =>> Either[Any, T]]]
    //   monad.flatMap(Right(1)) { v => Right(v + 1) }
    // .assert(_ == Right(2))

    test(m"Identity for Ordering does not exist"):
      demilitarize:
        summon[Identity[Ordering]].point(1)
      .map(_.message)
    . assert(_ == List("[↯SN-mc/1] the companion object Ordering has no candidate apply methods"))
