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
package mercator

import scala.collection.immutable.Vector

import soundness.*

import scala.util.{Try, Success}

object Tests extends Suite(m"Mercator tests"):
  def run(): Unit =
    test(m"Identity for Option"):
      val point = summon[Identity[Option]]
      point.point(1)
    . assert(_ == Some(1))

    test(m"Identity for List"):
      summon[Identity[scala.collection.immutable.List]].point(1)
    . assert(_ == scala.collection.immutable.List(1))

    // `scala.collection.immutable.Set`, not the opaque `Set`: mercator's derivation is for
    // stdlib `Iterable` constructors (as with `Vector` above).
    test(m"Identity for Set"):
      summon[Identity[scala.collection.immutable.Set]].point(1)
    . assert(_ == scala.collection.immutable.Set(1))

    // `Vector`, not `Vector`: mercator's derivation is for stdlib `Iterable` constructors, which
    // the opaque `Vector` deliberately is not. (Whether the opaque aliases should carry mercator
    // instances is an open design question for the wider collections migration.)
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
      val functor = summon[Functor[scala.collection.immutable.List]]
      functor.map(scala.collection.immutable.List(1, 2, 3))(_ + 1)
    . assert(_ == List(2, 3, 4))

    test(m"Functor for Set"):
      val functor = summon[Functor[scala.collection.immutable.Set]]
      functor.map(scala.collection.immutable.Set(1, 3, 5))(_ + 1)
    . assert(_ == scala.collection.immutable.Set(2, 4, 6))

    test(m"Functor for Vector"):
      val functor = summon[Functor[Vector]].asInstanceOf[Functor[Vector]]
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
      monad.bind(Some(1)) { v => Some(v + 1) }
    . assert(_ == Some(2))

    test(m"Monad for List"):
      val monad = summon[Monad[scala.collection.immutable.List]]
      monad.bind(scala.collection.immutable.List(1, 2, 3)) { v => if v > 1 then scala.collection.immutable.List(v + 1) else scala.collection.immutable.List.empty }
    . assert(_ == List(3, 4))

    test(m"Monad for Set"):
      val monad = summon[Monad[scala.collection.immutable.Set]]
      monad.bind(scala.collection.immutable.Set(1, 3, 5)):
        v => scala.collection.immutable.Set(v + 1)
    . assert(_ == scala.collection.immutable.Set(2, 4, 6))

    test(m"Monad for Vector"):
      val monad = summon[Monad[Vector]].asInstanceOf[Monad[Vector]]
      monad.bind(Vector(1, 2, 3)) { v => Vector(v + 1, v + 1) }
    . assert(_ == Vector(2, 2, 3, 3, 4, 4))

    test(m"Monad for Try"):
      val monad = summon[Monad[scala.util.Try]]
      monad.bind(scala.util.Success(1)) { v => scala.util.Try(v + 1) }
    . assert(_ == scala.util.Success(2))

    test(m"Sequence on List/Try"):
      scala.collection.immutable.List(Try(1), Try(2), Try(3)).sequence
    . assert(_ == Success(scala.collection.immutable.List(1, 2, 3)))

    // test(m"Sequence on Vector/Try"):
    //   Vector(Try(1), Try(2), Try(3)).sequence
    // .assert(_ == Success(Vector(1, 2, 3)))

    test(m"Sequence on List/Set"):
      scala.collection.immutable.List
        ( scala.collection.immutable.Set(1),
          scala.collection.immutable.Set(2),
          scala.collection.immutable.Set(3) )
      . sequence
    . assert(_ == scala.collection.immutable.Set(scala.collection.immutable.List(1, 2, 3)))

    // test(m"Monad for Either"):
    //   val monad = summon[Monad[[T] =>> Either[Any, T]]]
    //   monad.bind(Right(1)) { v => Right(v + 1) }
    // .assert(_ == Right(2))

    test(m"Identity for Ordering does not exist"):
      demilitarize:
        summon[Identity[Ordering]].point(1)
      .map(_.message)
    . assert(_ == List("[↯SN-569] the companion object Ordering has no candidate apply methods"))
