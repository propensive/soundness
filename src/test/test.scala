/*
    Mercator, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package mercator

import probably.*
import gossamer.*
import larceny.*

import scala.util.{Try, Success}

object Tests extends Suite(t"Mercator tests"):
  def run(): Unit =
    test(t"Point for Option"):
      val point = summon[Point[Option]]
      point.point(1)
    .assert(_ == Some(1))
    
    test(t"Point for List"):
      summon[Point[List]].point(1)
    .assert(_ == List(1))
    
    test(t"Point for Set"):
      summon[Point[Set]].point(1)
    .assert(_ == Set(1))
    
    test(t"Point for Vector"):
      summon[Point[Vector]].point(1)
    .assert(_ == Vector(1))
    
    test(t"Point for Try"):
      summon[Point[scala.util.Try]].point(1)
    .assert(_ == scala.util.Success(1))
    
    test(t"Point for Either"):
      summon[Point[[T] =>> Either[Any, T]]].point(1)
    .assert(_ == Right(1))
    
    test(t"Functor for Option"):
      val functor = summon[Functor[Option]]
      functor.map(Some(1))(_ + 1)
    .assert(_ == Some(2))
    
    test(t"Functor for List"):
      val functor = summon[Functor[List]]
      functor.map(List(1, 2, 3))(_ + 1)
    .assert(_ == List(2, 3, 4))
    
    test(t"Functor for Set"):
      val functor = summon[Functor[Set]]
      functor.map(Set(1, 3, 5))(_ + 1)
    .assert(_ == Set(2, 4, 6))
    
    test(t"Functor for Vector"):
      val functor = summon[Functor[Vector]]
      functor.map(Vector(1, 2, 3))(_ + 1)
    .assert(_ == Vector(2, 3, 4))
    
    test(t"Functor for Try"):
      val functor = summon[Functor[scala.util.Try]]
      functor.map(scala.util.Success(1))(_ + 1)
    .assert(_ == scala.util.Success(2))
    
    test(t"Functor for Either"):
      val functor = summon[Functor[[T] =>> Either[Any, T]]]
      functor.map(Right(1))(_ + 1)
    .assert(_ == Right(2))
    
    test(t"Monad for Option"):
      val monad = summon[Monad[Option]]
      monad.flatMap(Some(1)) { v => Some(v + 1) }
    .assert(_ == Some(2))
    
    test(t"Monad for List"):
      val monad = summon[Monad[List]]
      monad.flatMap(List(1, 2, 3)) { v => if v > 1 then List(v + 1) else Nil }
    .assert(_ == List(3, 4))
    
    test(t"Monad for Set"):
      val monad = summon[Monad[Set]]
      monad.flatMap(Set(1, 3, 5)) { v => Set(v + 1) }
    .assert(_ == Set(2, 4, 6))
    
    test(t"Monad for Vector"):
      val monad = summon[Monad[Vector]]
      monad.flatMap(Vector(1, 2, 3)) { v => Vector(v + 1, v + 1) }
    .assert(_ == Vector(2, 2, 3, 3, 4, 4))
    
    test(t"Monad for Try"):
      val monad = summon[Monad[scala.util.Try]]
      monad.flatMap(scala.util.Success(1)) { v => scala.util.Try(v + 1) }
    .assert(_ == scala.util.Success(2))
    
    test(t"Sequence on List/Try"):
      List(Try(1), Try(2), Try(3)).sequence
    .assert(_ == Success(List(1, 2, 3)))
    
    // test(t"Sequence on Vector/Try"):
    //   Vector(Try(1), Try(2), Try(3)).sequence
    // .assert(_ == Success(Vector(1, 2, 3)))
    
    test(t"Sequence on List/Set"):
      List(Set(1), Set(2), Set(3)).sequence
    .assert(_ == Set(List(1, 2, 3)))
    
    // test(t"Monad for Either"):
    //   val monad = summon[Monad[[T] =>> Either[Any, T]]]
    //   monad.flatMap(Right(1)) { v => Right(v + 1) }
    // .assert(_ == Right(2))

    test(t"Point for Ordering does not exist"):
      demilitarize:
        summon[Point[Ordering]].point(1)
      .map(_.message)
    .assert(_ == List("mercator: the companion object Ordering has no candidate apply methods"))
