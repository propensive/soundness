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

package wisteria.examples

import wisteria.*
import gossamer.*

extension [A: Csv](value: A) def csv: List[Text] = summon[Csv[A]](value)

trait Csv[A]:
  def apply(a: A): List[Text]

object Csv extends Derivation[Csv]:
  def join[A](ctx: CaseClass[Csv, A]): Csv[A] = a =>
    ctx.params.foldLeft(List[Text]()):
      (acc, p) => acc ++ p.typeclass(p.deref(a))

  def split[A](ctx: SealedTrait[Csv, A]): Csv[A] = ctx.choose(_):
    sub => sub.typeclass(sub.value)

  given Csv[Text] = List(_)
  given Csv[Int] = i => List(i.show)
  given Csv[Char] = c => List(c.show)
  given [T: Csv]: Csv[Seq[T]] = _.to(List).flatMap(summon[Csv[T]](_))

case class Foo(x: Int, y: Text) derives Csv
case class Bar(c: Char, fs: Foo*) derives Csv
