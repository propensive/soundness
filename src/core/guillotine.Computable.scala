/*
    Guillotine, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import language.experimental.pureFunctions

import java.io as ji

import scala.jdk.StreamConverters.StreamHasToScala

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import spectacular.*
import turbulence.*

object Computable:
  given lazyList: LazyList[Text] is Computable = proc =>
    val reader = ji.BufferedReader(ji.InputStreamReader(proc.getInputStream))
    reader.lines().nn.toScala(LazyList).map(_.tt)

  given list: List[Text] is Computable = proc =>
    val reader = ji.BufferedReader(ji.InputStreamReader(proc.getInputStream))
    reader.lines().nn.toScala(List).map(_.tt)

  given text: Text is Computable = proc =>
    Text.construct(lazyList.compute(proc).map(_.s).each(append(_)))

  given string: String is Computable = proc =>
    Text.construct(lazyList.compute(proc).map(_.s).each(append(_))).s

  given dataStream: LazyList[Bytes] is Computable raises StreamError =
    proc => Readable.inputStream.stream(proc.getInputStream.nn)

  given exitStatus: Exit is Computable = _.waitFor() match
    case 0     => Exit.Ok
    case other => Exit.Fail(other)

  given Unit is Computable = exitStatus.map(_ => ())

  given [PathType: SpecificPath] => PathType is Computable =
    proc => SpecificPath(text.compute(proc))

trait Computable:
  type Self
  def compute(process: java.lang.Process): Self

  def map[SelfType2](lambda: Self => SelfType2): SelfType2 is Computable =
    process => lambda(compute(process))
