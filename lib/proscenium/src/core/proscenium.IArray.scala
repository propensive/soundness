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
package proscenium

import scala.reflect.ClassTag

// The blessed immutable array: an opaque alias over the stdlib's `IArray`, so the runtime
// representation stays a raw JVM array while the partial stdlib surface (`apply`, `head`,
// `last`, …) is hidden behind the typeclass layer and the greppable `stdlib` bridge, exactly
// like `List` and `Series`. Construction always needs a `ClassTag` (a JVM array is
// element-typed), so every constructor takes one — including the `Convertible` instance that
// serves `xs.to[IArray]`.
//
// Pattern support: `case IArray(a, b)` matches via `unapplySeq`.
object IArray:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  def of[element](iarray: scala.IArray[element]): IArray[element] =
    iarray.asInstanceOf[IArray[element]]

  def apply[element: ClassTag](elements: element*): IArray[element] =
    of(scala.IArray(elements*))

  def empty[element: ClassTag]: IArray[element] = of(scala.IArray.empty[element])

  def unsafeFromArray[element](array: scala.Array[element]): IArray[element] =
    array.asInstanceOf[IArray[element]]

  // The transitional home of the `IArray`-producing freeze: the frozen form of an array is
  // now `Array[element]^{}` (see `Array.freeze`), so this exists only to keep call sites
  // compiling until they drain, and is retired with `IArray` itself. The soundness
  // argument is `consume`, exactly as for `Array.freeze`.
  def freeze[element](consume array: Array[element]^): IArray[element] =
    array.asInstanceOf[IArray[element]]

  def from[element: ClassTag](elements: IterableOnce[element]^): IArray[element] =
    of(scala.IArray.from(elements))

  def fill[element: ClassTag](count: Int)(element: => element): IArray[element] =
    of(scala.IArray.fill(count)(element))

  def tabulate[element: ClassTag](count: Int)(lambda: Int => element): IArray[element] =
    of(scala.IArray.tabulate(count)(lambda))

  def range(start: Int, end: Int): IArray[Int] = of(scala.IArray.range(start, end))

  def unapplySeq[element](iarray: IArray[element]): Option[Seq[element]] =
    Some(iarray.stdlib.toSeq)

  extension [element](iarray: IArray[element])
    inline def stdlib: scala.IArray[element] = iarray.asInstanceOf[scala.IArray[element]]

  // The erasure of the opaque alias is the underlying JVM array, so the `ClassTag` is the
  // element tag's array form; without this, summoning `ClassTag[IArray[element]]` outside
  // `proscenium` fails (the alias is abstract there).
  given classTag: [element] => (tag: ClassTag[element]) => ClassTag[IArray[element]] =
    tag.wrap.asInstanceOf[ClassTag[IArray[element]]]

opaque type IArray[+element] = scala.IArray[element]
