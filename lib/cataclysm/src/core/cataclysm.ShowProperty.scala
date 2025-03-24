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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package cataclysm

import anticipation.*
import gossamer.*
import prepositional.*
import proscenium.*
import quantitative.*
import spectacular.*

object ShowProperty:
  given ShowProperty[Length] = _.show
  given ShowProperty[Quantity[Seconds[1]]] = quantity => t"${quantity.value}s"

  given ShowProperty[Int | Length] =
    case length: Length => length.show
    case int: Int       => int.show

  given [property: ShowProperty, property2: ShowProperty]
  :     ShowProperty[(property, property2)] = tuple =>
    t"${property.show(tuple(0))} ${property2.show(tuple(1))}"

  given [property: ShowProperty, property2: ShowProperty, property3: ShowProperty]
  :     ShowProperty[(property, property2, property3)] =

    tuple =>
      List
       (property.show(tuple(0)),
        property2.show(tuple(1)),
        property3.show(tuple(2)))
      . join(t" ")

  given [property:  ShowProperty,
         property2: ShowProperty,
         property3: ShowProperty,
         property4: ShowProperty]
  =>    ShowProperty[(property, property2, property3, property4)] = tuple =>
    List
     (property.show(tuple(0)),
      property2.show(tuple(1)),
      property3.show(tuple(2)),
      property4.show(tuple(3))).join(t" ")

  given ShowProperty[Font] = _.names.map: f =>
    if f.contains(t" ") then t"'$f'" else f

  . join(t", ")

  //given ShowProperty[SimplePath] = path => t"url('${path}')"

  given [path: Abstractable across Paths into Text] => ShowProperty[path] =
    path => t"url('${path.generic}')"

  given ShowProperty[Text] = identity(_)
  given ShowProperty[Int] = _.show

  given [chromatic: Chromatic]: ShowProperty[chromatic] = color =>
    t"rgb(${chromatic.red(color)},${chromatic.green(color)},${chromatic.blue(color)})"

  //given ShowProperty[Relative] = rel => t"url('$rel')"
  //given ShowProperty[GenericPath] = rel => t"url('$rel')"
  given ShowProperty[PropertyValue] = _.show
  given ShowProperty[Inherit.type] = c => t"inherit"
  given ShowProperty[Transparent.type] = c => t"transparent"
  given ShowProperty[Initial.type] = c => t"initial"

trait ShowProperty[-property]:
  def show(value: property): Text
