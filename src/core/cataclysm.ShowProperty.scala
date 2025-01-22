/*
    Cataclysm, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataclysm

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

object ShowProperty:
  given ShowProperty[Length] = _.show
  given ShowProperty[Duration] = _.show

  given ShowProperty[Dimension] =
    case length: Length => length.show
    case int: Int       => int.show

  given [PropertyType: ShowProperty, PropertyType2: ShowProperty]
          : ShowProperty[(PropertyType, PropertyType2)] = tuple =>
    t"${PropertyType.show(tuple(0))} ${PropertyType2.show(tuple(1))}"

  given [PropertyType: ShowProperty, PropertyType2: ShowProperty, PropertyType3: ShowProperty]
          : ShowProperty[(PropertyType, PropertyType2, PropertyType3)] =

    tuple => List(PropertyType.show(tuple(0)), PropertyType2.show(tuple(1)), PropertyType3.show(tuple(2))).join(t" ")

  given [PropertyType: ShowProperty, PropertyType2: ShowProperty, PropertyType3: ShowProperty, PropertyType4: ShowProperty]
          : ShowProperty[(PropertyType, PropertyType2, PropertyType3, PropertyType4)] = tuple =>
    List
     (PropertyType.show(tuple(0)),
      PropertyType2.show(tuple(1)),
      PropertyType3.show(tuple(2)),
      PropertyType4.show(tuple(3))).join(t" ")

  given ShowProperty[Font] = _.names.map: f =>
    if f.contains(t" ") then t"'$f'" else f

  . join(t", ")

  //given ShowProperty[SimplePath] = path => t"url('${path}')"

  given [PathType: GenericPath]: ShowProperty[PathType] =
    path => t"url('${path.pathText}')"

  given ShowProperty[Text] = identity(_)
  given ShowProperty[Int] = _.show

  given [ColorType: Chromatic]: ShowProperty[ColorType] = color =>
    t"rgb(${ColorType.red(color)},${ColorType.green(color)},${ColorType.blue(color)})"

  //given ShowProperty[Relative] = rel => t"url('$rel')"
  //given ShowProperty[GenericPath] = rel => t"url('$rel')"
  given ShowProperty[PropertyValue] = _.show
  given ShowProperty[Inherit.type] = c => t"inherit"
  given ShowProperty[Transparent.type] = c => t"transparent"
  given ShowProperty[Initial.type] = c => t"initial"

trait ShowProperty[-PropertyType]:
  def show(value: PropertyType): Text
