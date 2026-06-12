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
package ypsiloid

import anticipation.*
import vacuous.*

extension (yaml: Yaml)
  // Walk the `PositionIndex` to find the `Position` of the node at
  // `pointer`. Returns `Unset` if the pointer doesn't resolve to a
  // node within this `Yaml` or if the `Yaml` was not parsed with
  // `Yaml.Tracking.On`. Mirrors `jacinta.Json#locate`.
  def locate(pointer: YamlPath): Optional[Yaml.Ast.Position] =
    yaml.positionIndex.let: posIndex =>
      walkIndex(yaml.root, posIndex.ints, 0, pointer.path.descent.toIndexedSeq, 0, false)

  // Find the `Position` of the mapping key matching the last segment of
  // `pointer`. Returns `Unset` for sequence-indexed leaves or paths
  // that don't resolve.
  def locateKey(pointer: YamlPath): Optional[Yaml.Ast.Position] =
    yaml.positionIndex.let: posIndex =>
      walkIndex(yaml.root, posIndex.ints, 0, pointer.path.descent.toIndexedSeq, 0, true)

private def walkIndex
  ( ast:      Yaml.Ast,
    data:     IArray[Int],
    offset:   Int,
    segments: IndexedSeq[Text],
    i:        Int,
    keyMode:  Boolean )
:   Optional[Yaml.Ast.Position] =

  if i >= segments.length then
    if keyMode then Unset
    else Yaml.Ast.Position
      ( line   = data(offset + 1),
        column = data(offset + 2),
        length = data(offset + 3) )
  else
    // `YamlPath.path.descent` is stored leaf-first (Serpentine's `/`
    // prepends), so iterate it in reverse to walk root-to-leaf.
    val seg = segments(segments.length - 1 - i).s

    if ast.isObject then
      val k = ast.objectIndexOf(seg)

      if k < 0 then Unset
      else
        val entryOff = data(offset + 5 + k)
        val isLast = i == segments.length - 1

        if isLast && keyMode then
          Yaml.Ast.Position
            ( line   = data(offset + entryOff),
              column = data(offset + entryOff + 1),
              length = data(offset + entryOff + 2) )
        else
          walkIndex
            ( ast.objectValue(k), data, offset + entryOff + 3, segments, i + 1, keyMode )
    else if ast.isArray then
      try
        val k = Integer.parseInt(seg)

        if k < 0 || k >= ast.arrayLength then Unset
        else
          val childOff = data(offset + 5 + k)

          walkIndex
            ( ast.arrayElement(k), data, offset + childOff, segments, i + 1, keyMode )
      catch case _: NumberFormatException => Unset
    else Unset
