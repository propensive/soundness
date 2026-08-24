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
package enigmatic

import scala.math

import anticipation.*
import breviloquence.*
import vacuous.*

// Deterministic CBOR encoding per RFC 8949 §4.2.1, as required by RFC 9052 §9.
// The `Cbor.Ast` encoder already emits shortest-form integers and definite-length
// items; what's missing is map-key ordering (bytewise lexicographic order of the
// encoded keys).
object CanonicalCbor:
  def encode(ast: Cbor.Ast): Data = canonicalise(ast).encode

  private def canonicalise(ast: Cbor.Ast): Cbor.Ast =
    if ast.isMap then
      val n = ast.entries
      val builder = scala.collection.mutable.ArrayBuffer.empty[(Data, Cbor.Ast, Cbor.Ast)]
      var index = 0

      while index < n do
        val canonicalKey = canonicalise(ast.key(index))
        val canonicalValue = canonicalise(ast.value(index))
        builder += ((canonicalKey.encode, canonicalKey, canonicalValue))
        index += 1

      val sorted = builder.sortWith: (a, b) => compareBytes(a._1, b._1) < 0
      val keys = Array.allocate[Any](n)
      val values = Array.allocate[Any](n)
      var write = 0

      while write < n do
        keys(write) = sorted(write)._2
        values(write) = sorted(write)._3
        write += 1

      Cbor.Ast.map(Array.freeze(keys), Array.freeze(values))
    else if ast.isArray then
      val n = ast.elements
      val out = Array.allocate[Any](n)
      var index = 0

      while index < n do
        out(index) = canonicalise(ast.element(index))
        index += 1

      Cbor.Ast.array(Array.freeze(out))
    else if ast.isTag then
      val tag = ast.asInstanceOf[Cbor.Tag]
      Cbor.Ast(Cbor.Tag(tag.tag, canonicalise(tag.value.asInstanceOf[Cbor.Ast])))
    else
      ast

  private def compareBytes(a: Data, b: Data): Int =
    val n = math.min(a.length, b.length)
    var index = 0

    while index < n do
      val diff = (a.readable(index) & 0xFF) - (b.readable(index) & 0xFF)
      if diff != 0 then return diff
      index += 1

    a.length - b.length
