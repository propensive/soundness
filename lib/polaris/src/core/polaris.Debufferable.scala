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
package polaris

import proscenium.compat.*

import anticipation.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import wisteria.*

object Debufferable extends ProductDerivable[Debufferable]:
  def apply[data](byteWidth: Int)(lambda: (Data, Int) -> data): data is Debufferable =
    new:
      def width: Int = byteWidth

      def debuffer(sextant: Sextant): data =
        lambda(sextant.bytes, sextant.offset).also(sextant.advance(width))

  given b8: B8 is Debufferable = Debufferable(1)(_.readUnchecked(_).bits)
  given b16: B16 is Debufferable = Debufferable(2)(B16(_, _))
  given b32: B32 is Debufferable = Debufferable(4)(B32(_, _))
  given b64: B64 is Debufferable = Debufferable(8)(B64(_, _))
  given s8: S8 is Debufferable = Debufferable(1)(_.readUnchecked(_).bits.s8)
  given s16: S16 is Debufferable = Debufferable(2)(B16(_, _).s16)
  given s32: S32 is Debufferable = Debufferable(4)(B32(_, _).s32)
  given s64: S64 is Debufferable = Debufferable(8)(B64(_, _).s64)
  given u8: U8 is Debufferable = Debufferable(1)(_.readUnchecked(_).bits.u8)
  given u16: U16 is Debufferable = Debufferable(2)(B16(_, _).u16)
  given u32: U32 is Debufferable = Debufferable(4)(B32(_, _).u32)
  given u64: U64 is Debufferable = Debufferable(8)(B64(_, _).u64)
  given byte: Byte is Debufferable = Debufferable(1)(_.readUnchecked(_))
  given short: Short is Debufferable = Debufferable(2)(B16(_, _).s16.short)
  given int: Int is Debufferable = Debufferable(4)(B32(_, _).s32.int)
  given long: Long is Debufferable = Debufferable(8)(B64(_, _).s64.long)

  class Join[derivation <: Product: ProductReflection]
    ( val width: Int, debuffer0: Sextant -> derivation )
  extends Debufferable:
    type Self = derivation
    def debuffer(sextant: Sextant): derivation = debuffer0(sextant)

  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Debufferable =
    // The lambda parameter is ascribed so its type is not inferred from the expected arrow:
    // inferred, it adopts a memoized root capability minted by the unit's first expansion of
    // this derivation, and any second expansion in the same unit fails the root-visibility
    // check (upstream #26547, from 2026-07-17 nightlies).
    Join[derivation]
      ( contexts[derivation]() { [field] => _.width }.sum,
        (sextant: Sextant) => build { [field] => context => context.debuffer(sextant) } )

trait Debufferable extends Typeclass:
  def width: Int
  def debuffer(sextant: Sextant): Self
