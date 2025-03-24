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
package polaris

import wisteria.*

object Bufferable extends ProductDerivable[Bufferable]:

  // given B8 is Bufferable = Bufferable(1)(_(_).bits)
  // given B16 is Bufferable = Bufferable(2)(B16(_, _))
  // given B32 is Bufferable = Bufferable(4)(B32(_, _))
  //given B64 is Bufferable = Bufferable(8)(B64(_, _))

  // given I8 is Bufferable = Bufferable(1)(_(_).bits.i8)
  // given I16 is Bufferable = Bufferable(2)(B16(_, _).i16)
  // given I32 is Bufferable = Bufferable(4)(B32(_, _).i32)
  // given I64 is Bufferable = Bufferable(8)(B64(_, _).i64)

  // given U8 is Bufferable = Bufferable(1)(_(_).bits.u8)
  // given U16 is Bufferable = Bufferable(2)(B16(_, _).u16)
  // given U32 is Bufferable = Bufferable(4)(B32(_, _).u32)
  // given U64 is Bufferable = Bufferable(8)(B64(_, _).u64)

  // given Byte is Bufferable = Bufferable(1)(_(_))
  // given Short is Bufferable = Bufferable(2)(B16(_, _).i16.short)
  // given Int is Bufferable = Bufferable(4)(B32(_, _).i32.int)
  // given Long is Bufferable = Bufferable(8)(B64(_, _).i64.long)

  class Join[derivation <: Product: ProductReflection]
     (val width: Int, buffer0: (Buffer, derivation) => Unit)
  extends Bufferable:
    type Self = derivation
    def buffer(buffer: Buffer, value: derivation): Unit = buffer0(buffer, value)

  inline def join[derivation <: Product: ProductReflection]: derivation is Bufferable =
    Join[derivation]
     (contexts { [field] => _.width }.sum,
      (buffer, value) => fields(value) { [field] => field => context.buffer(buffer, field) })

trait Bufferable:
  type Self
  def width: Int
  def buffer(buffer: Buffer, value: Self): Unit
