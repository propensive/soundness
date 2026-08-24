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
package praxinoscope

object Ranges:
  // The greatest symbol in the default (Unicode codepoint) domain. Other symbol domains (UTF-8
  // bytes, say) are strictly smaller, so this is a safe universal bound for `negate`.
  val maxSymbol: Int = 0x10ffff

  val empty: Ranges = new Ranges(Nil)
  def apply(lo: Int, hi: Int): Ranges = new Ranges(lo :: hi :: Nil)
  def point(symbol: Int): Ranges = new Ranges(symbol :: symbol :: Nil)

  // The RE2 perl classes are ASCII-only.
  val digit: Ranges = Ranges('0'.toInt, '9'.toInt)

  val word: Ranges =
    new Ranges
      ( '0'.toInt :: '9'.toInt :: 'A'.toInt :: 'Z'.toInt :: '_'.toInt :: '_'.toInt ::
        'a'.toInt :: 'z'.toInt :: Nil )

  val space: Ranges = new Ranges('\t'.toInt :: '\r'.toInt :: ' '.toInt :: ' '.toInt :: Nil)
  val any: Ranges = new Ranges(0 :: '\n'.toInt - 1 :: '\n'.toInt + 1 :: maxSymbol :: Nil)
  val anySymbol: Ranges = new Ranges(0 :: maxSymbol :: Nil)

// A set of symbols, represented as a sorted, disjoint, non-adjacent list of inclusive
// `lo :: hi :: …` bounds. Symbols are `Int`-encoded members of an ordered alphabet: Unicode
// codepoints today, though nothing here assumes more than an ordering and an upper bound. The
// constructor is private because it admits unnormalized bounds; every public constructor and
// operation preserves the invariant.
case class Ranges private(spans: List[Int]):
  def vacant: Boolean = spans == Ranges.empty.spans

  def contains(symbol: Int): Boolean =
    def recur(todo: List[Int]): Boolean = todo match
      case lo :: hi :: tail => if symbol < lo then false else symbol <= hi || recur(tail)
      case _                => false

    recur(spans)

  def union(that: Ranges): Ranges =
    def interleave(left: List[Int], right: List[Int]): List[Int] = (left, right) match
      case (Nil, _) => right
      case (_, Nil) => left

      case (lo1 :: hi1 :: tail1, lo2 :: hi2 :: tail2) =>
        if lo1 <= lo2 then lo1 :: hi1 :: interleave(tail1, right)
        else lo2 :: hi2 :: interleave(left, tail2)

      case _ => Nil

    def coalesce(todo: List[Int]): List[Int] = todo match
      case lo1 :: hi1 :: lo2 :: hi2 :: tail =>
        if lo2 <= hi1 + 1 then coalesce(lo1 :: Math.max(hi1, hi2) :: tail)
        else lo1 :: hi1 :: coalesce(lo2 :: hi2 :: tail)

      case other => other

    new Ranges(coalesce(interleave(spans, that.spans)))

  def negate(max: Int = Ranges.maxSymbol): Ranges =
    def recur(todo: List[Int], from: Int): List[Int] = todo match
      case lo :: hi :: tail =>
        if from < lo then from :: (lo - 1) :: recur(tail, hi + 1) else recur(tail, hi + 1)

      case _ =>
        if from <= max then from :: max :: Nil else Nil

    new Ranges(recur(spans, 0))

  def intersect(that: Ranges): Ranges = negate().union(that.negate()).negate()
