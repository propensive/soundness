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

import vacuous.*

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

  // Every symbol the predicate admits, as coalesced runs. Scanning the whole domain is what
  // makes the Unicode classes exact — the JDK's own tables answer the predicate, so there is no
  // second copy of the Unicode database here to drift out of date — and it costs one pass per
  // distinct class, which `unicode` memoises.
  def where(predicate: Int => Boolean): Ranges =
    val spans = scala.collection.mutable.ListBuffer.empty[Int]
    var symbol = 0
    var start = -1

    while symbol <= maxSymbol do
      if predicate(symbol) then (if start < 0 then start = symbol)
      else if start >= 0 then
        spans += start
        spans += symbol - 1
        start = -1

      symbol += 1

    if start >= 0 then
      spans += start
      spans += maxSymbol

    new Ranges(spans.to(List))

  // The POSIX classes of RE2's `[[:name:]]` form, all ASCII-only.
  val posix: Map[String, Ranges] = Map
    ( ("alnum": String)  -> Ranges('0', '9').union(Ranges('A', 'Z')).union(Ranges('a', 'z')),
      ("alpha": String)  -> Ranges('A', 'Z').union(Ranges('a', 'z')),
      ("ascii": String)  -> Ranges(0x00, 0x7f),
      ("blank": String)  -> Ranges.point('\t').union(Ranges.point(' ')),
      ("cntrl": String)  -> Ranges(0x00, 0x1f).union(Ranges.point(0x7f)),
      ("digit": String)  -> digit,
      ("graph": String)  -> Ranges('!', '~'),
      ("lower": String)  -> Ranges('a', 'z'),
      ("print": String)  -> Ranges(' ', '~'),
      ("punct": String)  -> Ranges('!', '/').union(Ranges(':', '@')).union(Ranges('[', '`'))
                  . union(Ranges('{', '~')),
      ("space": String)  -> Ranges('\t', '\r').union(Ranges.point(' ')),
      ("upper": String)  -> Ranges('A', 'Z'),
      ("word": String)   -> word,
      ("xdigit": String) -> Ranges('0', '9').union(Ranges('A', 'F')).union(Ranges('a', 'f')) )

  private val unicodeClasses: scala.collection.concurrent.TrieMap[String, Optional[Ranges]] =
    scala.collection.concurrent.TrieMap()

  // A Unicode general category (`L`, `Lu`, `Nd`, …) or script (`Greek`, `Han`, …) by the name
  // RE2's `\p{…}` uses, or `Unset` if no such class exists. `Any` is RE2's name for the whole
  // domain. General categories are matched by prefix, so the one-letter `L` is the union of
  // `Lu`, `Ll`, `Lt`, `Lm` and `Lo`, exactly as RE2 defines it.
  def unicode(name: String): Optional[Ranges] =
    unicodeClasses.getOrElseUpdate(name, compute(name))

  private def compute(name: String): Optional[Ranges] =
    if name == "Any" then anySymbol
    else if categories.stdlib.contains(name) then where(symbol => categoryName(symbol) == name)
    else if categoryGroups.stdlib.contains(name)
    then where(symbol => categoryName(symbol).startsWith(name))
    else
      try
        val script = Character.UnicodeScript.forName(name).nn
        where(symbol => Character.UnicodeScript.of(symbol) == script)
      catch case _: IllegalArgumentException => Unset

  // The two-letter general-category abbreviation for a codepoint, as Unicode names them.
  private def categoryName(symbol: Int): String = Character.getType(symbol) match
    case Character.UPPERCASE_LETTER          => "Lu"
    case Character.LOWERCASE_LETTER          => "Ll"
    case Character.TITLECASE_LETTER          => "Lt"
    case Character.MODIFIER_LETTER           => "Lm"
    case Character.OTHER_LETTER              => "Lo"
    case Character.NON_SPACING_MARK          => "Mn"
    case Character.COMBINING_SPACING_MARK    => "Mc"
    case Character.ENCLOSING_MARK            => "Me"
    case Character.DECIMAL_DIGIT_NUMBER      => "Nd"
    case Character.LETTER_NUMBER             => "Nl"
    case Character.OTHER_NUMBER              => "No"
    case Character.CONNECTOR_PUNCTUATION     => "Pc"
    case Character.DASH_PUNCTUATION          => "Pd"
    case Character.START_PUNCTUATION         => "Ps"
    case Character.END_PUNCTUATION           => "Pe"
    case Character.INITIAL_QUOTE_PUNCTUATION => "Pi"
    case Character.FINAL_QUOTE_PUNCTUATION   => "Pf"
    case Character.OTHER_PUNCTUATION         => "Po"
    case Character.MATH_SYMBOL               => "Sm"
    case Character.CURRENCY_SYMBOL           => "Sc"
    case Character.MODIFIER_SYMBOL           => "Sk"
    case Character.OTHER_SYMBOL              => "So"
    case Character.SPACE_SEPARATOR           => "Zs"
    case Character.LINE_SEPARATOR            => "Zl"
    case Character.PARAGRAPH_SEPARATOR       => "Zp"
    case Character.CONTROL                   => "Cc"
    case Character.FORMAT                    => "Cf"
    case Character.SURROGATE                 => "Cs"
    case Character.PRIVATE_USE               => "Co"
    case _                                   => "Cn"

  private val categories: Set[String] = Set
    ( "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps", "Pe",
      "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn" )

  private val categoryGroups: Set[String] = Set("L", "M", "N", "P", "S", "Z", "C")

  // Codepoints equivalent under simple case folding, grouped into orbits keyed by their folded
  // form. An orbit is not always a pair: `K`, `k` and the Kelvin sign all fold together, which
  // is why folding cannot be done by mapping each symbol up and down in isolation.
  private lazy val foldOrbits: scala.collection.immutable.Map[Int, scala.Array[Int]] =
    val groups = scala.collection.mutable.HashMap.empty[Int, scala.List[Int]]
    var symbol = 0

    while symbol <= maxSymbol do
      val folded = Character.toLowerCase(Character.toUpperCase(symbol))
      if folded != symbol then groups(folded) = symbol :: groups.getOrElse(folded, scala.Nil)
      symbol += 1

    groups.map((key, members) => (key, (key :: members).toArray)).toMap

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

  // The closure of this set under simple case folding, as RE2's `i` flag defines it. Only the
  // orbits are examined, not the whole domain, so this is proportional to the number of
  // case-varying codepoints rather than to the size of the set.
  def folded: Ranges =
    var result = this

    Ranges.foldOrbits.foreach: (_, members) =>
      var index = 0
      var hit = false

      while !hit && index < members.length do
        hit = contains(members(index))
        index += 1

      if hit then
        var at = 0
        while at < members.length do
          result = result.union(Ranges.point(members(at)))
          at += 1

    result
