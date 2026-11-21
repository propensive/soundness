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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.caps
import scala.collection.immutable as sci
import scala.quoted.*

object PerfectMap extends caps.Pure:
  // What a map's keys are, which decides how a key becomes the word that is hashed: a text by
  // its hash code, which the language specifies and the JVM keeps once it has it; a number or
  // a character by its value. Keys of any other kind have no perfect map.
  enum Kind:
    case Textual, Integral, Wide, Symbolic

  def kind(key: Any): Kind | Null = key match
    case _: String              => Kind.Textual
    case _: java.lang.Integer   => Kind.Integral
    case _: java.lang.Long      => Kind.Wide
    case _: java.lang.Character => Kind.Symbolic
    case _                      => null

  def word(key: Any): Long = key match
    case text: String                => text.hashCode.toLong
    case number: java.lang.Integer   => number.intValue.toLong
    case number: java.lang.Long      => number.longValue
    case symbol: java.lang.Character => symbol.charValue.toLong
    case _                           => 0L

  // The body of `index` for keys of one kind: the program run over the key's word, or -1 for
  // a value of any other kind. `table` is as for `PerfectHash.quoted`.
  def indexing(kind: Kind, hash: PerfectHash, key: Expr[Any])
    ( table: => Expr[scala.Array[Int]^{caps.any.rd}] )
    ( using Quotes )
  :   Expr[Int] =

    def slot(word: Expr[Long]): Expr[Int] = hash.quoted(List.of(sci.List(word)))(table)

    kind match
      case Kind.Textual =>
        ' {
            $key match
              case text: String => ${slot('{text.hashCode.toLong})}
              case _            => -1
          }

      case Kind.Integral =>
        ' {
            $key match
              case number: java.lang.Integer => ${slot('{number.intValue.toLong})}
              case _                         => -1
          }

      case Kind.Wide =>
        ' {
            $key match
              case number: java.lang.Long => ${slot('{number.longValue})}
              case _                      => -1
          }

      case Kind.Symbolic =>
        ' {
            $key match
              case symbol: java.lang.Character => ${slot('{symbol.charValue.toLong})}
              case _                           => -1
          }

  // The table of a map whose layout a macro worked out: entry i goes to slot `positions(i)`.
  // Trusted, since the macro that calls it placed each key by running the very program the
  // map will read by.
  def slots[key, value]
    ( capacity: Int, positions: scala.Array[Int]^{caps.any.rd}, entries: sci.Seq[(key, value)] )
  :   scala.Array[Any]^{} =

    val table: scala.Array[Any]^ = new scala.Array[Any](capacity << 1)
    val iterator = entries.iterator
    var index = 0

    // Terminated by the source's own state.
    while iterator.hasNext do
      val entry = iterator.next()
      val at = positions(index)
      table(at << 1) = entry._1
      table((at << 1) + 1) = entry._2
      index += 1

    // [transfer] freeze: the table is written only above, and never after this
    caps.unsafe.unsafeAssumePure(table)

  // A hash's table, frozen, for generated code to hand to the map it makes.
  def tabled(entries: Int*): scala.Array[Int]^{} =
    // [construction-fresh] a fresh array no writer can alias
    caps.unsafe.unsafeAssumePure(entries.toArray)

  // The most entries `Map.fixed` lays out: the class it generates carries its layout as a
  // literal, and the JVM limits the size of the method that would build a longer one.
  inline val Limit = 1024

  // `Map.fixed`: the keys are read from the source, a perfect hash of them is found, and the
  // map is generated as a `PerfectMap` whose `index` is that hash, written out. Whatever
  // prevents that is an error where the map is written, never an ordinary map in its place:
  // a map asked to be fixed either is, or does not compile.
  def fixed[key: Type, value: Type](entries: Expr[sci.Seq[(key, value)]])(using Quotes)
  :   Expr[Map[key, value]] =

    import quotes.reflect.*

    // An inline `->` binds its operands to values before it pairs them, so a key is sought
    // through the bindings of whatever was inlined around it.
    def resolve(term: Term, bound: sci.Map[Symbol, Term]): (Term, sci.Map[Symbol, Term]) =
      def binding(bound: sci.Map[Symbol, Term], definition: Definition | Statement)
      :   sci.Map[Symbol, Term] =

        definition match
          case value: ValDef => value.rhs.fold(bound)(bound.updated(value.symbol, _))
          case _             => bound

      term match
        case Inlined(_, bindings, body) => resolve(body, bindings.foldLeft(bound)(binding))
        case Block(statements, body)    => resolve(body, statements.foldLeft(bound)(binding))
        case Typed(body, _)             => resolve(body, bound)

        case reference: Ident if bound.contains(reference.symbol) =>
          resolve(bound(reference.symbol), bound)

        case other =>
          (other, bound)

    // A key's value: a literal, or a literal in the one-argument wrapper a text literal is.
    def constant(term: Term, bound: sci.Map[Symbol, Term]): Any =
      resolve(term, bound) match
        case (Literal(constant), _)                           => constant.value
        case (Apply(_, sci.List(argument)), bound2)           => constant(argument, bound2)
        case (Apply(Apply(_, sci.List(argument)), _), bound2) => constant(argument, bound2)

        case (other, _) =>
          report.errorAndAbort("a key of a fixed map must be a literal", other.pos)

    def keyOf(pair: Expr[(key, value)]): Any =
      resolve(pair.asTerm, sci.Map()) match
        case (Apply(_, sci.List(key, _)), bound) =>
          constant(key, bound)

        case (other, _) =>
          report.errorAndAbort("an entry of a fixed map must be a key and a value", other.pos)

    entries match
      case Varargs(pairs) =>
        val keys = pairs.map(keyOf(_)).toList
        val count = keys.length
        val kinds = keys.map(kind(_)).distinct

        if count == 0 then report.errorAndAbort("a fixed map must have at least one entry")

        if count > Limit then
          val advice = "build it with `Map.build` instead"
          report.errorAndAbort(s"a fixed map can have at most $Limit entries, not $count; $advice")

        if kinds.contains(null) || kinds.length != 1 then
          val kinds = "all be texts, all be `Int`s, all be `Long`s or all be `Char`s"
          report.errorAndAbort(s"the keys of a fixed map must $kinds")

        keys.diff(keys.distinct).headOption.foreach: key =>
          report.errorAndAbort(s"the key $key is given more than once in a fixed map")

        val keyed: Kind = kinds.head.nn
        val words: scala.Array[Long]^ = new scala.Array[Long](count)
        var rest = keys
        var position = 0

        // Bounded by the keys.
        while !rest.isEmpty do
          words(position) = word(rest.head)
          position += 1
          rest = rest.tail

        val hash = PerfectHash.search(words, 1)

        if hash == null then
          val reason = "two texts with the same hash code cannot be separated"
          report.errorAndAbort(s"no perfect hash was found for this fixed map's keys; $reason")
        else
          val positions = sci.List.tabulate(count): index => Expr(hash.slot(words, index))
          val capacity = Expr(hash.capacity)
          val table = sci.List.tabulate(hash.table.length): index => Expr(hash.table(index))

          ' {
              Map.from[key, value]:
                new PerfectMap[key, value]
                  ( slots($capacity, scala.Array[Int](${Varargs(positions)}*), $entries),
                    ${Expr(count)},
                    tabled(${Varargs(table)}*) ):

                  def locate(key: Any, table: scala.Array[Int]): Int =
                    ${indexing(keyed, hash, 'key)('table)}
            }

      case _ =>
        report.errorAndAbort("the entries of a fixed map must be written out where it is made")

// A map whose keys were all known when it was made, read through a perfect hash of them: the
// slot a key would occupy is computed, not searched for, so a read is that computation and
// one comparison, whether or not the key is there, and the table holds no hashes and need be
// no larger than its entries. Slot i holds its key at 2i and its value at 2i + 1; an empty
// slot's key is `null`, and no key is.
//
// The hash is the one abstract member, `locate`, because the class that supplies it is
// generated for one set of keys: by `Map.fixed`, a macro, for keys written in the source, or
// by code staged when the keys become known (murmuration's `optimize`). A subclass rather
// than a function held in a field: a call site which only ever sees one such map can then
// have the hash inlined into its read, where a shared field's call could not be.
//
// Like a table from `Map.build`, it stands behind the opaque `Map` as a standard-library map,
// equal to the others and hashed like them, and its first update that changes anything builds
// the persistent tree.
//
// `displacements` is the table a displacing hash reads, empty for any other. It is held here,
// frozen, and handed to `locate` as an argument. The generated subclass cannot hold it, since
// a field there would be an array the checker could not know to be immutable; nor can its
// code reach a member of this class from inside a splice, which the compiler cannot pickle
// for an anonymous class.
abstract class PerfectMap[key, +value]
  ( slots: scala.Array[Any]^{}, count: Int, displacements: scala.Array[Int]^{} )
extends sci.AbstractMap[key, value]:

  // The hash, which the generated subclass supplies: the slot `key` occupies if it is
  // defined — always a slot of the table, for any value of the kind the keys are — and -1 for
  // a value of any other kind. `table` is the displacement table.
  def locate(key: Any, table: scala.Array[Int]): Int

  final def index(key: Any): Int = locate(key, displacements)

  // [synchronized] the tree is a cache of what the table already determines: racing threads
  // build equal trees, and `@volatile` publishes whichever is kept safely
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var tree: sci.Map[key, value] | Null = null

  private def persistent: sci.Map[key, value] =
    val cached = tree

    if cached != null then cached else
      val built = Hashtable.persistent[key, value](slots, sci.HashMap.newBuilder[key, value])
      tree = built
      built

  // A value of another kind than the keys can still equal one of them, as `1L` equals `1`,
  // so it is looked for the slow way. Bounded by the table.
  private def scan(key: Any): Int =
    var from = 0
    var result = -1

    while result < 0 && from < slots.length do
      val found = slots(from)
      if found != null && key == found then result = from >> 1
      from += 2

    result

  private def slot(key: key): Int =
    val at = index(key)

    if at < 0 then scan(key) else if java.util.Objects.equals(key, slots(at << 1)) then at else -1

  override def size: Int = count
  override def knownSize: Int = count
  override def isEmpty: Boolean = false
  override def contains(key: key): Boolean = slot(key) >= 0

  override def apply(key: key): value =
    val at = slot(key)
    if at < 0 then default(key) else slots((at << 1) + 1).asInstanceOf[value]

  override def getOrElse[value2 >: value](key: key, default: => value2): value2 =
    val at = slot(key)
    if at < 0 then default else slots((at << 1) + 1).asInstanceOf[value]

  def get(key: key): scala.Option[value] =
    val at = slot(key)
    if at < 0 then scala.None else scala.Some(slots((at << 1) + 1).asInstanceOf[value])

  def iterator: scala.collection.Iterator[(key, value)] =
    def occupied(at: Int): Boolean = slots(at << 1) != null

    scala.collection.Iterator.range(0, slots.length >> 1).filter(occupied(_)).map: at =>
      (slots(at << 1).asInstanceOf[key], slots((at << 1) + 1).asInstanceOf[value])

  override def foreachEntry[result](lambda: (key, value) => result): Unit =
    var from = 0

    // Bounded by the table.
    while from < slots.length do
      val key = slots(from)
      if key != null then lambda(key.asInstanceOf[key], slots(from + 1).asInstanceOf[value])
      from += 2

  def removed(key: key): sci.Map[key, value] =
    if slot(key) < 0 then this else persistent.removed(key)

  def updated[value2 >: value](key: key, value: value2): sci.Map[key, value2] =
    persistent.updated(key, value)
