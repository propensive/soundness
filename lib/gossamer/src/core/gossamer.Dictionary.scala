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
package gossamer

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.{ArrayBuffer, HashMap as MutMap, LinkedHashSet}

import anticipation.*
import vacuous.*

// Dense flat-array trie keyed on chars from an `Alphabet`. Each node owns a
// contiguous slot of `alphabet.size` `Int` entries in `children` indexed by
// `alphabet.slot(char)`; `-1` means "no child". `values(i)` is the value
// associated with the pattern that terminates at node `i`, or `null` if no
// pattern terminates here.
//
// Each character step is a single int-array index; each value access is a
// single null check. Both are substantially cheaper than walking a
// case-class trie with `Map.at(char)` HashMap lookups, at the cost of
// materialising the trie into a dense form up-front.
//
// `value` is expected to be a reference type at runtime; the internal
// storage allocates `new Array[value](n)` via `ClassTag` and re-views it
// as `Array[value | Null]` so callers can null-check at access sites.
// Storing a primitive `value` (e.g. `Int`) would box at runtime — fine
// for occasional access, wasteful in hot loops.
//
// `Dictionary.aho` populates `depth`, `fail`, and `dictLink` for an
// Aho-Corasick walk; the default builders leave them empty so callers
// that only do exact-key lookups don't pay the BFS build cost.
object Dictionary:
  object Alphabet:
    // Build an Alphabet from a string of supported chars. Slot index of
    // each char is its position in the string. Unsupported chars return
    // `-1`. Restricted to ASCII (char codes 0..127); higher code points
    // return `-1`.
    def of(chars: String): Alphabet = new Alphabet:
      private val table = Array.fill[Int](128)(-1)
      private val charTable: Array[Char] = chars.toCharArray.nn
      private val n = chars.length

      locally:
        var i = 0

        while i < n do
          val c = chars.charAt(i).toInt
          if c < 128 then table(c) = i
          i += 1

      def size = n

      def slot(char: Char): Int =
        val c = char.toInt
        if c < 128 then table(c) else -1

      def char(slot: Int): Char = charTable(slot)

    // An empty alphabet — no chars are recognised. Used by the empty
    // dictionary.
    val empty: Alphabet = new Alphabet:
      def size = 0
      def slot(char: Char): Int = -1
      def char(slot: Int): Char = throw IndexOutOfBoundsException(slot.toString)

  // A small alphabet for `Dictionary`: a fixed mapping from char to a
  // dense slot index in `[0, size)`. Chars not in the alphabet return
  // `-1`. Callers supply different alphabets for different uses (a-z + `.`
  // for hyphenation, a-z + 0-9 + `-` for HTML attributes, etc.). The
  // default builders (`Dictionary(pairs*)`, `Dictionary.empty`,
  // `Dictionary.aho(pairs*)`) auto-derive an alphabet from the keys'
  // distinct characters, so most callers never construct one explicitly.
  trait Alphabet:
    def slot(char: Char): Int
    def char(slot: Int): Char
    def size: Int

  // An empty Dictionary with no entries and an empty alphabet. Adds via
  // `+`/`++` rebuild the trie with an alphabet derived from the keys.
  def empty[value: ClassTag]: Dictionary[value] =
    val emptyInts = new Array[Int](0)

    val emptyValues: Array[AnyRef | Null] =
      new Array[AnyRef](0).asInstanceOf[Array[AnyRef | Null]]

    new Dictionary[value]
      ( emptyInts, emptyValues, emptyInts, emptyInts, emptyInts, Alphabet.empty, summon )

  // Build a Dictionary from `(key -> value)` pairs. The alphabet is
  // auto-derived from the distinct characters appearing in the keys, in
  // first-encountered order.
  def apply[value: ClassTag](pairs: (Text, value)*): Dictionary[value] =
    build(pairs, autoAlphabet(pairs), ahoCorasick = false)

  // Build a Dictionary from pairs with an explicit alphabet. Keys
  // containing characters not in the alphabet are silently dropped from
  // the trie (they would be unreachable via `step` regardless).
  def withAlphabet[value: ClassTag]
    ( alphabet: Alphabet, pairs: (Text, value)* )
  :   Dictionary[value] =

    build(pairs, alphabet, ahoCorasick = false)

  // Build a Dictionary configured for an Aho-Corasick walk. The trie
  // additionally carries `depth`, `fail`, and `dictLink` arrays so a
  // single forward pass through input chars can find every key that
  // terminates at each position.
  def aho[value: ClassTag]
    ( alphabet: Alphabet, pairs: (Text, value)* )
  :   Dictionary[value] =

    build(pairs, alphabet, ahoCorasick = true)

  // Internal: derive an alphabet from the union of chars in the supplied
  // keys. Insertion order is preserved so the slot mapping is stable
  // across runs with the same input.
  private def autoAlphabet[value](pairs: Iterable[(Text, value)]): Alphabet =
    val chars = LinkedHashSet[Char]()

    pairs.foreach: (key, _) =>
      val s = key.s
      var i = 0

      while i < s.length do
        chars += s.charAt(i)
        i += 1

    Alphabet.of(chars.mkString)

  // Internal: assemble flat children/values arrays from a sequence of
  // (key, value) pairs, then (optionally) compute Aho-Corasick failure
  // and dictionary-suffix links via BFS.
  private def build[value: ClassTag]
    ( pairs:       Iterable[(Text, value)],
      alphabet:    Alphabet,
      ahoCorasick: Boolean )
  :   Dictionary[value] =

    // Temporary mutable tree, flattened once the structure is known.
    // `value` is held as `AnyRef | Null` even when the user's `value` type
    // is a primitive — Scala auto-boxes on assignment, which lets the same
    // mutable cell carry either reference or primitive values.
    final class NodeBuilder:
      val children = MutMap[Char, NodeBuilder]()
      var value: AnyRef | Null = null

    val root = new NodeBuilder
    val alpha = alphabet.size

    pairs.foreach: (key, v) =>
      var node = root
      val s = key.s
      var i = 0
      var live = true

      while live && i < s.length do
        val c = s.charAt(i)

        if alphabet.slot(c) < 0 then live = false
        else
          node = node.children.getOrElseUpdate(c, new NodeBuilder)
          i += 1

      if live then node.value = v.asInstanceOf[AnyRef]

    // BFS assignment of node ids gives root = 0, then nodes at depth 1,
    // then depth 2, etc. — convenient when the AC build needs to access
    // failure links of shallower nodes from deeper ones.
    val nodeList = ArrayBuffer[NodeBuilder](root)
    var head = 0

    while head < nodeList.length do
      val n = nodeList(head)
      head += 1
      var sl = 0

      while sl < alpha do
        val c = alphabet.char(sl)
        n.children.get(c).foreach(nodeList += _)
        sl += 1

    val nodeCount = nodeList.length
    val ids = MutMap[NodeBuilder, Int]()
    var i = 0

    while i < nodeCount do
      ids(nodeList(i)) = i
      i += 1

    val childrenArr = Array.fill[Int](nodeCount*alpha)(-1)

    val valuesArr: Array[AnyRef | Null] =
      new Array[AnyRef](nodeCount).asInstanceOf[Array[AnyRef | Null]]

    i = 0

    while i < nodeCount do
      val n = nodeList(i)
      valuesArr(i) = n.value
      var sl = 0

      while sl < alpha do
        val c = alphabet.char(sl)

        n.children.get(c).foreach: child =>
          childrenArr(i*alpha + sl) = ids(child)

        sl += 1

      i += 1

    if !ahoCorasick then
      val emptyInts = new Array[Int](0)

      new Dictionary[value]
        ( childrenArr, valuesArr, emptyInts, emptyInts, emptyInts, alphabet, summon )
    else
      // Aho-Corasick failure / dictionary-suffix links via BFS. Depth-1
      // nodes get fail = 0; deeper nodes get the longest proper suffix
      // of their path that is itself a path from root. `dictLink` skips
      // fail-chain ancestors that have no value.
      val depthArr    = new Array[Int](nodeCount)
      val failArr     = new Array[Int](nodeCount)
      val dictLinkArr = new Array[Int](nodeCount)
      val queue       = new Array[Int](nodeCount)
      var qHead = 0
      var qTail = 0
      var c = 0

      while c < alpha do
        val child = childrenArr(c)

        if child >= 0 then
          depthArr(child) = 1
          failArr(child) = 0
          dictLinkArr(child) = 0
          queue(qTail) = child
          qTail += 1

        c += 1

      while qHead < qTail do
        val node = queue(qHead)
        qHead += 1
        val nd = depthArr(node)
        val nf = failArr(node)
        var sl = 0

        while sl < alpha do
          val child = childrenArr(node*alpha + sl)

          if child >= 0 then
            depthArr(child) = nd + 1
            var f = nf
            while f != 0 && childrenArr(f*alpha + sl) < 0 do f = failArr(f)
            val fChild = childrenArr(f*alpha + sl)
            failArr(child) = if fChild >= 0 && fChild != child then fChild else 0
            val fl = failArr(child)
            dictLinkArr(child) = if valuesArr(fl) != null then fl else dictLinkArr(fl)
            queue(qTail) = child
            qTail += 1

          sl += 1

      new Dictionary[value]
        ( childrenArr, valuesArr, depthArr, failArr, dictLinkArr, alphabet, summon )

final class Dictionary[+value]
  ( val children: Array[Int],
    val values:   Array[AnyRef | Null],
    val depth:    Array[Int],
    val fail:     Array[Int],
    val dictLink: Array[Int],
    val alphabet: Dictionary.Alphabet,
    classTag:     ClassTag[value @uncheckedVariance] ):

  // The exposed `values` array is `Array[AnyRef | Null]` rather than
  // `Array[value | Null]` because Array is invariant and the JVM checks
  // the runtime array type on every field access — exposing a typed view
  // would cost a `ClassCastException` on the first reference. Callers
  // who want the typed view use `value(node)`, which casts at the access
  // site (which the JIT typically elides).

  // Re-expose the `ClassTag` to the rebuild path so `+`/`++` can allocate
  // their fresh values array without the caller having to summon one.
  private[gossamer] given valueTag: ClassTag[value @uncheckedVariance] = classTag

  inline def root: Int = 0

  // Walk one character. Returns the next node id or `-1` if the alphabet
  // doesn't include `char` or no key in the trie continues with that
  // character at `node`.
  def step(node: Int, char: Char): Int =
    val sl = alphabet.slot(char)
    if sl < 0 then -1 else children(node*alphabet.size + sl)

  // Value at `node`, or `null` if no key terminates here.
  inline def value(node: Int): value | Null = values(node).asInstanceOf[value | Null]

  // Exact lookup of a full key. `Unset` if the key is not in the trie.
  def apply(key: Text): Optional[value] =
    val s = key.s
    val n = s.length
    var node = 0
    var i = 0

    while i < n && node >= 0 do
      node = step(node, s.charAt(i))
      i += 1

    if node < 0 then Unset else
      val v = values(node)
      if v == null then Unset else v.asInstanceOf[value]

  // Slice variant: lookup against `buffer[offset, offset + length)`
  // without allocating a `Text`. Useful in hot loops that already hold a
  // char buffer (e.g. parser word boundaries).
  def apply(buffer: Array[Char], offset: Int, length: Int): Optional[value] =
    var node = 0
    var i = 0

    while i < length && node >= 0 do
      node = step(node, buffer(offset + i))
      i += 1

    if node < 0 then Unset else
      val v = values(node)
      if v == null then Unset else v.asInstanceOf[value]

  // Number of stored entries (nodes whose value is non-null).
  def size: Int =
    var count = 0
    var i = 0

    while i < values.length do
      if values(i) != null then count += 1
      i += 1

    count

  // Iterate over stored values.
  def iterator: Iterable[value] =
    val buffer = ArrayBuffer[value]()
    var i = 0

    while i < values.length do
      val v = values(i)
      if v != null then buffer += v.asInstanceOf[value]
      i += 1

    buffer

  // Iterate over `(key, value)` pairs by walking the trie in alphabet
  // order. Allocates an `ArrayBuffer` and a `StringBuilder` shared
  // across recursive descents.
  def entries: Iterable[(Text, value)] =
    val buffer = ArrayBuffer[(Text, value)]()
    val key = new java.lang.StringBuilder
    val alpha = alphabet.size

    def walk(node: Int): Unit =
      val v = values(node)
      if v != null then buffer += ((key.toString.nn.tt, v.asInstanceOf[value]))
      var sl = 0

      while sl < alpha do
        val child = children(node*alpha + sl)

        if child >= 0 then
          key.append(alphabet.char(sl))
          walk(child)
          key.setLength(key.length - 1)

        sl += 1

    if values.length > 0 then walk(0)
    buffer

  // Add an entry, returning a new Dictionary. Widens the value type to
  // include the new entry's value type via the standard `[v2 >: value]`
  // bound (`+`/`++` follow the same pattern).
  def add[value2 >: value: ClassTag](key: Text, value: value2): Dictionary[value2] =
    this + (key -> value)

  def + [value2 >: value: ClassTag](entry: (Text, value2)): Dictionary[value2] =
    this ++ Seq(entry)

  def ++ [value2 >: value: ClassTag](extras: Iterable[(Text, value2)]): Dictionary[value2] =
    val combined = entries.asInstanceOf[Iterable[(Text, value2)]] ++ extras
    Dictionary[value2](combined.toSeq*)
