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

import scala.collection.mutable.ArrayBuffer

import anticipation.*
import vacuous.*

// Dense flat-array trie keyed on chars from an `Alphabet`. Each node owns a
// contiguous slot of `alphabet.size` `Int` entries in `children` indexed by
// `alphabet.slot(char)`; `-1` means "no child". `values(i)` is the value
// associated with the pattern that terminates at node `i`, or `null` if no
// pattern terminates here.
//
// `ahoCorasick = true` populates `depth`, `fail`, and `dictLink` for an
// Aho-Corasick walk; the default leaves them empty so the build only pays
// for what callers will use.
//
// Lookups against the trie are a single int-array index per character;
// pattern matches on the node value are a single null check. Both are
// substantially cheaper than walking `gossamer.Dictionary`'s case-class
// tree with `Map.at(char)` HashMap lookups, at the cost of materialising
// the trie into a separate dense form up-front.
//
// `value` is expected to be a reference type at runtime; the internal
// storage uses an `Array[value]` allocated via `ClassTag` and re-viewed as
// `Array[value | Null]` so callers can `null`-check at access sites.
// Storing a primitive `value` (e.g. `Int`) would box at runtime — fine for
// occasional access, wasteful in hot loops.
object CompactTrie:
  // A small alphabet for `CompactTrie`: a fixed mapping from char to a dense
  // slot index in `[0, size)`. Chars not in the alphabet return `-1`.
  // Callers supply different alphabets for different uses (a-z + `.` for
  // hyphenation, a-z + 0-9 + `-` for HTML attributes, etc.).
  trait Alphabet:
    def slot(char: Char): Int
    def size: Int

  object Alphabet:
    // Build an Alphabet from a string of supported chars. Slot index of
    // each char is its position in the string. Unsupported chars return
    // `-1`. Restricted to ASCII (char codes 0..127); higher code points
    // return `-1`.
    def of(chars: String): Alphabet = new Alphabet:
      private val table = Array.fill[Int](128)(-1)
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

  // Build a `CompactTrie[value]` from a `gossamer.Dictionary[value]`. If
  // `ahoCorasick` is `true`, also populate failure and dictionary-suffix
  // links via a BFS — pay for this only when the caller will use it (e.g.
  // hyphenation, which walks every starting position; one-shot exact
  // lookups do not need it).
  def from[value: ClassTag]
    ( dict:        Dictionary[value],
      alphabet:    Alphabet,
      ahoCorasick: Boolean = false )
  :   CompactTrie[value] =

    val alpha = alphabet.size
    val childrenBuf: ArrayBuffer[Int]               = ArrayBuffer()
    val valuesBuf:   ArrayBuffer[value | Null]      = ArrayBuffer()

    def addNode(v: value | Null): Int =
      val id = valuesBuf.length
      var k = 0

      while k < alpha do
        childrenBuf += -1
        k += 1

      valuesBuf += v
      id

    def setChild(parent: Int, char: Char, child: Int): Unit =
      val sl = alphabet.slot(char)
      if sl >= 0 then childrenBuf(parent*alpha + sl) = child

    val rootId = addNode(null)

    def walk(node: Dictionary[value], myId: Int): Unit = node match
      case Dictionary.Empty => ()

      case branch: Dictionary.Branch[value] @unchecked =>
        branch.value.let: v =>
          valuesBuf(myId) = v

        branch.map.foreach: (char, child) =>
          val childId = addNode(null)
          setChild(myId, char, childId)
          walk(child, childId)

      case just: Dictionary.Just[value] @unchecked =>
        val text = just.text.s
        val offset = just.offset
        val v = just.value
        val tailLen = text.length - offset

        if tailLen == 0 then
          // The Just is already at its leaf (offset == text.length). Its
          // value belongs on `myId` itself — not on a child node, because
          // there are no further characters to consume to reach it. This
          // case arises when `Dictionary.add` splits a Just on a sibling
          // path and the original tail terminates exactly at the split
          // point.
          valuesBuf(myId) = v
        else
          var prevId = myId
          var k = 0

          while k < tailLen do
            val char = text.charAt(offset + k)
            val isLast = (k == tailLen - 1)
            val nodeValue: value | Null = if isLast then v else null
            val childId = addNode(nodeValue)
            setChild(prevId, char, childId)
            prevId = childId
            k += 1

    walk(dict, rootId)

    val nodeCount = valuesBuf.length
    val childrenArr = new Array[Int](nodeCount*alpha)
    var i = 0

    while i < childrenBuf.length do
      childrenArr(i) = childrenBuf(i)
      i += 1

    val valuesArr = new Array[value](nodeCount).asInstanceOf[Array[value | Null]]
    i = 0

    while i < valuesBuf.length do
      valuesArr(i) = valuesBuf(i)
      i += 1

    if !ahoCorasick then
      val emptyInts = new Array[Int](0)

      new CompactTrie[value]
        ( childrenArr, valuesArr, emptyInts, emptyInts, emptyInts, alphabet )
    else
      // BFS to compute Aho-Corasick failure links and dictionary-suffix
      // links. Depth-1 nodes get `fail = 0` (root); deeper nodes get the
      // longest proper suffix of their path that is itself a path from
      // root. `dictLink` skips fail-chain ancestors that have no value.
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

      new CompactTrie[value]
        ( childrenArr, valuesArr, depthArr, failArr, dictLinkArr, alphabet )

final class CompactTrie[value]
  ( val children: Array[Int],
    val values:   Array[value | Null],
    val depth:    Array[Int],
    val fail:     Array[Int],
    val dictLink: Array[Int],
    val alphabet: CompactTrie.Alphabet ):

  inline def root: Int = 0

  // Walk one character. Returns the next node id or `-1` if the alphabet
  // doesn't include `char` or no pattern in the trie continues with that
  // character at `node`.
  def step(node: Int, char: Char): Int =
    val sl = alphabet.slot(char)
    if sl < 0 then -1 else children(node*alphabet.size + sl)

  // Value at `node`, or `null` if no pattern terminates here.
  inline def value(node: Int): value | Null = values(node)

  // Exact lookup of a full key. `null` if the key is not in the trie.
  def apply(key: Text): value | Null =
    val s = key.s
    val n = s.length
    var node = 0
    var i = 0

    while i < n && node >= 0 do
      node = step(node, s.charAt(i))
      i += 1

    if node < 0 then null else values(node)
