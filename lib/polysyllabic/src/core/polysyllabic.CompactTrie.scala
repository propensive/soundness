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
package polysyllabic

import scala.collection.mutable.ArrayBuffer

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// Dense flat-array hyphenation pattern trie. Each node owns a contiguous
// `Alphabet`-sized slot in `children` indexed by `slot(char)`; `-1` means
// "no child". `values(i)` is the per-gap score array for the pattern that
// ends at node `i`, or `null` if no pattern terminates here. Replacing the
// `gossamer.Dictionary[IArray[Byte]]` walk with this representation cuts
// the per-character cost of the Liang algorithm from a `Map.at` lookup to
// a single int-array index.
//
// Alphabet layout: `a..z` → `0..25`, `.` → `26`, anything else → `-1`
// (the algorithm treats such characters as a dead-end, which matches the
// English-only patterns it ships with).
private[polysyllabic] final class CompactTrie
    ( val children: Array[Int],
      val values:   Array[IArray[Byte] | Null],
      val depth:    Array[Int],
      val fail:     Array[Int],
      val dictLink: Array[Int] )

private[polysyllabic] object CompactTrie:
  inline val Alphabet: 27 = 27

  inline def slot(char: Char): Int =
    if char >= 'a' && char <= 'z' then char - 'a'
    else if char == '.' then 26
    else -1

  // Build a CompactTrie by walking a gossamer.Dictionary. Each `Just`
  // node's path-compressed tail is expanded back into a chain of one node
  // per character, because the flat layout has no per-node "skip these N
  // characters" encoding.
  def from(dict: Dictionary[IArray[Byte]]): CompactTrie =
    val childrenBuf: ArrayBuffer[Int]                  = ArrayBuffer()
    val valuesBuf:   ArrayBuffer[IArray[Byte] | Null]  = ArrayBuffer()

    def addNode(value: IArray[Byte] | Null): Int =
      val id = valuesBuf.length
      var k = 0
      while k < Alphabet do
        childrenBuf += -1
        k += 1
      valuesBuf += value
      id

    def setChild(parent: Int, char: Char, child: Int): Unit =
      val sl = slot(char)
      if sl >= 0 then childrenBuf(parent*Alphabet + sl) = child

    val rootId = addNode(null)

    def walk(node: Dictionary[IArray[Byte]], myId: Int): Unit = node match
      case Dictionary.Empty => ()

      case branch: Dictionary.Branch[IArray[Byte]] @unchecked =>
        branch.value.let(v => valuesBuf(myId) = v)
        branch.map.foreach: (char, child) =>
          val childId = addNode(null)
          setChild(myId, char, childId)
          walk(child, childId)

      case just: Dictionary.Just[IArray[Byte]] @unchecked =>
        val text = just.text.s
        val offset = just.offset
        val value = just.value
        val tailLen = text.length - offset

        if tailLen == 0 then
          // The Just is already at its leaf (offset == text.length). Its
          // value belongs on `myId` itself — not on a child node, because
          // there are no further characters to consume to reach it. This
          // case arises when `Dictionary.add` splits a Just on a sibling
          // path and the original tail terminates exactly at the split
          // point.
          valuesBuf(myId) = value
        else
          var prevId = myId
          var k = 0

          while k < tailLen do
            val char = text.charAt(offset + k)
            val isLast = (k == tailLen - 1)
            val v: IArray[Byte] | Null = if isLast then value else null
            val childId = addNode(v)
            setChild(prevId, char, childId)
            prevId = childId
            k += 1

    walk(dict, rootId)

    val nodeCount = valuesBuf.length
    val childrenArr = new Array[Int](nodeCount*Alphabet)
    var i = 0
    while i < childrenBuf.length do
      childrenArr(i) = childrenBuf(i)
      i += 1

    val valuesArr = new Array[IArray[Byte] | Null](nodeCount)
    i = 0
    while i < valuesBuf.length do
      valuesArr(i) = valuesBuf(i)
      i += 1

    // Build Aho-Corasick failure and dictionary-suffix links via BFS from
    // the root. `depth(n)` is the path length from root to `n`; `fail(n)`
    // is the longest proper suffix of `n`'s path that is itself a path
    // from root; `dictLink(n)` is the nearest ancestor along the fail
    // chain that carries a value (or 0 if none).
    val depthArr    = new Array[Int](nodeCount)
    val failArr     = new Array[Int](nodeCount)
    val dictLinkArr = new Array[Int](nodeCount)
    val queue       = new Array[Int](nodeCount)
    var qHead = 0
    var qTail = 0

    // Depth-1 nodes have failure link = root (0).
    var c = 0
    while c < Alphabet do
      val child = childrenArr(c)
      if child >= 0 then
        depthArr(child) = 1
        failArr(child) = 0
        dictLinkArr(child) = 0
        queue(qTail) = child
        qTail += 1
      c += 1

    while qHead < qTail do
      val n = queue(qHead)
      qHead += 1
      val nd = depthArr(n)
      val nf = failArr(n)

      var sl = 0
      while sl < Alphabet do
        val child = childrenArr(n*Alphabet + sl)
        if child >= 0 then
          depthArr(child) = nd + 1
          // Find fail target: follow fail chain from nf until a node has
          // child via this slot, or we reach root.
          var f = nf
          while f != 0 && childrenArr(f*Alphabet + sl) < 0 do f = failArr(f)
          val fChild = childrenArr(f*Alphabet + sl)
          failArr(child) =
            if fChild >= 0 && fChild != child then fChild else 0
          // Dictionary suffix link: closest fail-chain ancestor with value.
          val fl = failArr(child)
          dictLinkArr(child) = if valuesArr(fl) != null then fl else dictLinkArr(fl)
          queue(qTail) = child
          qTail += 1
        sl += 1

    new CompactTrie(childrenArr, valuesArr, depthArr, failArr, dictLinkArr)
