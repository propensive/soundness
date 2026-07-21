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
package pneumatic

import Flate.*
import FlateTables.*

// A streaming DEFLATE decompressor, ported faithfully from JZlib (BSD 3-clause, Copyright (c)
// 2000-2011 ymnk, JCraft, Inc.), itself a port of zlib 1.1.3 by Jean-loup Gailly and Mark Adler.
// The port preserves JZlib's incremental structure — bounded-memory streaming over a sliding
// window, resumable whenever input or output space runs out — with the fall-through `switch`
// state machines rendered as dispatch loops over a mode variable (every fall-through in the
// original assigns the successor mode first, so re-dispatching is equivalent). Only the zlib and
// raw framings are supported (`nowrap`); the gzip framing is handled by `Inflation` above this.
//
// Huffman decoding tables, built by `huftBuild` (zlib's `huft_build`): each entry is a triple
// (operation, bits, value) flattened into an `Array[Int]`.
private[pneumatic] final class InfTree:
  private final val Many = 1440
  private final val Bmax = 15

  private val hn = new Array[Int](1)   // hufts used in space
  private var v = new Array[Int](288)  // work area for huftBuild
  private val c = new Array[Int](Bmax + 1) // bit length count table
  private val r = new Array[Int](3)    // table entry for structure assignment
  private val u = new Array[Int](Bmax) // table stack
  private val x = new Array[Int](Bmax + 1) // bit offsets, then code stack

  private def initWorkArea(vsize: Int): Unit =
    if v.length < vsize then v = new Array[Int](vsize)
    var i = 0
    while i < vsize do { v(i) = 0; i += 1 }
    i = 0
    while i < Bmax + 1 do { c(i) = 0; i += 1 }
    i = 0
    while i < 3 do { r(i) = 0; i += 1 }
    i = 0
    while i < Bmax do { u(i) = 0; i += 1 }
    i = 0
    while i < Bmax + 1 do { x(i) = 0; i += 1 }

  // Given a list of code lengths and a maximum table size, make a set of tables to decode that
  // set of codes. Returns `ZOk` on success, `ZBufError` if the given code set is incomplete (the
  // tables are still built in this case), or `ZDataError` if the input is invalid.
  private def huftBuild
    ( b: Array[Int], bindex: Int, n0: Int, s: Int, d: Array[Int], e: Array[Int],
      t: Array[Int], m: Array[Int], hp: Array[Int] )
  :   Int =

    var n = n0
    var a = 0     // counter for codes of length k
    var f = 0     // i repeats in table every f entries
    var g = 0     // maximum code length
    var h = 0     // table level
    var i = 0     // counter, current code
    var j = 0     // counter
    var k = 0     // number of bits in current code
    var l = 0     // bits per table (returned in m)
    var mask = 0  // (1 << w) - 1
    var p = 0     // pointer into c, b, or v
    var q = 0     // points to current table
    var w = 0     // bits before this table == (l*h)
    var xp = 0    // pointer into x
    var y = 0     // number of dummy codes added
    var z = 0     // number of entries in current table

    // Generate counts for each bit length
    p = 0
    i = n
    while { c(b(bindex + p)) += 1; p += 1; i -= 1; i != 0 } do ()

    if c(0) == n then // null input: all zero-length codes
      t(0) = -1
      m(0) = 0
      ZOk
    else
      // Find minimum and maximum length, bound m(0) by those
      l = m(0)
      j = 1
      while j <= Bmax && c(j) == 0 do j += 1
      k = j // minimum code length
      if l < j then l = j
      i = Bmax
      while i != 0 && c(i) == 0 do i -= 1
      g = i // maximum code length
      if l > i then l = i
      m(0) = l

      // Adjust last length count to fill out codes, if needed
      var bad = false
      y = 1 << j

      while j < i && !bad do
        y -= c(j)

        if y < 0 then bad = true
        else
          j += 1
          y <<= 1

      if !bad then
        y -= c(i)
        if y < 0 then bad = true else c(i) += y

      if bad then ZDataError else
        // Generate starting offsets into the value table for each length
        x(1) = 0
        j = 0
        p = 1
        xp = 2

        while { i -= 1; i != 0 } do // note that i == g from above
          j += c(p)
          x(xp) = j
          xp += 1
          p += 1

        // Make a table of values in order of bit lengths
        i = 0
        p = 0

        while
          j = b(bindex + p)
          if j != 0 then { v(x(j)) = i; x(j) += 1 }
          p += 1
          i += 1
          i < n
        do ()

        n = x(g) // set n to length of v

        // Generate the Huffman codes and for each, make the table entries
        x(0) = 0
        i = 0     // first Huffman code is zero
        p = 0     // grab values in bit order
        h = -1    // no tables yet — level -1
        w = -l    // bits decoded == (l*h)
        u(0) = 0
        q = 0
        z = 0

        var overflow = false

        // go through the bit lengths (k already is bits in shortest code)
        while k <= g && !overflow do
          a = c(k)

          while a != 0 && !overflow do
            a -= 1
            // here i is the Huffman code of length k bits for value v(p); make tables up to
            // required level
            while k > w + l && !overflow do
              h += 1
              w += l // previous table always l bits

              // compute minimum size table less than or equal to l bits
              z = g - w
              z = if z > l then l else z
              j = k - w
              f = 1 << j

              if f > a + 1 then // try a k-w bit table: too few codes for k-w bit table
                f -= a + 1 // deduct codes from patterns left
                xp = k

                if j < z then
                  var go = true

                  while go && { j += 1; j < z } do // try smaller tables up to z bits
                    f <<= 1
                    xp += 1

                    if f <= c(xp) then go = false // enough codes to use up j bits
                    else f -= c(xp) // else deduct codes from patterns

              z = 1 << j // table entries for j-bit table

              // allocate new table
              if hn(0) + z > Many then overflow = true // overflow of Many
              else
                q = hn(0)
                u(h) = q
                hn(0) += z

                // connect to last table, if there is one
                if h != 0 then
                  x(h) = i // save pattern for backing up
                  r(0) = j
                  r(1) = l
                  j = i >>> (w - l)
                  r(2) = q - u(h - 1) - j
                  System.arraycopy(r, 0, hp, (u(h - 1) + j)*3, 3) // connect to last table
                else
                  t(0) = q // first table is returned result

            if !overflow then
              // set up table entry in r
              r(1) = k - w

              if p >= n then r(0) = 128 + 64 // out of values — invalid code
              else if v(p) < s then
                r(0) = if v(p) < 256 then 0 else 32 + 64 // 256 is end-of-block
                r(2) = v(p) // simple code is just the value
                p += 1
              else
                r(0) = e(v(p) - s) + 16 + 64 // non-simple: look up in lists
                r(2) = d(v(p) - s)
                p += 1

              // fill code-like entries with r
              f = 1 << (k - w)
              j = i >>> w

              while j < z do
                System.arraycopy(r, 0, hp, (q + j)*3, 3)
                j += f

              // backwards increment the k-bit code i
              j = 1 << (k - 1)

              while (i & j) != 0 do
                i ^= j
                j >>>= 1

              i ^= j

              // backup over finished tables
              mask = (1 << w) - 1

              while (i & mask) != x(h) do
                h -= 1
                w -= l
                mask = (1 << w) - 1

          k += 1

        if overflow then ZDataError
        else if y != 0 && g != 1 then ZBufError // incomplete table
        else ZOk

  def inflateTreesBits(c0: Array[Int], bb: Array[Int], tb: Array[Int], hp: Array[Int], z: Inflater)
  :   Int =

    initWorkArea(19)
    hn(0) = 0
    var result = huftBuild(c0, 0, 19, 19, emptyInts, emptyInts, tb, bb, hp)

    if result == ZDataError then z.msg = "oversubscribed dynamic bit lengths tree"
    else if result == ZBufError || bb(0) == 0 then
      z.msg = "incomplete dynamic bit lengths tree"
      result = ZDataError

    result

  def inflateTreesDynamic
    ( nl: Int, nd: Int, c0: Array[Int], bl: Array[Int], bd: Array[Int], tl: Array[Int],
      td: Array[Int], hp: Array[Int], z: Inflater )
  :   Int =

    // build literal/length tree
    initWorkArea(288)
    hn(0) = 0
    var result = huftBuild(c0, 0, nl, 257, cplens, cplext, tl, bl, hp)

    if result != ZOk || bl(0) == 0 then
      if result == ZDataError then z.msg = "oversubscribed literal/length tree"
      else if result != ZMemError then
        z.msg = "incomplete literal/length tree"
        result = ZDataError

      result
    else
      // build distance tree
      initWorkArea(288)
      result = huftBuild(c0, nl, nd, 0, cpdist, cpdext, td, bd, hp)

      if result != ZOk || (bd(0) == 0 && nl > 257) then
        if result == ZDataError then z.msg = "oversubscribed distance tree"
        else if result == ZBufError then
          z.msg = "incomplete distance tree"
          result = ZDataError
        else if result != ZMemError then
          z.msg = "empty distance tree with lengths"
          result = ZDataError

        result
      else
        ZOk

private[pneumatic] object InfCodes:
  // waiting for "i:"=input, "o:"=output, "x:"=nothing
  final val Start = 0    // x: set up for Len
  final val Len = 1      // i: get length/literal/eob next
  final val Lenext = 2   // i: getting length extra (have base)
  final val Dist = 3     // i: get distance next
  final val Distext = 4  // i: getting distance extra
  final val Copy = 5     // o: copying bytes in window, waiting for space
  final val Lit = 6      // o: got literal, waiting for output space
  final val Wash = 7     // o: got eob, possibly still output waiting
  final val End = 8      // x: got eob and all data flushed
  final val Badcode = 9  // x: got error

private[pneumatic] final class InfCodes(z: Inflater, s: InfBlocks):
  import InfCodes.*

  var mode: Int = 0 // current inflate_codes mode

  var len: Int = 0
  var tree: Array[Int] = emptyInts // pointer into tree
  var treeIndex: Int = 0
  var need: Int = 0 // bits needed
  var lit: Int = 0
  var get: Int = 0  // bits to get for extra
  var dist: Int = 0 // distance back to copy from

  var lbits: Int = 0 // ltree bits decoded per branch
  var dbits: Int = 0 // dtree bits decoded per branch
  var ltree: Array[Int] = emptyInts
  var ltreeIndex: Int = 0
  var dtree: Array[Int] = emptyInts
  var dtreeIndex: Int = 0

  def init(bl: Int, bd: Int, tl: Array[Int], tlIndex: Int, td: Array[Int], tdIndex: Int): Unit =
    mode = Start
    lbits = bl
    dbits = bd
    ltree = tl
    ltreeIndex = tlIndex
    dtree = td
    dtreeIndex = tdIndex
    tree = emptyInts

  def proc(r0: Int): Int =
    var r = r0
    var j = 0      // temporary storage
    var tindex = 0 // temporary pointer
    var e = 0      // extra bits or operation
    var b = 0      // bit buffer
    var k = 0      // bits in bit buffer
    var p = 0      // input data pointer
    var n = 0      // bytes available there
    var q = 0      // output window write pointer
    var m = 0      // bytes to end of window or read pointer
    var f = 0      // pointer to copy strings from

    // copy input/output information to locals; `leave` restores it and flushes the window
    val input = z.nextIn
    val win = s.window

    p = z.nextInIndex; n = z.availIn; b = s.bitb; k = s.bitk
    q = s.write; m = if q < s.read then s.read - q - 1 else s.end - q

    // Passing the loop state as parameters (rather than capturing the `var`s) keeps the hot
    // locals unboxed: captured mutable locals would otherwise be compiled to heap references.
    def leave(status: Int, b: Int, k: Int, p: Int, n: Int, q: Int): Int =
      s.bitb = b; s.bitk = k
      z.availIn = n; z.totalIn += p - z.nextInIndex; z.nextInIndex = p
      s.write = q
      s.inflateFlush(status)

    // process input and output based on current state
    while true do
      mode match
        case Start => // x: set up for Len
          if m >= 258 && n >= 10 then
            s.bitb = b; s.bitk = k
            z.availIn = n; z.totalIn += p - z.nextInIndex; z.nextInIndex = p
            s.write = q
            r = inflateFast(lbits, dbits, ltree, ltreeIndex, dtree, dtreeIndex)

            p = z.nextInIndex; n = z.availIn; b = s.bitb; k = s.bitk
            q = s.write; m = if q < s.read then s.read - q - 1 else s.end - q

            if r != ZOk then mode = if r == ZStreamEnd then Wash else Badcode

          if mode == Start then
            need = lbits
            tree = ltree
            treeIndex = ltreeIndex
            mode = Len

        case Len => // i: get length/literal/eob next
          j = need

          while k < j do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (input(p) & 0xff) << k
            p += 1
            k += 8

          tindex = (treeIndex + (b & inflateMask(j)))*3
          b >>>= tree(tindex + 1)
          k -= tree(tindex + 1)
          e = tree(tindex)

          if e == 0 then // literal
            lit = tree(tindex + 2)
            mode = Lit
          else if (e & 16) != 0 then // length
            get = e & 15
            len = tree(tindex + 2)
            mode = Lenext
          else if (e & 64) == 0 then // next table
            need = e
            treeIndex = tindex/3 + tree(tindex + 2)
          else if (e & 32) != 0 then mode = Wash // end of block
          else
            mode = Badcode // invalid code
            z.msg = "invalid literal/length code"
            r = ZDataError
            return leave(r, b, k, p, n, q)

        case Lenext => // i: getting length extra (have base)
          j = get

          while k < j do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (input(p) & 0xff) << k
            p += 1
            k += 8

          len += b & inflateMask(j)
          b >>= j
          k -= j

          need = dbits
          tree = dtree
          treeIndex = dtreeIndex
          mode = Dist

        case Dist => // i: get distance next
          j = need

          while k < j do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (input(p) & 0xff) << k
            p += 1
            k += 8

          tindex = (treeIndex + (b & inflateMask(j)))*3
          b >>= tree(tindex + 1)
          k -= tree(tindex + 1)
          e = tree(tindex)

          if (e & 16) != 0 then // distance
            get = e & 15
            dist = tree(tindex + 2)
            mode = Distext
          else if (e & 64) == 0 then // next table
            need = e
            treeIndex = tindex/3 + tree(tindex + 2)
          else
            mode = Badcode // invalid code
            z.msg = "invalid distance code"
            r = ZDataError
            return leave(r, b, k, p, n, q)

        case Distext => // i: getting distance extra
          j = get

          while k < j do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (input(p) & 0xff) << k
            p += 1
            k += 8

          dist += b & inflateMask(j)
          b >>= j
          k -= j
          mode = Copy

        case Copy => // o: copying bytes in window, waiting for space
          f = q - dist

          while f < 0 do f += s.end // modulo window size — "while" handles invalid distances

          var bail = false

          while len != 0 && !bail do
            if m == 0 then
              if q == s.end && s.read != 0 then
                q = 0
                m = if q < s.read then s.read - q - 1 else s.end - q

              if m == 0 then
                s.write = q
                r = s.inflateFlush(r)
                q = s.write
                m = if q < s.read then s.read - q - 1 else s.end - q

                if q == s.end && s.read != 0 then
                  q = 0
                  m = if q < s.read then s.read - q - 1 else s.end - q

                if m == 0 then bail = true

            if !bail then
              win(q) = win(f)
              q += 1
              f += 1
              m -= 1
              if f == s.end then f = 0
              len -= 1

          if bail then return leave(r, b, k, p, n, q)
          mode = Start

        case Lit => // o: got literal, waiting for output space
          if m == 0 then
            if q == s.end && s.read != 0 then
              q = 0
              m = if q < s.read then s.read - q - 1 else s.end - q

            if m == 0 then
              s.write = q
              r = s.inflateFlush(r)
              q = s.write
              m = if q < s.read then s.read - q - 1 else s.end - q

              if q == s.end && s.read != 0 then
                q = 0
                m = if q < s.read then s.read - q - 1 else s.end - q

              if m == 0 then return leave(r, b, k, p, n, q)

          r = ZOk
          win(q) = lit.toByte
          q += 1
          m -= 1
          mode = Start

        case Wash => // o: got eob, possibly more output
          if k > 7 then // return unused byte, if any
            k -= 8
            n += 1
            p -= 1 // can always return one

          s.write = q
          r = s.inflateFlush(r)
          q = s.write
          m = if q < s.read then s.read - q - 1 else s.end - q

          if s.read != s.write then return leave(r, b, k, p, n, q)
          mode = End

        case End =>
          r = ZStreamEnd
          return leave(r, b, k, p, n, q)

        case Badcode => // x: got error
          r = ZDataError
          return leave(r, b, k, p, n, q)

        case _ =>
          r = ZStreamError
          return leave(r, b, k, p, n, q)

    ZStreamError // unreachable

  // Called with number of bytes left to write in window at least 258 (the maximum string length)
  // and number of input bytes available at least ten.
  private def inflateFast
    ( bl: Int, bd: Int, tl: Array[Int], tlIndex: Int, td: Array[Int], tdIndex: Int )
  :   Int =


    var t = 0       // temporary pointer
    var tp = tl     // temporary pointer
    var tpIndex = 0 // temporary pointer
    var e = 0       // extra bits or operation
    var b = 0       // bit buffer
    var k = 0       // bits in bit buffer
    var p = 0       // input data pointer
    var n = 0       // bytes available there
    var q = 0       // output window write pointer
    var m = 0       // bytes to end of window or read pointer
    var c = 0       // bytes to copy
    var d = 0       // distance back to copy from
    var r = 0       // copy source pointer
    var tpIndexT3 = 0 // (tpIndex + t)*3

    val input = z.nextIn
    val win = s.window

    // load input, output, bit values
    p = z.nextInIndex; n = z.availIn; b = s.bitb; k = s.bitk
    q = s.write; m = if q < s.read then s.read - q - 1 else s.end - q

    // initialize masks
    val ml = inflateMask(bl) // mask for literal/length tree
    val md = inflateMask(bd) // mask for distance tree

    // Parameters rather than captures, to keep the hot locals unboxed (see `proc`).
    def leave(status: Int, b: Int, k0: Int, p0: Int, n0: Int, q: Int): Int =
      var c0 = z.availIn - n0
      c0 = if (k0 >> 3) < c0 then k0 >> 3 else c0
      val n1 = n0 + c0
      val p1 = p0 - c0
      val k1 = k0 - (c0 << 3)
      s.bitb = b; s.bitk = k1
      z.availIn = n1; z.totalIn += p1 - z.nextInIndex; z.nextInIndex = p1
      s.write = q
      status

    // do until not enough input or output space for fast loop
    while // assume called with m >= 258 && n >= 10
      // get literal/length code
      while k < 20 do // max bits for literal/length code
        n -= 1
        b |= (input(p) & 0xff) << k
        p += 1
        k += 8

      t = b & ml
      tp = tl
      tpIndex = tlIndex
      tpIndexT3 = (tpIndex + t)*3
      e = tp(tpIndexT3)

      if e == 0 then
        b >>= tp(tpIndexT3 + 1)
        k -= tp(tpIndexT3 + 1)
        win(q) = tp(tpIndexT3 + 2).toByte
        q += 1
        m -= 1
      else
        var innerDone = false

        while !innerDone do
          b >>= tp(tpIndexT3 + 1)
          k -= tp(tpIndexT3 + 1)

          if (e & 16) != 0 then
            e &= 15
            c = tp(tpIndexT3 + 2) + (b & inflateMask(e))
            b >>= e
            k -= e

            // decode distance base of block to copy
            while k < 15 do // max bits for distance code
              n -= 1
              b |= (input(p) & 0xff) << k
              p += 1
              k += 8

            t = b & md
            tp = td
            tpIndex = tdIndex
            tpIndexT3 = (tpIndex + t)*3
            e = tp(tpIndexT3)

            var distDone = false

            while !distDone do
              b >>= tp(tpIndexT3 + 1)
              k -= tp(tpIndexT3 + 1)

              if (e & 16) != 0 then
                // get extra bits to add to distance base
                e &= 15

                while k < e do // get extra bits (up to 13)
                  n -= 1
                  b |= (input(p) & 0xff) << k
                  p += 1
                  k += 8

                d = tp(tpIndexT3 + 2) + (b & inflateMask(e))
                b >>= e
                k -= e

                // do the copy
                m -= c

                if q >= d then // offset before dest: just copy
                  r = q - d

                  if q - r > 0 && 2 > (q - r) then
                    win(q) = win(r); q += 1; r += 1 // minimum count is three,
                    win(q) = win(r); q += 1; r += 1 // so unroll loop a little
                    c -= 2
                  else
                    System.arraycopy(win, r, win, q, 2)
                    q += 2
                    r += 2
                    c -= 2
                else // else offset after destination
                  r = q - d

                  while { r += s.end; r < 0 } do () // force pointer in window

                  e = s.end - r

                  if c > e then // if source crosses, wrapped copy
                    c -= e

                    if q - r > 0 && e > (q - r) then
                      while { win(q) = win(r); q += 1; r += 1; e -= 1; e != 0 } do ()
                    else
                      System.arraycopy(win, r, win, q, e)
                      q += e
                      r += e
                      e = 0

                    r = 0 // copy rest from start of window

                // copy all or what's left
                if q - r > 0 && c > (q - r) then
                  while { win(q) = win(r); q += 1; r += 1; c -= 1; c != 0 } do ()
                else
                  System.arraycopy(win, r, win, q, c)
                  q += c
                  r += c
                  c = 0

                distDone = true
              else if (e & 64) == 0 then
                t += tp(tpIndexT3 + 2)
                t += b & inflateMask(e)
                tpIndexT3 = (tpIndex + t)*3
                e = tp(tpIndexT3)
              else
                z.msg = "invalid distance code"
                return leave(ZDataError, b, k, p, n, q)

            innerDone = true
          else if (e & 64) == 0 then
            t += tp(tpIndexT3 + 2)
            t += b & inflateMask(e)
            tpIndexT3 = (tpIndex + t)*3
            e = tp(tpIndexT3)

            if e == 0 then
              b >>= tp(tpIndexT3 + 1)
              k -= tp(tpIndexT3 + 1)
              win(q) = tp(tpIndexT3 + 2).toByte
              q += 1
              m -= 1
              innerDone = true
          else if (e & 32) != 0 then
            return leave(ZStreamEnd, b, k, p, n, q)
          else
            z.msg = "invalid literal/length code"
            return leave(ZDataError, b, k, p, n, q)

      m >= 258 && n >= 10
    do ()

    // not enough input or output — restore pointers and return
    leave(ZOk, b, k, p, n, q)

private[pneumatic] object InfBlocks:
  final val Many = 1440

  final val Type = 0   // get type bits (3, including end bit)
  final val Lens = 1   // get lengths for stored
  final val Stored = 2 // processing stored block
  final val Table = 3  // get table lengths
  final val Btree = 4  // get bit lengths tree for a dynamic block
  final val Dtree = 5  // get length, distance trees for a dynamic block
  final val Codes = 6  // processing fixed or dynamic block
  final val Dry = 7    // output remaining window bytes
  final val Done = 8   // finished last block, done
  final val Bad = 9    // got a data error — stuck here

private[pneumatic] final class InfBlocks(z: Inflater, check: Boolean, w: Int):
  import InfBlocks.*

  var mode: Int = Type // current inflate_block mode
  var left: Int = 0    // if Stored, bytes left to copy
  var table: Int = 0   // table lengths (14 bits)
  var index: Int = 0   // index into blens (or border)
  var blens: Array[Int] = emptyInts // bit lengths of codes
  val bb: Array[Int] = new Array[Int](1) // bit length tree depth
  val tb: Array[Int] = new Array[Int](1) // bit length decoding tree

  private val bl = new Array[Int](1)
  private val bd = new Array[Int](1)
  private val tli = new Array[Int](1) // tl index
  private val tdi = new Array[Int](1) // td index

  private val codes: InfCodes = InfCodes(z, this)
  private val inftree: InfTree = InfTree()

  var last: Int = 0 // true if this block is the last block

  // mode independent information
  var bitk: Int = 0 // bits in bit buffer
  var bitb: Int = 0 // bit buffer
  val hufts: Array[Int] = new Array[Int](Many*3) // single allocation for tree space
  val window: Array[Byte] = new Array[Byte](w)   // sliding window
  val end: Int = w    // one byte after sliding window
  var read: Int = 0   // window read pointer
  var write: Int = 0  // window write pointer

  reset()

  def reset(): Unit =
    mode = Type
    bitk = 0
    bitb = 0
    read = 0
    write = 0
    if check then z.adler.reset()

  def proc(r0: Int): Int =
    var r = r0
    var t = 0 // temporary storage
    var b = 0 // bit buffer
    var k = 0 // bits in bit buffer
    var p = 0 // input data pointer
    var n = 0 // bytes available there
    var q = 0 // output window write pointer
    var m = 0 // bytes to end of window or read pointer

    // copy input/output information to locals; `leave` restores it and flushes the window
    p = z.nextInIndex; n = z.availIn; b = bitb; k = bitk
    q = write; m = if q < read then read - q - 1 else end - q

    // Parameters rather than captures, to keep the hot locals unboxed (see `InfCodes.proc`).
    def leave(status: Int, b: Int, k: Int, p: Int, n: Int, q: Int): Int =
      bitb = b; bitk = k
      z.availIn = n; z.totalIn += p - z.nextInIndex; z.nextInIndex = p
      write = q
      inflateFlush(status)

    // process input based on current state
    while true do
      mode match
        case Type =>
          while k < 3 do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (z.nextIn(p) & 0xff) << k
            p += 1
            k += 8

          t = b & 7
          last = t & 1

          (t >>> 1) match
            case 0 => // stored
              b >>>= 3
              k -= 3
              t = k & 7 // go to byte boundary
              b >>>= t
              k -= t
              mode = Lens // get length of stored block

            case 1 => // fixed
              codes.init(fixedBl, fixedBd, fixedTl, 0, fixedTd, 0)
              b >>>= 3
              k -= 3
              mode = Codes

            case 2 => // dynamic
              b >>>= 3
              k -= 3
              mode = Table

            case _ => // illegal
              b >>>= 3
              k -= 3
              mode = Bad
              z.msg = "invalid block type"
              r = ZDataError
              return leave(r, b, k, p, n, q)

        case Lens =>
          while k < 32 do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (z.nextIn(p) & 0xff) << k
            p += 1
            k += 8

          if (((~b) >>> 16) & 0xffff) != (b & 0xffff) then
            mode = Bad
            z.msg = "invalid stored block lengths"
            r = ZDataError
            return leave(r, b, k, p, n, q)

          left = b & 0xffff
          b = 0 // dump bits
          k = 0
          mode = if left != 0 then Stored else if last != 0 then Dry else Type

        case Stored =>
          if n == 0 then return leave(r, b, k, p, n, q)

          var bail = false

          if m == 0 then
            if q == end && read != 0 then
              q = 0
              m = if q < read then read - q - 1 else end - q

            if m == 0 then
              write = q
              r = inflateFlush(r)
              q = write
              m = if q < read then read - q - 1 else end - q

              if q == end && read != 0 then
                q = 0
                m = if q < read then read - q - 1 else end - q

              if m == 0 then bail = true

          if bail then return leave(r, b, k, p, n, q)
          r = ZOk

          t = left
          if t > n then t = n
          if t > m then t = m
          System.arraycopy(z.nextIn, p, window, q, t)
          p += t
          n -= t
          q += t
          m -= t
          left -= t
          if left == 0 then mode = if last != 0 then Dry else Type

        case Table =>
          while k < 14 do
            if n != 0 then r = ZOk else return leave(r, b, k, p, n, q)
            n -= 1
            b |= (z.nextIn(p) & 0xff) << k
            p += 1
            k += 8

          t = b & 0x3fff
          table = t

          if (t & 0x1f) > 29 || ((t >> 5) & 0x1f) > 29 then
            mode = Bad
            z.msg = "too many length or distance symbols"
            r = ZDataError
            return leave(r, b, k, p, n, q)

          t = 258 + (t & 0x1f) + ((t >> 5) & 0x1f)

          if blens.length < t then blens = new Array[Int](t)
          else
            var i = 0
            while i < t do { blens(i) = 0; i += 1 }

          b >>>= 14
          k -= 14
          index = 0
          mode = Btree

        case Btree =>
          var bail = false

          while index < 4 + (table >>> 10) && !bail do
            while k < 3 && !bail do
              if n != 0 then r = ZOk else bail = true

              if !bail then
                n -= 1
                b |= (z.nextIn(p) & 0xff) << k
                p += 1
                k += 8

            if !bail then
              blens(border(index)) = b & 7
              index += 1
              b >>>= 3
              k -= 3

          if bail then return leave(r, b, k, p, n, q)

          while index < 19 do
            blens(border(index)) = 0
            index += 1

          bb(0) = 7
          t = inftree.inflateTreesBits(blens, bb, tb, hufts, z)

          if t != ZOk then
            r = t
            if r == ZDataError then mode = Bad
            return leave(r, b, k, p, n, q)

          index = 0
          mode = Dtree

        case Dtree =>
          var bail = false
          var loop = true

          while loop && !bail do
            t = table

            if !(index < 258 + (t & 0x1f) + ((t >> 5) & 0x1f)) then loop = false
            else
              t = bb(0)

              while k < t && !bail do
                if n != 0 then r = ZOk else bail = true

                if !bail then
                  n -= 1
                  b |= (z.nextIn(p) & 0xff) << k
                  p += 1
                  k += 8

              if !bail then
                t = hufts((tb(0) + (b & inflateMask(t)))*3 + 1)
                val c = hufts((tb(0) + (b & inflateMask(t)))*3 + 2)

                if c < 16 then
                  b >>>= t
                  k -= t
                  blens(index) = c
                  index += 1
                else // c == 16..18
                  var i = if c == 18 then 7 else c - 14
                  var j = if c == 18 then 11 else 3

                  while k < t + i && !bail do
                    if n != 0 then r = ZOk else bail = true

                    if !bail then
                      n -= 1
                      b |= (z.nextIn(p) & 0xff) << k
                      p += 1
                      k += 8

                  if !bail then
                    b >>>= t
                    k -= t
                    j += b & inflateMask(i)
                    b >>>= i
                    k -= i

                    i = index
                    t = table

                    if i + j > 258 + (t & 0x1f) + ((t >> 5) & 0x1f) || (c == 16 && i < 1) then
                      mode = Bad
                      z.msg = "invalid bit length repeat"
                      r = ZDataError
                      return leave(r, b, k, p, n, q)

                    val cv = if c == 16 then blens(i - 1) else 0

                    while { blens(i) = cv; i += 1; j -= 1; j != 0 } do ()

                    index = i

          if bail then return leave(r, b, k, p, n, q)

          tb(0) = -1
          bl(0) = 9 // must be <= 9 for lookahead assumptions
          bd(0) = 6 // must be <= 9 for lookahead assumptions
          t = table

          t = inftree.inflateTreesDynamic(257 + (t & 0x1f), 1 + ((t >> 5) & 0x1f), blens, bl, bd,
              tli, tdi, hufts, z)

          if t != ZOk then
            if t == ZDataError then mode = Bad
            r = t
            return leave(r, b, k, p, n, q)

          codes.init(bl(0), bd(0), hufts, tli(0), hufts, tdi(0))
          mode = Codes

        case Codes =>
          bitb = b; bitk = k
          z.availIn = n; z.totalIn += p - z.nextInIndex; z.nextInIndex = p
          write = q

          r = codes.proc(r)
          if r != ZStreamEnd then return inflateFlush(r)

          r = ZOk
          p = z.nextInIndex; n = z.availIn; b = bitb; k = bitk
          q = write; m = if q < read then read - q - 1 else end - q

          if last == 0 then mode = Type
          else mode = Dry

        case Dry =>
          write = q
          r = inflateFlush(r)
          q = write
          m = if q < read then read - q - 1 else end - q
          if read != write then return leave(r, b, k, p, n, q)
          mode = Done

        case Done =>
          r = ZStreamEnd
          return leave(r, b, k, p, n, q)

        case Bad =>
          r = ZDataError
          return leave(r, b, k, p, n, q)

        case _ =>
          r = ZStreamError
          return leave(r, b, k, p, n, q)

    ZStreamError // unreachable

  // copy as much as possible from the sliding window to the output area
  def inflateFlush(r0: Int): Int =
    var r = r0

    // local copies of source and destination pointers
    var p = z.nextOutIndex
    var q = read

    // compute number of bytes to copy as far as end of window
    var n = (if q <= write then write else end) - q
    if n > z.availOut then n = z.availOut
    if n != 0 && r == ZBufError then r = ZOk

    // update counters
    z.availOut -= n
    z.totalOut += n

    // update check information
    if check && n > 0 then z.adler.update(window, q, n)

    // copy as far as end of window
    System.arraycopy(window, q, z.nextOut, p, n)
    p += n
    q += n

    // see if more to copy at beginning of window
    if q == end then
      // wrap pointers
      q = 0
      if write == end then write = 0

      // compute bytes to copy
      n = write - q
      if n > z.availOut then n = z.availOut
      if n != 0 && r == ZBufError then r = ZOk

      // update counters
      z.availOut -= n
      z.totalOut += n

      // update check information
      if check && n > 0 then z.adler.update(window, q, n)

      // copy
      System.arraycopy(window, q, z.nextOut, p, n)
      p += n
      q += n

    // update pointers
    z.nextOutIndex = p
    read = q

    r

private[pneumatic] object Inflater:
  final val Head = 14   // waiting for the zlib header (or straight to Blocks when raw)
  final val Dict4 = 2   // four dictionary check bytes to go
  final val Dict3 = 3   // three dictionary check bytes to go
  final val Dict2 = 4   // two dictionary check bytes to go
  final val Dict1 = 5   // one dictionary check byte to go
  final val Dict0 = 6   // waiting for a preset dictionary (unsupported)
  final val Blocks = 7  // decompressing blocks
  final val Check4 = 8  // four check bytes to go
  final val Check3 = 9  // three check bytes to go
  final val Check2 = 10 // two check bytes to go
  final val Check1 = 11 // one check byte to go
  final val Done = 12   // finished check, done
  final val Bad = 13    // got an error — stay here

// A streaming inflater with the same call pattern as `java.util.zip.Inflater`: feed input with
// `setInput`, drain output with `inflate`, which returns the number of bytes produced. Corrupt
// data throws `IllegalStateException`. The zlib wrapper (header and Adler-32 trailer) is parsed
// unless `nowrap` is set, in which case the stream is raw DEFLATE.
private[pneumatic] final class Inflater(nowrap: Boolean) extends InflateEngine:
  import Inflater.*

  private[pneumatic] var nextIn: Array[Byte] = empty
  private[pneumatic] var nextInIndex: Int = 0
  private[pneumatic] var availIn: Int = 0
  private[pneumatic] var totalIn: Long = 0
  private[pneumatic] var nextOut: Array[Byte] = empty
  private[pneumatic] var nextOutIndex: Int = 0
  private[pneumatic] var availOut: Int = 0
  private[pneumatic] var totalOut: Long = 0
  private[pneumatic] var msg: String = ""
  private[pneumatic] var adler: FlateChecksum = Adler32()

  private val wrap: Int = if nowrap then 0 else 1
  private val wbits: Int = MaxWbits
  private var mode: Int = Head
  private var method: Int = 0
  private var was: Long = -1 // computed check value
  private var need: Long = 0 // stream check value
  private var needBytes: Int = -1

  private val blocks: InfBlocks = InfBlocks(this, wrap != 0, 1 << wbits)

  private def run(f0: Int): Int =
    val f = if f0 == ZFinish then ZBufError else ZOk
    var r = ZBufError

    while true do
      mode match
        case Head =>
          if wrap == 0 then mode = Blocks
          else
            if needBytes == -1 then
              needBytes = 2
              need = 0

            while needBytes > 0 do
              if availIn == 0 then return r
              r = f
              availIn -= 1
              totalIn += 1
              need = need | ((nextIn(nextInIndex) & 0xff).toLong << ((2 - needBytes)*8))
              nextInIndex += 1
              needBytes -= 1

            need &= 0xffffL
            needBytes = -1

            method = need.toInt & 0xff
            val b = (need >> 8).toInt & 0xff

            if (method & 0xf) != ZDeflated then
              mode = Bad
              msg = "unknown compression method"
            else if ((method << 8) + b)%31 != 0 then
              mode = Bad
              msg = "incorrect header check"
            else if (method >> 4) + 8 > wbits then
              mode = Bad
              msg = "invalid window size"
            else
              adler = Adler32()
              if (b & PresetDict) == 0 then mode = Blocks else mode = Dict4

        case Dict4 | Dict3 | Dict2 | Dict1 | Dict0 =>
          mode = Bad
          msg = "preset dictionaries are not supported"
          return ZStreamError

        case Blocks =>
          r = blocks.proc(r)

          if r == ZDataError then mode = Bad
          else
            if r == ZOk then r = f

            if r != ZStreamEnd then return r

            r = f
            was = adler.value
            blocks.reset()

            if wrap == 0 then mode = Done else mode = Check4

        case Check4 =>
          if availIn == 0 then return r
          r = f
          availIn -= 1
          totalIn += 1
          need = ((nextIn(nextInIndex) & 0xff).toLong << 24) & 0xff000000L
          nextInIndex += 1
          mode = Check3

        case Check3 =>
          if availIn == 0 then return r
          r = f
          availIn -= 1
          totalIn += 1
          need += ((nextIn(nextInIndex) & 0xff).toLong << 16) & 0xff0000L
          nextInIndex += 1
          mode = Check2

        case Check2 =>
          if availIn == 0 then return r
          r = f
          availIn -= 1
          totalIn += 1
          need += ((nextIn(nextInIndex) & 0xff).toLong << 8) & 0xff00L
          nextInIndex += 1
          mode = Check1

        case Check1 =>
          if availIn == 0 then return r
          r = f
          availIn -= 1
          totalIn += 1
          need += (nextIn(nextInIndex) & 0xff).toLong
          nextInIndex += 1

          if was.toInt != need.toInt then
            mode = Bad
            msg = "incorrect data check"
          else
            mode = Done

        case Done =>
          return ZStreamEnd

        case Bad =>
          return ZDataError

        case _ =>
          return ZStreamError

    ZStreamError // unreachable

  def setInput(buffer: Array[Byte]): Unit = setInput(buffer, 0, buffer.length)

  def setInput(buffer: Array[Byte], offset: Int, length: Int): Unit =
    nextIn = buffer
    nextInIndex = offset
    availIn = length

  def getRemaining: Int = availIn

  def finished: Boolean = mode == Done

  def inflate(target: Array[Byte]): Int = inflate(target, 0, target.length)

  def inflate(target: Array[Byte], offset: Int, space: Int): Int =
    nextOut = target
    nextOutIndex = offset
    availOut = space
    val result = run(ZNoFlush)
    val produced = space - availOut
    nextOut = empty

    if result == ZDataError || result == ZStreamError
    then throw IllegalStateException("the compressed data is corrupt: "+msg)

    produced

  def end(): Unit = ()
