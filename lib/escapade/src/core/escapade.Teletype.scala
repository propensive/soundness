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
package escapade

import language.experimental.pureFunctions

import scala.util.*

import anticipation.*
import denominative.*
import gossamer.*
import mercator.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

object Teletype:
  // Heuristic: convert a dense styles array to sparse iff
  //   runs * SparseThreshold <= plain.length
  // (i.e., text is at least ~SparseThreshold× longer than the run count)
  inline val SparseThreshold = 2

  given add: NotGiven[Teletype is Textual] => Teletype is Addable:
    type Operand = Teletype
    type Result = Teletype

    inline def add(left: Teletype, right: Teletype): Teletype = left.append(right)

  given concatenable: Teletype is Concatenable:
    type Operand = Teletype
    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)

  given out: Stdio => Out.type is Writable by Teletype = new Writable:
    type Self = Out.type
    type Operand = Teletype

    def write(target: Self, stream: Stream[Teletype]): Unit =
      stream.flow(())(Out.print(next) yet write(target, more))

  given err: Stdio => Err.type is Writable by Teletype = new Writable:
    type Self = Err.type
    type Operand = Teletype

    def write(target: Self, stream: Stream[Teletype]): Unit =
      stream.flow(())(Err.print(next) yet write(target, more))

  given textual: Teletype is Textual:
    type Operand = Char
    type Show[value] = value is Teletypeable

    def classTag: ClassTag[Teletype] = summon[ClassTag[Teletype]]
    def size(text: Teletype): Int = text.plain.length
    def text(teletype: Teletype): Text = teletype.plain
    def length(text: Teletype): Int = text.plain.length
    def apply(text: Text): Teletype = Teletype(text)
    def single(operand: Char): Teletype = Teletype(operand.show)
    def fromChar(char: Char): Char = char

    def map(text: Teletype)(lambda: Char => Char): Teletype =
      val array = text.plain.s.toCharArray.nn

      array.indices.each: index =>
        array(index) = lambda(array(index))

      Teletype(new String(array).tt, text.styles, text.hyperlinks, text.insertions, text.boundaries)

    def segment(text: Teletype, interval: Interval): Teletype =
      text.dropChars(interval.start.n0).takeChars(interval.size)

    val empty: Teletype = Teletype.empty

    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)
    def at(text: Teletype, index: Ordinal): Char = text.plain.s.charAt(index.n0)

    def indexOf(text: Teletype, sub: Text, start: Ordinal): Optional[Ordinal] =
      text.plain.s.indexOf(sub.s, start.n0).puncture(-1).let(_.z)

    def show[value: Teletypeable](value: value) = value.teletype
    def builder(size: Optional[Int] = Unset): TeletypeBuilder = TeletypeBuilder(size)

  // Empty Teletype: dense form with no chars and one trailing entry.
  val empty: Teletype =
    new Teletype(t"", IArray(0L), Map.empty, TreeMap.empty, IArray.empty[Int])

  given joinable: Teletype is Joinable = _.fold(empty)(_ + _)
  given printable: Teletype is Printable = _.render(_)

  given cuttable: Teletype is Cuttable by Text = (text, delimiter, limit) =>
    import java.util.regex.*

    val pattern = Pattern.compile(t"(.*)${Pattern.quote(delimiter.s).nn}(.*)".s).nn

    @tailrec
    def recur(source: Teletype, limit: Int, acc: List[Teletype]): List[Teletype] =
      if limit <= 0 then acc
      else
        val matcher = pattern.matcher(source.plain.s).nn

        if !matcher.matches then source :: acc else
          val output = source.keep(matcher.group(2).nn.length, Rtl)
          recur(source.keep(matcher.group(1).nn.length), limit - 1, output :: acc)

    recur(text, limit, Nil)

  given ordering: Ordering[Teletype] = Ordering.by(_.plain)

  // Build a Teletype from text with no styling. Always sparse (1 run).
  def apply(text: Text): Teletype =
    new Teletype(text, IArray(0L, 0L), Map.empty, TreeMap.empty, IArray(0))

  // Build a Teletype with a single uniform style applied to all chars.
  def styled[value: Showable](value: value)(transform: Ansi.Transform): Teletype =
    val text: Text = value.show
    val styled: Long = transform(TextStyle()).styleWord
    if text.length == 0 then
      new Teletype(t"", IArray(styled, 0L), Map.empty, TreeMap.empty, IArray(0))
    else
      new Teletype(text, IArray(styled, 0L), Map.empty, TreeMap.empty, IArray(0))

  // Compress a dense styles array into sparse form if it would benefit.
  // Returns the resulting (styles, boundaries) pair.
  def compressIfBeneficial(plain: Text, denseStyles: IArray[Long])
  :     (IArray[Long], IArray[Int]) =

    val n = plain.length
    if n == 0 then
      (IArray(if denseStyles.length > 0 then denseStyles(0) else 0L), IArray.empty[Int])
    else
      // Count runs
      var runs = 1
      var i = 1
      while i < n do
        if denseStyles(i) != denseStyles(i - 1) then runs += 1
        i += 1

      if runs * SparseThreshold > n then
        // Keep dense
        (denseStyles, IArray.empty[Int])
      else
        // Convert to sparse
        val newStyles = new Array[Long](runs + 1)
        val newBoundaries = new Array[Int](runs)
        newBoundaries(0) = 0
        newStyles(0) = denseStyles(0)
        var src = 1
        var dst = 1
        while src < n do
          if denseStyles(src) != denseStyles(src - 1) then
            newBoundaries(dst) = src
            newStyles(dst) = denseStyles(src)
            dst += 1
          src += 1
        newStyles(runs) = denseStyles(n)  // trailing
        (IArray.unsafeFromArray(newStyles), IArray.unsafeFromArray(newBoundaries))


// `boundaries` is the run-start array for the sparse form; empty for the dense form.
// Dense:  styles.length == plain.length + 1; styles(i) is the style for char i (0 ≤ i < length)
//         and styles(length) is the trailing style.
// Sparse: styles.length == boundaries.length + 1; boundaries(i) is the start position of run i
//         (boundaries(0) == 0). Run i covers [boundaries(i), nextStart) where nextStart is
//         boundaries(i+1) or plain.length for the last run. styles(boundaries.length) is the
//         trailing style.
case class Teletype
  ( plain:      Text,
    styles:     IArray[Long],
    hyperlinks: Map[Int, Text]            = Map.empty,
    insertions: TreeMap[Int, Text]        = TreeMap.empty,
    boundaries: IArray[Int]               = IArray.empty[Int] ):

  inline def isDense: Boolean = boundaries.length == 0

  // Style at position p (0 ≤ p ≤ plain.length, where length means "trailing").
  def styleAt(p: Int): Long =
    if isDense then styles(p)
    else if p >= plain.length then styles(boundaries.length)
    else
      // Binary search for the run that contains p.
      var lo = 0
      var hi = boundaries.length
      while lo + 1 < hi do
        val mid = (lo + hi) >>> 1
        if boundaries(mid) <= p then lo = mid else hi = mid
      styles(lo)

  // Trailing style (the style after the last char; used for joins).
  inline def trailingStyle: Long =
    if isDense then styles(plain.length) else styles(boundaries.length)

  // Convert to sparse form's (styles, boundaries) representation.
  // For an already-sparse Teletype this is O(1); for a dense one it's O(plain.length).
  def asSparseArrays: (IArray[Long], IArray[Int]) =
    if !isDense then (styles, boundaries)
    else if plain.length == 0 then (IArray(if styles.length > 0 then styles(0) else 0L), IArray(0))
    else
      val n = plain.length
      // Count runs
      var runs = 1
      var i = 1
      while i < n do
        if styles(i) != styles(i - 1) then runs += 1
        i += 1
      val newStyles = new Array[Long](runs + 1)
      val newBoundaries = new Array[Int](runs)
      newBoundaries(0) = 0
      newStyles(0) = styles(0)
      var src = 1
      var dst = 1
      while src < n do
        if styles(src) != styles(src - 1) then
          newBoundaries(dst) = src
          newStyles(dst) = styles(src)
          dst += 1
        src += 1
      newStyles(runs) = styles(n)
      (IArray.unsafeFromArray(newStyles), IArray.unsafeFromArray(newBoundaries))

  def explicit: Text = render(termcapDefinitions.xtermTrueColor).bind: char =>
    if char.toInt == 27 then t"\\e" else char.show

  @targetName("add")
  def append(text: Text): Teletype =
    if text.length == 0 then this else
      val tail = trailingStyle
      val combinedPlain = t"$plain$text"
      if isDense then
        // Stay dense: extend styles array with the trailing style
        val newLength = plain.length + text.length + 1
        val arr = new Array[Long](newLength)
        var i = 0
        while i < plain.length do
          arr(i) = styles(i)
          i += 1
        while i < newLength do
          arr(i) = tail
          i += 1
        Teletype(combinedPlain, IArray.unsafeFromArray(arr), hyperlinks, insertions, IArray.empty[Int])
      else
        // Stay sparse: the new chars become part of the last run (since trailing style = last run style)
        // unless the last run's style differs from the trailing style — but that can't happen because
        // styles(boundaries.length-1) is the last run's style, and styles(boundaries.length) is trailing.
        // They may differ. If so, we need a new run for the appended text.
        val k = boundaries.length
        val lastRunStyle = styles(k - 1)
        if lastRunStyle == tail then
          // Just extend plain; runs unchanged
          Teletype(combinedPlain, styles, hyperlinks, insertions, boundaries)
        else
          // Add a new run starting at plain.length, with style = tail
          val newBoundaries = new Array[Int](k + 1)
          val newStyles = new Array[Long](k + 2)
          System.arraycopy(boundaries.asInstanceOf[Array[Int]], 0, newBoundaries, 0, k)
          newBoundaries(k) = plain.length
          System.arraycopy(styles.asInstanceOf[Array[Long]], 0, newStyles, 0, k)
          newStyles(k) = tail
          newStyles(k + 1) = tail
          Teletype
            ( combinedPlain,
              IArray.unsafeFromArray(newStyles),
              hyperlinks,
              insertions,
              IArray.unsafeFromArray(newBoundaries) )

  @targetName("add2")
  def append(that: Teletype): Teletype =
    if that.plain.length == 0 then this else if plain.length == 0 then that else
      val aN = plain.length
      val combinedPlain = plain+that.plain

      val shiftedLinks = if that.hyperlinks.isEmpty then hyperlinks else
        hyperlinks ++ that.hyperlinks.map((k, v) => (k + aN) -> v)

      val shiftedInsertions = if that.insertions.isEmpty then insertions else
        insertions ++ that.insertions.map((k, v) => (k + aN) -> v)

      if isDense && that.isDense then
        // Both dense — direct array copy
        val newLength = aN + that.plain.length + 1
        val arr = new Array[Long](newLength)
        System.arraycopy(styles.asInstanceOf[Array[Long]], 0, arr, 0, aN)
        System.arraycopy
          (that.styles.asInstanceOf[Array[Long]], 0, arr, aN, that.styles.length)
        Teletype
          ( combinedPlain,
            IArray.unsafeFromArray(arr),
            shiftedLinks,
            shiftedInsertions,
            IArray.empty[Int] )
      else
        // At least one is sparse — combine in sparse form.
        val (aStyles, aBoundaries) = asSparseArrays
        val (bStyles, bBoundaries) = that.asSparseArrays
        val aK = aBoundaries.length
        val bK = bBoundaries.length
        val aLastStyle = aStyles(aK - 1)
        val bFirstStyle = bStyles(0)
        val merge = aLastStyle == bFirstStyle
        val newK = aK + bK - (if merge then 1 else 0)
        val newBoundariesArr = new Array[Int](newK)
        val newStylesArr = new Array[Long](newK + 1)
        // Copy A's runs
        System.arraycopy(aBoundaries.asInstanceOf[Array[Int]], 0, newBoundariesArr, 0, aK)
        System.arraycopy(aStyles.asInstanceOf[Array[Long]], 0, newStylesArr, 0, aK)
        // Copy B's runs (shifted by aN), optionally skipping the first if merging
        var bi = if merge then 1 else 0
        var ni = aK
        while bi < bK do
          newBoundariesArr(ni) = bBoundaries(bi) + aN
          newStylesArr(ni) = bStyles(bi)
          bi += 1
          ni += 1
        // Trailing style is B's trailing
        newStylesArr(newK) = bStyles(bK)
        Teletype
          ( combinedPlain,
            IArray.unsafeFromArray(newStylesArr),
            shiftedLinks,
            shiftedInsertions,
            IArray.unsafeFromArray(newBoundariesArr) )

  def dropChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl => takeChars(plain.length - n)

    case Ltr =>
      val keepLength = plain.length - n
      if keepLength <= 0 then Teletype.empty
      else if n <= 0 then this
      else
        val newHyperlinks = hyperlinks.collect { case (k, v) if k >= n => (k - n) -> v }
        val newInsertions =
          insertions.collect { case (k, v) if k >= n => (k - n) -> v }.to(TreeMap)
        if isDense then
          val arr = new Array[Long](keepLength + 1)
          var i = 0
          while i <= keepLength do
            arr(i) = styles(n + i)
            i += 1
          Teletype
            ( plain.skip(n),
              IArray.unsafeFromArray(arr),
              newHyperlinks,
              newInsertions,
              IArray.empty[Int] )
        else
          // Sparse: find the run that contains position n; drop earlier runs;
          // adjust the first kept run's boundary to 0; shift all other boundaries by -n.
          val k = boundaries.length
          // Binary search for the run at position n
          var lo = 0
          var hi = k
          while lo + 1 < hi do
            val mid = (lo + hi) >>> 1
            if boundaries(mid) <= n then lo = mid else hi = mid
          val firstRun = lo
          val newK = k - firstRun
          val newBoundariesArr = new Array[Int](newK)
          val newStylesArr = new Array[Long](newK + 1)
          newBoundariesArr(0) = 0
          newStylesArr(0) = styles(firstRun)
          var i = 1
          while i < newK do
            newBoundariesArr(i) = boundaries(firstRun + i) - n
            newStylesArr(i) = styles(firstRun + i)
            i += 1
          newStylesArr(newK) = styles(k)  // trailing
          Teletype
            ( plain.skip(n),
              IArray.unsafeFromArray(newStylesArr),
              newHyperlinks,
              newInsertions,
              IArray.unsafeFromArray(newBoundariesArr) )

  def takeChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl => dropChars(plain.length - n)

    case Ltr =>
      if n <= 0 then Teletype.empty
      else if n >= plain.length then this
      else
        val newHyperlinks = hyperlinks.filter((k, _) => k < n)
        val newInsertions = insertions.rangeUntil(n)
        if isDense then
          val arr = new Array[Long](n + 1)
          var i = 0
          while i < n do
            arr(i) = styles(i)
            i += 1
          arr(n) = 0L
          Teletype
            ( plain.keep(n),
              IArray.unsafeFromArray(arr),
              newHyperlinks,
              newInsertions,
              IArray.empty[Int] )
        else
          // Sparse: keep runs whose start is < n; trim the last kept run; trailing style = 0L.
          val k = boundaries.length
          // Binary search for the last run whose start is < n
          var lo = 0
          var hi = k
          while lo + 1 < hi do
            val mid = (lo + hi) >>> 1
            if boundaries(mid) < n then lo = mid else hi = mid
          val lastRun = lo
          val newK = lastRun + 1
          val newBoundariesArr = new Array[Int](newK)
          val newStylesArr = new Array[Long](newK + 1)
          var i = 0
          while i < newK do
            newBoundariesArr(i) = boundaries(i)
            newStylesArr(i) = styles(i)
            i += 1
          newStylesArr(newK) = 0L  // trailing reset
          Teletype
            ( plain.keep(n),
              IArray.unsafeFromArray(newStylesArr),
              newHyperlinks,
              newInsertions,
              IArray.unsafeFromArray(newBoundariesArr) )

  def render(termcap: Termcap): Text =
    if !termcap.ansi then plain else
      val buffer = StringBuilder()
      val depth = termcap.color
      val n = plain.length
      var prev: Long = 0L

      // Emit the chars in [from, to), interleaving any insertions that fall within.
      inline def emitText(from: Int, to: Int): Unit =
        if from < to then
          val ins = insertions.range(from, to)
          if ins.isEmpty then buffer.add(plain.s.substring(from, to).nn.tt)
          else
            var p = from
            ins.each: (k, v) =>
              if p < k then buffer.add(plain.s.substring(p, k).nn.tt)
              buffer.add(v)
              p = k
            if p < to then buffer.add(plain.s.substring(p, to).nn.tt)

      inline def emitRunStyle(s: Long, from: Int): Unit =
        if s != prev then StyleWord.emitDiff(buffer, prev, s, depth)
        if (s & StyleWord.HyperlinkChange) != 0 then
          hyperlinks.get(from) match
            case Some(url) => buffer.add(t"\e]8;;$url\e\\")
            case None      => buffer.add(t"\e]8;;\e\\")
        prev = s

      if isDense then
        // Dense: walk per char but coalesce consecutive equal styles into one emit.
        var i = 0
        while i < n do
          val s = styles(i)
          var j = i + 1
          while j < n && styles(j) == s do j += 1
          emitRunStyle(s, i)
          emitText(i, j)
          i = j
      else
        val k = boundaries.length
        var r = 0
        while r < k do
          val from = boundaries(r)
          val to = if r + 1 < k then boundaries(r + 1) else n
          val s = styles(r)
          emitRunStyle(s, from)
          emitText(from, to)
          r += 1

      val tail = trailingStyle
      if tail != prev then StyleWord.emitDiff(buffer, prev, tail, depth)
      if (tail & StyleWord.HyperlinkChange) != 0 then
        hyperlinks.get(n) match
          case Some(url) => buffer.add(t"\e]8;;$url\e\\")
          case None      => buffer.add(t"\e]8;;\e\\")
      insertions.rangeFrom(n).values.each { content => buffer.add(content) }

      buffer.text
