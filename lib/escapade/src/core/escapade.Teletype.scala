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
package escapade

import scala.language.experimental.pureFunctions

import scala.util.*

import anticipation.*
import denominative.*
import gossamer.*
import gossamer.collationOrdering
import gossamer.collations.codepoints
import hieroglyph.*
import mercator.*
import prepositional.*
import rudiments.*
import scala.collection.mutable as scm
import spectacular.*
import symbolism.*
import zephyrine.*
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
    type Result = Teletype
    type Operand = Teletype
    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)

  // Styled text occupies exactly the cells of its plain content: SGR styling and OSC
  // insertions are zero-width, so display width is the plain text's width under the
  // contextual metric.
  given measurable: (Text is Measurable) => Teletype is Measurable = _.plain.metrics

  // In `Teletype`'s companion (implicit scope for `Teletype is Reversible`), delegating to gossamer's
  // shared textual reversal so `teletype.reverse` resolves through the single `rudiments` `reverse`.
  given reversible: (Teletype is Reversible { type Result = Teletype }) = reversibleTextual

  // Likewise for traversal: this is what lets the generic predicate forms of `keep` and `skip`
  // (in `rudiments`) serve `Teletype`, boundary-by-traversal and rebuild-by-`segment`, so
  // styling survives.
  given traversable: (Teletype is Traversable { type Operand = Char }) = traversableTextual

  given textual: Teletype is Textual:
    type Result = Char
    type Show[value] = value is Teletypeable

    def classTag: ClassTag[Teletype] = summon[ClassTag[Teletype]]
    def size(text: Teletype): Int = text.plain.length
    def text(teletype: Teletype): Text = teletype.plain
    def length(text: Teletype): Int = text.plain.length
    def apply(text: Text): Teletype = Teletype(text)
    def single(operand: Char): Teletype = Teletype(operand.show)
    def fromChar(char: Char): Char = char

    def map(text: Teletype)(lambda: Char => Char): Teletype =
      val plain = text.plain
      val array = Array.scribe[Char](plain.length): scribe =>
        _ => plain.iterate { index => scribe.append(lambda(plain(index))) }

      Teletype
        ( new String(Array.unsafeJvm(array)).tt,
          text.styles,
          text.hyperlinks,
          text.insertions,
          text.boundaries )

    def segment(text: Teletype, interval: Interval): Teletype =
      text.dropChars(interval.start.n0).takeChars(interval.size)

    val empty: Teletype = Teletype.empty

    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)
    def access(text: Teletype, index: Ordinal): Char = text.plain.s.charAt(index.n0)

    def indexOf(text: Teletype, sub: Text, start: Ordinal): Optional[Ordinal] =
      text.plain.s.indexOf(sub.s, start.n0).puncture(-1).let(_.z)

    def show[value: Teletypeable](value: value) = value.teletype
    def builder(size: Optional[Int] = Unset): Teletype.Builder = Teletype.Builder(size)

  // Empty Teletype: dense form with no chars and one trailing entry.
  val empty: Teletype =
    new Teletype(t"", Array(0L), Map.empty, TreeMap.empty, Array.empty[Int])

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
    new Teletype(text, Array(0L, 0L), Map.empty, TreeMap.empty, Array(0))

  // Build a Teletype with a single uniform style applied to all chars.
  def styled[value: Showable](value: value)(transform: Ansi.Transform): Teletype =
    val text: Text = value.show
    val styled: Long = transform(TextStyle()).styleWord

    if text.length == 0 then
      new Teletype(t"", Array(styled, 0L), Map.empty, TreeMap.empty, Array(0))
    else
      new Teletype(text, Array(styled, 0L), Map.empty, TreeMap.empty, Array(0))

  // Compress a dense styles array into sparse form if it would benefit.
  // Returns the resulting (styles, boundaries) pair.
  def compressIfBeneficial(plain: Text, denseStyles: Array[Long]^{})
  :   (Array[Long]^{}, Array[Int]^{}) =

    val n = plain.length

    if n == 0
    then (Array(if denseStyles.length > 0 then denseStyles.readUnchecked(0) else 0L), Array.empty[Int])
    else
      // Count runs, tracking the previous style rather than reading `i - 1` again: the
      // confined scan visits each index once.
      var runs = 1
      var previous: Long = denseStyles.at(Prim).or(0L)

      denseStyles.iterate(denseStyles.extent.capped(n)): index =>
        val style = denseStyles.at(index)
        if style != previous then runs += 1
        previous = style

      if runs * SparseThreshold > n then
        // Keep dense
        (denseStyles, Array.empty[Int])
      else
        // Convert to sparse: sequential appends track the irregular write position.
        var newBoundaries: Array[Int]^{} = Array.empty[Int]

        val newStyles = Array.scribe[Long](runs + 1): styleScribe =>
          _ =>
            newBoundaries = Array.scribe[Int](runs): boundaryScribe =>
              _ =>
                boundaryScribe.append(0)
                previous = denseStyles.at(Prim).or(0L)
                styleScribe.append(previous)

                denseStyles.iterate(denseStyles.extent.capped(n)): index =>
                  val style = denseStyles.at(index)

                  if style != previous then
                    boundaryScribe.append((index: Ordinal).n0)
                    styleScribe.append(style)

                  previous = style

                styleScribe.append(denseStyles.at(Ordinal.zerary(n)).or(0L))  // trailing

        (newStyles, newBoundaries)

  // TeletypeBuilder → Teletype.Builder
  class Builder(size: Optional[Int] = Unset) extends gossamer.Builder[Teletype]:
    private val builder: StringBuilder = StringBuilder()
    private val styles: scm.ArrayBuffer[Long] = scm.ArrayBuffer.empty
    private val hyperlinks: scm.HashMap[Int, Text] = scm.HashMap()
    private val insertions: scm.TreeMap[Int, Text] = scm.TreeMap()

    @scala.caps.unsafe.untrackedCaptures
    private var offset: Int = 0

    def length: Int = builder.length

    protected def wipe(): Unit =
      offset = 0
      builder.clear()
      styles.clear()
      hyperlinks.clear()
      insertions.clear()

    protected def put(text: Teletype): Unit =
      builder.append(text.plain.s)
      var i = 0

      while i < text.plain.length do
        styles += text.styleAt(i)
        i += 1

      text.hyperlinks.each: (k, v) => hyperlinks(k + offset) = v
      text.insertions.each: (k, v) => insertions(k + offset) = v

      offset += text.plain.length

    protected def putChar(char: Char): Unit =
      builder.append(char)
      styles += 0L
      offset += 1

    protected def result(): Teletype =
      styles += 0L

      val plainText = builder.toString.tt
      val denseStyles = Array.unsafeFrozen(styles.toArray)
      val (newStyles, newBoundaries) = Teletype.compressIfBeneficial(plainText, denseStyles)

      Teletype
        ( plainText,
          newStyles,
          hyperlinks.toMap.to(Map),
          insertions.to(TreeMap),
          newBoundaries )


// `boundaries` is the run-start array for the sparse form; empty for the dense form.
// Dense:  styles.length == plain.length + 1; styles.at(i) is the style for char i (0 ≤ i < length)
//         and styles.at(length) is the trailing style.
// Sparse: styles.length == boundaries.length + 1; boundaries.at(i) is the start position of run i
//         (boundaries.at(0) == 0). Run i covers [boundaries.at(i), nextStart) where nextStart is
//         boundaries.at(i+1) or plain.length for the last run. styles.at(boundaries.length) is the
//         trailing style.
case class Teletype
  ( plain:      Text,
    styles:     Array[Long]^{},
    hyperlinks: Map[Int, Text]            = Map.empty,
    insertions: TreeMap[Int, Text]        = TreeMap.empty,
    boundaries: Array[Int]^{}               = Array.empty[Int] ):

  inline def isDense: Boolean = boundaries.length == 0

  // The run-lookup binary search shared by `styleAt`, `dropChars` and `takeChars`: the
  // greatest `lo` in [0, boundaries.length) whose boundary is `<= position` (`< position`
  // when `strict`). Each probe is bounds-checked: log₂ n checks on a cold path, and the
  // default is unreachable because `mid` always lies strictly between `lo` and `hi`.
  private def searchRun(position: Int, strict: Boolean): Int =
    var lo = 0
    var hi = boundaries.length

    while lo + 1 < hi do
      val mid = (lo + hi) >>> 1
      val boundary = boundaries.at(Ordinal.zerary(mid)).or(Int.MaxValue)
      if (if strict then boundary < position else boundary <= position) then lo = mid else hi = mid

    lo

  // Style at position p (0 ≤ p ≤ plain.length, where length means "trailing").
  def styleAt(p: Int): Long =
    if isDense then styles.readUnchecked(p)
    else if p >= plain.length then styles.readUnchecked(boundaries.length)
    else styles.readUnchecked(searchRun(p, strict = false))

  // Trailing style (the style after the last char; used for joins).
  inline def trailingStyle: Long =
    if isDense then styles.readUnchecked(plain.length) else styles.readUnchecked(boundaries.length)

  // Convert to sparse form's (styles, boundaries) representation.
  // For an already-sparse Teletype this is O(1); for a dense one it's O(plain.length).
  def asSparseArrays: (Array[Long]^{}, Array[Int]^{}) =
    if !isDense then (styles, boundaries)
    else if plain.length == 0 then (Array(if styles.length > 0 then styles.readUnchecked(0) else 0L), Array(0))
    else
      val n = plain.length
      // Count runs, tracking the previous style; see `compressIfBeneficial`.
      var runs = 1
      var previous: Long = styles.at(Prim).or(0L)

      styles.iterate(styles.extent.capped(n)): index =>
        val style = styles.at(index)
        if style != previous then runs += 1
        previous = style

      var newBoundaries: Array[Int]^{} = Array.empty[Int]

      val newStyles = Array.scribe[Long](runs + 1): styleScribe =>
        _ =>
          newBoundaries = Array.scribe[Int](runs): boundaryScribe =>
            _ =>
              boundaryScribe.append(0)
              previous = styles.at(Prim).or(0L)
              styleScribe.append(previous)

              styles.iterate(styles.extent.capped(n)): index =>
                val style = styles.at(index)

                if style != previous then
                  boundaryScribe.append((index: Ordinal).n0)
                  styleScribe.append(style)

                previous = style

              styleScribe.append(styles.at(Ordinal.zerary(n)).or(0L))

      (newStyles, newBoundaries)

  def explicit: Text = Text:
    render(termcapDefinitions.xtermTrueColorTermcap).s.flatMap: char =>
      if char.toInt == 27 then "\\e" else char.toString

  @targetName("add")
  def append(text: Text): Teletype =
    if text.length == 0 then this else
      val tail = trailingStyle
      val combinedPlain = t"$plain$text"
      if isDense then
        // Stay dense: extend styles array with the trailing style
        val newLength = plain.length + text.length + 1

        // For indexes within the old array this reads the old style (the old trailing style
        // at `plain.length` equals `tail`); beyond it, the default IS the extension.
        val arr = Array.scribe[Long](newLength): scribe =>
          _ => scribe.iterate { i => scribe(i) = styles.at(i).or(tail) }

        Teletype(combinedPlain, arr, hyperlinks, insertions, Array.empty[Int])
      else
        // Stay sparse: the new chars become part of the last run (since trailing style = last run
        // style) unless the last run's style differs from the trailing style — but that can't
        // happen because styles.at(boundaries.length-1) is the last run's style, and
        // styles.at(boundaries.length) is trailing.
        // They may differ. If so, we need a new run for the appended text.
        val k = boundaries.length
        val lastRunStyle = styles.readUnchecked(k - 1)
        if lastRunStyle == tail then
          // Just extend plain; runs unchanged
          Teletype(combinedPlain, styles, hyperlinks, insertions, boundaries)
        else
          // Add a new run starting at plain.length, with style = tail
          val newBoundaries = Array.allocate[Int](k + 1)
          val newStyles = Array.allocate[Long](k + 2)
          newBoundaries.copyFrom(boundaries, 0, 0, k)
          newBoundaries(k) = plain.length
          newStyles.copyFrom(styles, 0, 0, k)
          newStyles(k) = tail
          newStyles(k + 1) = tail

          Teletype
            ( combinedPlain,
              Array.freeze(newStyles),
              hyperlinks,
              insertions,
              Array.freeze(newBoundaries) )

  @targetName("add2")
  def append(that: Teletype): Teletype =
    if that.plain.length == 0 then this else if plain.length == 0 then that else
      val aN = plain.length
      val combinedPlain = plain+that.plain

      val shiftedLinks = if that.hyperlinks.nil then hyperlinks else
        val moved: Map[Int, Text] = that.hyperlinks.remap { (k, v) => (k + aN) -> v }
        hyperlinks + moved

      val shiftedInsertions = if that.insertions.isEmpty then insertions else
        insertions ++ that.insertions.map: (k, v) => (k + aN) -> v

      if isDense && that.isDense then
        // Both dense — direct array copy
        val newLength = aN + that.plain.length + 1
        val arr = Array.allocate[Long](newLength)
        arr.copyFrom(styles, 0, 0, aN)
        arr.copyFrom(that.styles, 0, aN, that.styles.length)

        Teletype
          ( combinedPlain,
            Array.freeze(arr),
            shiftedLinks,
            shiftedInsertions,
            Array.empty[Int] )
      else
        // At least one is sparse — combine in sparse form.
        val (aStyles, aBoundaries) = asSparseArrays
        val (bStyles, bBoundaries) = that.asSparseArrays
        val aK = aBoundaries.length
        val bK = bBoundaries.length
        val aLastStyle = aStyles.readUnchecked(aK - 1)
        val bFirstStyle = bStyles.readUnchecked(0)
        val merge = aLastStyle == bFirstStyle
        val newK = aK + bK - (if merge then 1 else 0)
        val newBoundariesArr = Array.allocate[Int](newK)
        val newStylesArr = Array.allocate[Long](newK + 1)
        // Copy A's runs
        newBoundariesArr.copyFrom(aBoundaries, 0, 0, aK)
        newStylesArr.copyFrom(aStyles, 0, 0, aK)
        // Copy B's runs (shifted by aN), optionally skipping the first if merging
        var bi = if merge then 1 else 0
        var ni = aK

        while bi < bK do
          newBoundariesArr(ni) = bBoundaries.readUnchecked(bi) + aN
          newStylesArr(ni) = bStyles.readUnchecked(bi)
          bi += 1
          ni += 1
        // Trailing style is B's trailing
        newStylesArr(newK) = bStyles.readUnchecked(bK)

        Teletype
          ( combinedPlain,
            Array.freeze(newStylesArr),
            shiftedLinks,
            shiftedInsertions,
            Array.freeze(newBoundariesArr) )

  def dropChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl => takeChars(plain.length - n)

    case Ltr =>
      val keepLength = plain.length - n

      if keepLength <= 0 then Teletype.empty
      else if n <= 0 then this
      else
        val newHyperlinks = hyperlinks.sweep { case (k, v) if k >= n => (k - n) -> v }

        val newInsertions =
          insertions.collect { case (k, v) if k >= n => (k - n) -> v }.to(TreeMap)

        if isDense then
          val arr = Array.scribe[Long](keepLength + 1): scribe =>
            _ => scribe.iterate { i => scribe(i) = styles.at(Ordinal.zerary(n + (i: Ordinal).n0)).or(0L) }

          Teletype
            ( plain.skip(n),
              arr,
              newHyperlinks,
              newInsertions,
              Array.empty[Int] )
        else
          // Sparse: find the run that contains position n; drop earlier runs;
          // adjust the first kept run's boundary to 0; shift all other boundaries by -n.
          val k = boundaries.length
          val firstRun = searchRun(n, strict = false)
          val newK = k - firstRun

          val newBoundariesArr = Array.scribe[Int](newK): scribe =>
            _ =>
              scribe.append(0)

              scribe.iterate: i =>
                if (i: Ordinal) != Prim then
                  scribe(i) = boundaries.at(Ordinal.zerary(firstRun + (i: Ordinal).n0)).or(0) - n

          val newStylesArr = Array.scribe[Long](newK + 1): scribe =>
            _ => scribe.iterate { i => scribe(i) = styles.at(Ordinal.zerary(firstRun + (i: Ordinal).n0)).or(0L) }

          Teletype
            ( plain.skip(n),
              newStylesArr,
              newHyperlinks,
              newInsertions,
              newBoundariesArr )

  def takeChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl => dropChars(plain.length - n)

    case Ltr =>
      if n <= 0 then Teletype.empty
      else if n >= plain.length then this
      else
        val newHyperlinks = hyperlinks.filter(_(0) < n)
        val newInsertions = insertions.rangeUntil(n)

        if isDense then
          val arr = Array.scribe[Long](n + 1): scribe =>
            _ => scribe.iterate { i => scribe(i) = if (i: Ordinal).n0 == n then 0L else styles.at(i).or(0L) }

          Teletype
            ( plain.keep(n),
              arr,
              newHyperlinks,
              newInsertions,
              Array.empty[Int] )
        else
          // Sparse: keep runs whose start is < n; trim the last kept run; trailing style = 0L.
          val lastRun = searchRun(n, strict = true)
          val newK = lastRun + 1

          val newBoundariesArr = Array.scribe[Int](newK): scribe =>
            _ => scribe.iterate { i => scribe(i) = boundaries.at(i).or(0) }

          val newStylesArr = Array.scribe[Long](newK + 1): scribe =>
            _ => scribe.iterate { i => scribe(i) = if (i: Ordinal).n0 == newK then 0L else styles.at(i).or(0L) }

          Teletype
            ( plain.keep(n),
              newStylesArr,
              newHyperlinks,
              newInsertions,
              newBoundariesArr )

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
            // An explicit iterator loop rather than `.each`: under capture checking the
            // per-element closure cannot flow into the surrounding builder (as in
            // rudiments' `weave`).
            val iterator = ins.iterator

            while iterator.hasNext do
              val (k, v) = iterator.next()
              if p < k then buffer.add(plain.s.substring(p, k).nn.tt)
              buffer.add(v)
              p = k

            if p < to then buffer.add(plain.s.substring(p, to).nn.tt)

      inline def emitRunStyle(s: Long, from: Int): Unit =
        if s != prev then StyleWord.emitDiff(buffer, prev, s, depth)

        if (s & StyleWord.HyperlinkChange) != 0 then
          hyperlinks.at(from).option match
            case Some(url) => buffer.add(t"\e]8;;$url\e\\")
            case None      => buffer.add(t"\e]8;;\e\\")

        prev = s

      if isDense then
        // Dense: walk per char but coalesce consecutive equal styles into one emit.
        var i = 0

        while i < n do
          val s = styles.readUnchecked(i)
          var j = i + 1
          while j < n && styles.readUnchecked(j) == s do j += 1
          emitRunStyle(s, i)
          emitText(i, j)
          i = j
      else
        val k = boundaries.length
        var r = 0

        while r < k do
          val from = boundaries.readUnchecked(r)
          val to = if r + 1 < k then boundaries.readUnchecked(r + 1) else n
          val s = styles.readUnchecked(r)
          emitRunStyle(s, from)
          emitText(from, to)
          r += 1

      val tail = trailingStyle
      if tail != prev then StyleWord.emitDiff(buffer, prev, tail, depth)

      if (tail & StyleWord.HyperlinkChange) != 0 then
        hyperlinks.at(n).option match
          case Some(url) => buffer.add(t"\e]8;;$url\e\\")
          case None      => buffer.add(t"\e]8;;\e\\")

      insertions.rangeFrom(n).values.each(buffer.add(_))

      buffer.text
