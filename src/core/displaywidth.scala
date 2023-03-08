package hieronymus

import gossamer.*
import rudiments.*
import kaleidoscope.*
import java.io as ji

import scala.collection.immutable.TreeMap

object HieronymusOpaques:
  opaque type CharRange = Long
  object CharRange:
    def apply(from: Int, to: Int): CharRange = (from.toLong << 32) + to.toLong
    def apply(char: Char): CharRange = (char.toLong << 32) + char.toInt
    def apply(char: Int): CharRange = (char.toLong << 32) + char

    given Show[CharRange] = range => t"${range.from}..${range.to}"

  given Ordering[CharRange] = Ordering.Long

  extension (range: CharRange)
    def from: Int = (range >> 32).toInt
    def to: Int = range.toInt
    def contains(char: Char): Boolean = char.toInt >= from && char.toInt <= to

object Unicode:
  import HieronymusOpaques.*
  
  object EaWidth:
    def unapply(code: Text): Option[EaWidth] = code.only:
      case t"N"  => Neutral
      case t"W"  => Wide
      case t"A"  => Ambiguous
      case t"H"  => HalfWidth
      case t"F"  => FullWidth
      case t"Na" => Narrow
  
  enum EaWidth:
    case Neutral, Narrow, Wide, Ambiguous, FullWidth, HalfWidth

    def width: Int = this match
      case Wide | FullWidth => 2
      case _                => 1

  object Hex:
    def unapply(text: Text): Option[Int] =
      try Some(Integer.parseInt(text.s, 16)) catch case err: NumberFormatException => None

  def eastAsianWidth(char: Char): Maybe[EaWidth] =
    eastAsianWidths.minAfter(CharRange(char.toInt, char.toInt)).maybe.mm(_(1))

  var count = 0

  lazy val eastAsianWidths: TreeMap[CharRange, EaWidth] =
    extension (map: TreeMap[CharRange, EaWidth])
      def append(range: CharRange, width: EaWidth): TreeMap[CharRange, EaWidth] =
        if map.isEmpty then map.updated(range, width)
        else if map.lastKey.to == (range.from - 1) && map(map.lastKey) == width
        then map.removed(map.lastKey).updated(CharRange(map.lastKey.from, range.to), width)
        else map.updated(range, width)

    @tailrec
    def recur(stream: LazyList[Text], map: TreeMap[CharRange, EaWidth]): TreeMap[CharRange, EaWidth] =
      stream match
        case r"${Hex(from)}@([0-9A-F]{4})\.\.${Hex(to)}@([0-9A-F]{4});${EaWidth(w)}@([AFHNW]a?).*" #:: tail =>
          recur(tail, map.append(CharRange(from, to), w))
        case r"${Hex(from)}@([0-9A-F]{4});${EaWidth(w)}@([AFHNW]a?).*" #:: tail =>
          recur(tail, map.append(CharRange(from, from), w))
        case head #:: tail =>
          recur(tail, map)
        case _ =>
          map
    
    val in: ji.InputStream = Option(getClass.getResourceAsStream("/hieronymus/EastAsianWidth.txt")).map(_.nn).getOrElse:
      throw Mistake("Could not find hieronymus/EastAsianWidth.txt on the classpath")
    
    val stream = scala.io.Source.fromInputStream(in).getLines.map(Text(_)).to(LazyList)
  
    recur(stream, TreeMap())

extension (char: Char)
  def displayWidth: Int = Unicode.eastAsianWidth(char).mm(_.width).or(1)

extension (text: Text)
  def displayWidth: Int = text.chars.map(_.displayWidth).sum
