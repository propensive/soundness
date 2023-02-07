package chiaroscuro

import wisteria.*
import rudiments.*
import deviation.*
import gossamer.*
import dissonance.*
import escapade.*
import iridescence.*
import dendrology.*
import escritoire.*

enum Comparison:
  case Same(value: Text)
  case Different(left: Text, right: Text)
  case Structural(comparison: IArray[(Text, Comparison)], left: Text, right: Text)

extension [T](value: T)
  def compareTo(other: T)(using comparable: Comparable[T]): Comparison = comparable.compare(value, other)

object Comparison:
  given (using calc: TextWidthCalculator): AnsiShow[Comparison] =
    case Comparison.Structural(cmp, l, r) =>
      import tableStyles.horizontalGaps
      import treeStyles.default
      
      def children(comp: (Text, Comparison)): List[(Text, Comparison)] = comp(1) match
        case Same(value)                         => Nil
        case Different(left, right)              => Nil
        case Structural(comparison, left, right) => comparison.to(List)
      
      case class Row(treeLine: Text, left: AnsiText, right: AnsiText)

      def mkLine(tiles: List[TreeTile], data: (Text, Comparison)): Row =
        def line(bullet: Text) = t"${tiles.map(_.show).join}$bullet ${data(0)}"
        
        data(1) match
          case Same(v) =>
            Row(line(t"▪"), ansi"${rgb"#667799"}($v)", ansi"${rgb"#667799"}($v)")
          
          case Different(left, right) =>
            Row(line(t"▪"), ansi"${colors.YellowGreen}($left)", ansi"${colors.Crimson}($right)")
          
          case Structural(cmp, left, right) =>
            Row(line(t"■"), ansi"$left", ansi"$right")
      
      Table[Row](
        Column(t"")(_.treeLine),
        Column(t"Expected", align = Alignment.Right)(_.left),
        Column(t"Found")(_.right)
      ).tabulate(drawTree(children, mkLine)(cmp), maxWidth = 200).join(ansi"${'\n'}")
    
    case Different(left, right) => ansi"The value ${colors.YellowGreen}($left) did not equal ${colors.Crimson}($right)"
    case Same(value)            => ansi"The value ${colors.Gray}($value) was expected"

object Similar:
  given [T]: Similar[T] = (a, b) => a == b

trait Similar[-T]:
  def similar(a: T, b: T): Boolean

trait Comparable[-T]:
  def compare(a: T, b: T): Comparison

object Comparable extends Derivation[Comparable]:
  def nothing[T]: Comparable[T] = (a, b) => Comparison.Same(a.toString.show)
  
  def simplistic[T]: Comparable[T] = (a, b) =>
    if a == b then Comparison.Same(a.toString.show) else Comparison.Different(a.toString.show, b.toString.show)

  given [T: Canonical]: Comparable[T] = (left, right) =>
    if left.canon == right.canon
    then Comparison.Same(left.canon) else Comparison.Different(left.canon, right.canon)
  
  given Comparable[Text] = (left, right) =>
    if left == right then Comparison.Same(left.debug) else Comparison.Different(left.debug, right.debug)
  
  given Comparable[Int] = (left, right) =>
    if left == right then Comparison.Same(left.debug) else Comparison.Different(left.debug, right.debug)

  given Comparable[Exception] = (left, right) =>
    val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.show)
    val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.show)
    if left.getClass == right.getClass && leftMsg == rightMsg then Comparison.Same(leftMsg)
    else Comparison.Different(leftMsg, rightMsg)
  
  given seq[T: Debug: Comparable: Similar, Coll[X] <: Seq[X]]
           : Comparable[Coll[T]] = (left, right) =>
    val leftSeq = left.to(IndexedSeq)
    val rightSeq = right.to(IndexedSeq)

    if leftSeq == rightSeq then Comparison.Same(leftSeq.map(_.debug).join(t"[", t", ", t"]")) else
      val comparison = IArray.from:
        diff(leftSeq, rightSeq).rdiff2(summon[Similar[T]].similar).changes.map:
          case Change.Keep(lid, rid, v) =>
            (if lid == rid then lid.show else t"$lid/$rid") -> Comparison.Same(v.debug)
          
          case Change.Ins(rid, v) =>
            t" /$rid" -> Comparison.Different(t"—", v.debug)
          
          case Change.Del(lid, v) =>
            t"$lid/" -> Comparison.Different(v.debug, t"—")
          
          case Change.Replace(lid, rid, lv, rv) =>
            t"$lid/$rid" -> summon[Comparable[T]].compare(lv, rv)
      
      Comparison.Structural(comparison, left.toString.show, right.toString.show)
  
  given iarray[T: Debug: Comparable: Similar]: Comparable[IArray[T]] = (left, right) =>
    seq[T, IndexedSeq].compare(left.to(IndexedSeq), right.to(IndexedSeq))
  
  def join[T](caseClass: CaseClass[Comparable, T]): Comparable[T] = (left, right) =>
    val same = caseClass.params.forall: param =>
      param.deref(left) == param.deref(right)

    if same then Comparison.Same(left.toString.show)
    else
      val comparison = caseClass.params.map: param =>
        (Text(param.label), param.typeclass.compare(param.deref(left), param.deref(right)))
      Comparison.Structural(comparison, left.toString.show, right.toString.show)
  
  def split[T](sealedTrait: SealedTrait[Comparable, T]): Comparable[T] = (left, right) =>
    sealedTrait.choose(left): subtype =>
      sealedTrait.choose(right): subtype2 =>
        if subtype.subtype.index == subtype2.subtype.index
        then subtype.typeclass.compare(subtype.cast(left), subtype.cast(right))
        else Comparison.Different(left.toString.show, right.toString.show)