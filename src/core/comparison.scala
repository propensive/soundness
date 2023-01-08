package probably2

import wisteria.*
import rudiments.*
import gossamer.*
import dissonance.*

enum Comparison:
  case Same
  case Different(left: Text, right: Text)
  case Structural(comparison: IArray[(Text, Comparison)])

trait Comparable[-T]:
  def compare(a: T, b: T): Comparison

object Comparable extends Derivation[Comparable]:
  given [T](using debug: Debug[T]): Comparable[T] = (left, right) =>
    if left == right then Comparison.Same else Comparison.Different(left.debug, right.debug)

  given [T: ClassTag](using cmp: Comparable[T], debug: Debug[T]): Comparable[Seq[T]] = (left, right) =>
    Comparison.Structural:
      IArray.from:
        Diff.diff(IArray.from(left), IArray.from(right)).changes.map:
          case Change.Keep(lid, rid, v) => (if lid == rid then lid.show else t"$lid/$rid") -> Comparison.Same
          case Change.Ins(rid, v)       => t"/$rid"     -> Comparison.Different(t"—", v.debug)
          case Change.Del(lid, v)       => t"$lid/"     -> Comparison.Different(v.debug, t"—")
      

  def join[T](caseClass: CaseClass[Comparable, T]): Comparable[T] = (left, right) =>
    val same = caseClass.params.forall: param =>
      param.deref(left) == param.deref(right)

    if same then Comparison.Same
    else Comparison.Structural:
      caseClass.params.map: param =>
        (Text(param.label), param.typeclass.compare(param.deref(left), param.deref(right)))
  
  def split[T](sealedTrait: SealedTrait[Comparable, T]): Comparable[T] = (left, right) =>
    sealedTrait.choose(left): subtype =>
      sealedTrait.choose(right): subtype2 =>
        if subtype.typeInfo == subtype2.typeInfo
        then subtype.typeclass.compare(subtype.cast(left), subtype.cast(right))
        else Comparison.Different(subtype.typeInfo.short.show, subtype2.typeInfo.short.show)
    