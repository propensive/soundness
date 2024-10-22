package rudiments

import denominative.*
import anticipation.*

object Segmentable:
  given [ElementType] => IndexedSeq[ElementType] is Segmentable =
    (seq, interval) => seq.slice(interval.start.n0, interval.end.n1)

  given [ElementType] => IArray[ElementType] is Segmentable as iarray =
    (iarray, interval) => iarray.slice(interval.start.n0, interval.end.n1)

  given Text is Segmentable = (text, interval) =>
    text.s.substring(interval.start.n0, interval.end.n1).nn.tt

trait Segmentable:
  type Self
  def segment(entity: Self, interval: Interval): Self
