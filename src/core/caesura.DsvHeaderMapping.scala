package caesura

import anticipation.*

trait DsvHeaderMapping:
  def transform(name: Text): Text
