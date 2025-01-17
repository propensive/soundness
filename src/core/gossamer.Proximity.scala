package gossamer

import anticipation.*

trait Proximity:
  def distance(left: Text, right: Text): Double
