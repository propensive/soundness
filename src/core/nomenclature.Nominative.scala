package nomenclature

import anticipation.*

trait Nominative:
  type Self
  def validate(name: Text): Unit
