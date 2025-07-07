package chiaroscuro

import soundness.*

object Case2Real:
  def p1: Text = t"hello".decompose.text
  def p2: Decomposition = t"hello".decompose
  val p3: Text is Contrastable = Contrastable.nothing[Text]
  def p4(x: Text, y: Text): Juxtaposition = x.contrast(y)
