// P5 negative: freeze consumes the array — mutating the original afterwards must be
// rejected (use of a consumed capability). This is the safety half of the
// materialize-once-share-N-ways pattern (Manifold/Confluence).
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

def bad(): Int =
  val array = new Array[Byte](4)
  val frozen = caps.freeze(array)
  array(0) = 1
  frozen(0).toInt
