package rudiments

import language.experimental.captureChecking

erased sealed class Internet()

def internet[T](fn: (erased Internet) ?=> T): T =
  val inet: Internet = Internet()
  fn(using inet)
