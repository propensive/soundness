package rudiments

import language.experimental.captureChecking

erased class Internet()

def internet[T](fn: (erased Internet) ?=> T): T =
  val inet: Internet = compiletime.erasedValue
  fn(using inet)
