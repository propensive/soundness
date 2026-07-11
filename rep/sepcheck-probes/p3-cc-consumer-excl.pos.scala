// P3 cascade positive: the same CC-only consumer using an exclusive reference must work —
// this is the mechanical `^` fix the parser-module sweep would apply.
//LIB: p3-kernel.lib.scala
import language.experimental.captureChecking

import sepprobe.Kernel

def consume(k: Kernel^): Int = { k.bump(); k.get }

def local(): Int =
  val k = Kernel()
  k.bump()
  k.get
